use entropy::probabilities::{ InstancesToProbabilities, SymbolIndex, SymbolInfo };

use binjs_shared::{ FieldName, InterfaceName };

use std;
use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::Hash;
use std::rc::Rc;

use range_encoding;

pub type IOPath = binjs_shared::ast::Path<InterfaceName, (/* child index */ usize, /* field name */ FieldName)>;
pub type IOPathItem = binjs_shared::ast::PathItem<InterfaceName, (/* child index */ usize, /* field name */ FieldName)>;

/// A newtype for `usize` used to count the number of instances of some item.
#[derive(Default, Serialize, Deserialize, From, Into, AddAssign, Clone, Copy)]
pub struct Instances(usize);

mod context_information {
    use super::Instances;
    use entropy::probabilities::{ SymbolIndex, SymbolInfo };
    use std::collections::HashMap;
    use std::hash::Hash;

    /// A container for the statistics available in a given prediction context
    /// (a typical prediction context is a path in the AST, or a position in
    /// the file, etc.).
    ///
    /// This container is meant to be used in two settings:
    ///
    /// - to count the number of instances of values in a context (instantiated with `Statistics=Instances`); or
    /// - once number of instances have been converted to frequency information and unique indices
    ///     (instantiated with `Statistics=SymbolInfo`, in which case the `SymbolInfo` contains the unique index).
    ///
    /// For this reason, all the meaningful methods of this struct are implemented only if `Statistics=Instances`
    /// or `Statistics=SymbolInfo`.
    #[derive(Clone, Debug, Deserialize, Serialize)]
    pub struct ContextInformation<NodeValue, Statistics> where NodeValue: Eq + Hash {
        /// NodeValue => Statistics mapping, always valid
        stats_by_node_value: HashMap<NodeValue, Statistics>,

        /// SymbolIndex => NodeValue mapping.
        ///
        /// This vector is populated only when `Statistics = SymbolInfo`. When that is the case,
        /// `value_by_symbol_index` is effectively the reverse mapping from `stats_by_node_value` (using the
        /// index embedded in `SymbolInfo`).
        value_by_symbol_index: Vec<NodeValue>,
    }
    impl<NodeValue, Statistics> ContextInformation<NodeValue, Statistics> where NodeValue: Eq + Hash {
        pub fn new() -> Self {
            ContextInformation {
                stats_by_node_value: HashMap::new(),
                value_by_symbol_index: Vec::new(),
            }
        }

        /// Return the number of entries.
        pub fn len(&self) -> usize {
            self.stats_by_node_value.len()
        }
    }

    // Methods that make sense only when we have finished computing frequency information.
    impl<NodeValue> ContextInformation<NodeValue, SymbolInfo> where NodeValue: Eq + Hash {
        pub fn stats_by_node_value(&self) -> &HashMap<NodeValue, SymbolInfo> {
            &self.stats_by_node_value
        }

        pub fn stats_by_node_value_mut(&mut self) -> &mut HashMap<NodeValue, SymbolInfo> {
            &mut self.stats_by_node_value
        }

        pub fn value_by_symbol_index(&self, index: SymbolIndex) -> Option<&NodeValue> {
            let index: usize = index.into();
            self.value_by_symbol_index.get(index)
        }
    }

    // Methods that make sense only while we are collecting instances.

    impl<NodeValue> ContextInformation<NodeValue, Instances> where NodeValue: Eq + Hash {
        /// Register a value as being used in this context.
        pub fn add(&mut self, node_value: NodeValue) {
            self.stats_by_node_value.entry(node_value)
            .and_modify(|instances| *instances += 1.into())
            .or_insert(1.into());
        }
    }

    impl<NodeValue> ::entropy::probabilities::InstancesToProbabilities for ContextInformation<NodeValue, Instances> where NodeValue: Clone + Eq + Hash {
        type AsProbabilities = ContextInformation<NodeValue, SymbolInfo>;
        fn instances_to_probabilities(self, _description: &str) -> ContextInformation<NodeValue, SymbolInfo> {
            let instances: Vec<_> = self
                .stats_by_node_value
                .values()
                .map(|x| {
                    let x: usize = x.clone().into();
                    x as u32
                })
                .collect();

            let distribution = std::rc::Rc::new(std::cell::RefCell::new(range_encoding::CumulativeDistributionFrequency::new(instances).
                unwrap()));
            // FIXME: This will fail if `stats_by_node_value` is empty.
            // FIXME: We should have a fallback distribution in case everything is empty.

            let (stats_by_node_value, value_by_symbol_index): (HashMap<_, _>, Vec<_>) = self.stats_by_node_value
                .into_iter()
                .enumerate()
                .map(|(index, (value, _))| {
                    let for_stats_by_node_value = (value.clone(), SymbolInfo {
                        index: index.into(),
                        distribution: distribution.clone(),
                    });
                    let for_value_by_symbol_index = value;
                    (for_stats_by_node_value, for_value_by_symbol_index)
                })
                .unzip();
            ContextInformation {
                stats_by_node_value,
                value_by_symbol_index
            }
        }
    }
}
use self::context_information::ContextInformation;

/// A generic mechanism used to predict possible values in a given context (e.g.
/// AST path or file position) in a file.
///
/// This mechanism is meant to be used as follows:
///
/// 1. Create a new `ContextPredict<Context, NodeValue, Instances>` and use `ContextPredict::add` to count the number
///     of instances of each possible `NodeValue` in each possible `Context`. Alternatively, load this data
///     from an existing dictionary.
/// 2. Convert the `ContextPredict<Context, NodeValue, Instances>` into a `ContextPredict<Context, NodeValue, SymbolInfo>`
///     by calling `ContextPredict::instances_to_probabilities`.
/// 3. Use method `ContextPredict::<_, _, SymbolInfo>::stats_by_node_value` and `stats_by_node_value_mut` to get the statistics
///     information in a specific context for a specific node value (used for compression). This information contains
///     an index designed to be written to a compressed stream.
/// 4. Use method `ContextPredict::<_, _, SymbolInfo>::frequencies_at` to get the statistics information in a
///     specific context for all node values that have shown up in this context (used for decompression). This
///     information is used to extract an index from a compressed stream.
/// 5. Use method `ContextPredict::<_, _, SymbolInfo>::index_at` to get a specific node value in a specific
///     context from the index extracted from the compressed stream.
///
/// As most methods of this struct can only be used if `Statistics = Instances` xor `Statistics = SymbolInfo`,
/// the implementation of these methods is only available respectively if `Statistics = Instances` or if
/// `Statistics = SymbolInfo`.
///
/// For most use cases, you probably want one of the more specialized predictors.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct ContextPredict<Context, NodeValue, Statistics> where Context: Eq + Hash + Clone, NodeValue: Eq + Hash + Clone {
    by_context: HashMap<Context, ContextInformation<NodeValue, Statistics>>,
}
impl<Context, NodeValue, Statistics> ContextPredict<Context, NodeValue, Statistics> where Context: Eq + Hash + Clone, NodeValue: Eq + Hash + Clone {
    pub fn new() -> Self {
        Self {
            by_context: HashMap::new()
        }
    }

    /// All the contexts known to this predictor.
    ///
    /// Used mainly for debugging.
    pub fn contexts(&self) -> impl Iterator<Item=&Context> {
        self.by_context.keys()
    }

    /// The number of states in this predictor.
    pub fn len(&self) -> usize {
        self.by_context.values()
            .map(ContextInformation::len)
            .sum()
    }
}

impl<Context, NodeValue> ContextPredict<Context, NodeValue, Instances> where Context: Eq + Hash + Clone, NodeValue: Eq + Hash + Clone {
    /// Register a value as being used in this context.
    pub fn add(&mut self, context: Context, value: NodeValue) {
        let stats_by_node_value = self.by_context.entry(context)
            .or_insert_with(|| ContextInformation::new());
        stats_by_node_value.add(value)
    }
}

impl<Context, NodeValue> ContextPredict<Context, NodeValue, SymbolInfo> where Context: Eq + Hash + Clone, NodeValue: Eq + Hash + Clone {
    /// Get a value by context and index.
    ///
    /// This method is only implemented when `Statistics=SymbolInfo` as the index is initialized
    /// by `instances_to_probabilities`. The index corresponds to the one defined in
    /// the `SymbolInfo`.
    pub fn value_by_symbol_index<C2: ?Sized>(&mut self, context: &C2, index: SymbolIndex) -> Option<&NodeValue>
        where
            Context: std::borrow::Borrow<C2>,
            C2: Hash + Eq
    {
        self.by_context.get(context)?
            .value_by_symbol_index(index)
    }

    pub fn stats_by_node_value<C2: ?Sized>(&self, context: &C2, value: &NodeValue) -> Option<&SymbolInfo>
        where
            Context: std::borrow::Borrow<C2>,
            C2: Hash + Eq
    {
        self.by_context.get(context)?
            .stats_by_node_value()
            .get(value)
    }

    pub fn stats_by_node_value_mut<C2: ?Sized>(&mut self, context: &C2, value: &NodeValue) -> Option<&mut SymbolInfo>
        where
            Context: std::borrow::Borrow<C2>,
            C2: Hash + Eq
    {
        self.by_context.get_mut(context)?
            .stats_by_node_value_mut()
            .get_mut(value)
    }
}

impl<Context, NodeValue> InstancesToProbabilities for ContextPredict<Context, NodeValue, Instances> where Context: Eq + Hash + Clone + std::fmt::Debug, NodeValue: Eq + Hash + Clone {
    type AsProbabilities = ContextPredict<Context, NodeValue, SymbolInfo>;
    fn instances_to_probabilities(self, description: &str) -> ContextPredict<Context, NodeValue, SymbolInfo> {
        debug!(target: "entropy", "Converting ContextPredict {} to probabilities", description);
        let by_context = self.by_context.into_iter()
            .map(|(context, info)| (context, info.instances_to_probabilities("ContextInformation")))
            .collect();
        ContextPredict {
            by_context,
        }
    }
}

/// A specialized predictor used to predict possible values at a possible path in the AST.
///
/// This mechanism is meant to be used as follows:
///
/// 1. Create a new `PathPredict<NodeValue, Instances>` and use `PathPredict::add` to count the number
///     of instances of each possible `NodeValue` in each possible path. Alternatively, load this data
///     from an existing dictionary.
/// 2. Convert the `PathPredict<NodeValue, Instances>` into a `PathPredict<NodeValue, SymbolInfo>`
///     by calling `PathPredict::<_, Instances>::instances_to_probabilities`.
/// 3. Use method `PathPredict::<_, SymbolInfo>::stats_by_node_value` and `stats_by_node_value_mut` to get the statistics
///     information in a specific path for a specific node value (used for compression). This information contains
///     an index designed to be written to a compressed stream.
/// 4. Use method `PathPredict::<_, SymbolInfo>::frequencies_at` to get the statistics information in a
///     specific path for all node values that have shown up in this path (used for decompression). This
///     information is used to extract an index from a compressed stream.
/// 5. Use method `PathPredict::<_, SymbolInfo>::index_at` to get a specific node value in a specific
///     path from the index extracted from the compressed stream.
///
/// As most methods of this struct can only be used if `Statistics = Instances` xor `Statistics = SymbolInfo`,
/// the implementation of these methods is only available respectively if `Statistics = Instances` or if
/// `Statistics = SymbolInfo`.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct PathPredict<NodeValue, Statistics> where NodeValue: Eq + Hash + Clone {
    context_predict: ContextPredict<IOPath, NodeValue, Statistics>,
}


impl<NodeValue> InstancesToProbabilities for PathPredict<NodeValue, Instances> where NodeValue: Eq + Hash + Clone {
    type AsProbabilities = PathPredict<NodeValue, SymbolInfo>;
    fn instances_to_probabilities(self, description: &str) -> PathPredict<NodeValue, SymbolInfo> {
        PathPredict {
            context_predict: self.context_predict
                .instances_to_probabilities(description)
        }
    }
}

impl<NodeValue, Statistics> PathPredict<NodeValue, Statistics> where NodeValue: Eq + Hash + Clone + std::fmt::Debug {
    pub fn new() -> Self {
        PathPredict {
            context_predict: ContextPredict::new(),
        }
    }

    /// The number of states in this predictor.
    pub fn len(&self) -> usize {
        self.context_predict.len()
    }

    /// All the paths known to this predictor.
    ///
    /// Used mainly for debugging.
    pub fn paths(&self) -> impl Iterator<Item=&IOPath> {
        self.context_predict.contexts()
    }
}
impl<NodeValue> PathPredict<NodeValue, Instances> where NodeValue: Eq + Hash + Clone {
        /// Register a value as being used at this path.
    pub fn add(&mut self, path: &[IOPathItem], value: NodeValue) {
        let mut as_path = IOPath::new();
        as_path.extend_from_slice(path);
        self.context_predict.add(as_path, value);
    }
}
impl<NodeValue> PathPredict<NodeValue, SymbolInfo> where NodeValue: Eq + Hash + Clone {
    /// Get a value by path and index.
    ///
    /// This method is only implemented when `Statistics=SymbolInfo` as the index is initialized
    /// by `instances_to_probabilities`.
    pub fn value_by_symbol_index(&mut self, tail: &[IOPathItem], index: SymbolIndex) -> Option<&NodeValue> {
        self.context_predict.value_by_symbol_index(tail, index)
    }


    pub fn stats_by_node_value(&mut self, tail: &[IOPathItem], value: &NodeValue) -> Option<&SymbolInfo> {
        self.context_predict.stats_by_node_value(tail, value)
    }

    pub fn stats_by_node_value_mut(&mut self, tail: &[IOPathItem], value: &NodeValue) -> Option<&mut SymbolInfo> {
        self.context_predict.stats_by_node_value_mut(tail, value)
    }


    /// Get frequency information for a given path.
    pub fn frequencies_at(&mut self, path: &[IOPathItem]) -> Option<&Rc<RefCell<range_encoding::CumulativeDistributionFrequency>>> {
        let info =
            if let Some(info) = self.context_predict
                .by_context
                .get_mut(path)
            {
                info
            } else {
                return None;
            };
        info
            .stats_by_node_value_mut()
            .values_mut()
            .next()
            .map(|any| &any.distribution)
    }
}

