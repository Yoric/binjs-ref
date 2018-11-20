use entropy::probabilities::{ InstancesToProbabilities, SymbolIndex, SymbolInfo };
use io::content::Instances;

use binjs_shared::{ FieldName, InterfaceName };

use std;
use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::Hash;
use std::rc::Rc;

use itertools::Itertools;
use range_encoding;

pub type IOPath = binjs_shared::ast::Path<InterfaceName, (/* child index */ usize, /* field name */ FieldName)>;
pub type IOPathItem = binjs_shared::ast::PathItem<InterfaceName, (/* child index */ usize, /* field name */ FieldName)>;

/// A newtype for `usize` used to represent an index in a dictionary of values.
#[derive(Add, Constructor, Eq, PartialEq, Ord, PartialOrd, Clone, Copy, From, Into, Debug, Hash, Serialize, Deserialize)]
pub struct DictionaryIndex(usize);

/// A newtype for `usize` used to represent a reference to a value already encountered.
///
/// By convention, `0` is the latest value, `1` the value before, etc.
#[derive(Constructor, Eq, PartialEq, Ord, PartialOrd, Clone, Copy, Hash, Into, Debug, Serialize, Deserialize)]
pub struct BackReference(usize);


mod context_information {
    use super::Instances;
    use entropy::probabilities::{ SymbolIndex, SymbolInfo };

    use std::collections::HashMap;
    use std::hash::Hash;

    use itertools::Itertools;

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

    impl<NodeValue> ContextInformation<NodeValue, Instances> where NodeValue: Eq + Hash {
        pub fn instances_by_node_value(&self) -> &HashMap<NodeValue, Instances> {
            &self.stats_by_node_value
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
            self.value_by_symbol_index.get(Into::<usize>::into(index))
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

    impl<NodeValue> ::entropy::probabilities::InstancesToProbabilities for ContextInformation<NodeValue, Instances> where NodeValue: Clone + Eq + Hash + Ord {
        type AsProbabilities = ContextInformation<NodeValue, SymbolInfo>;
        fn instances_to_probabilities(self, _description: &str) -> ContextInformation<NodeValue, SymbolInfo> {
            let stats_by_node_value = self.stats_by_node_value.into_iter()
                .sorted_by(|(value_1, _), (value_2, _)| Ord::cmp(value_1, value_2)); // We need to ensure that the order remains stable across process restarts.

            let instances = stats_by_node_value.iter()
                .map(|(_, instances)| Into::<usize>::into(instances.clone()) as u32)
                .collect();

            let distribution = std::rc::Rc::new(std::cell::RefCell::new(range_encoding::CumulativeDistributionFrequency::new(instances)));

            let (stats_by_node_value, value_by_symbol_index): (HashMap<_, _>, Vec<_>) = stats_by_node_value
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

impl<Context, NodeValue> InstancesToProbabilities for ContextPredict<Context, NodeValue, Instances> where Context: Eq + Hash + Clone + std::fmt::Debug, NodeValue: Eq + Hash + Clone + Ord {
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
    /// The amount of context to use.
    ///
    /// With a depth of 0, paths are ignored. With a depth of 1, we only take into account
    /// the node/field. With a depth of 2, we also take into account the node/field of the
    /// grand parent, etc.
    depth: usize,

    /// Actual information stored.
    context_predict: ContextPredict<IOPath, NodeValue, Statistics>,
}


impl<NodeValue> InstancesToProbabilities for PathPredict<NodeValue, Instances> where NodeValue: Eq + Hash + Clone + Ord {
    type AsProbabilities = PathPredict<NodeValue, SymbolInfo>;
    fn instances_to_probabilities(self, description: &str) -> PathPredict<NodeValue, SymbolInfo> {
        PathPredict {
            depth: self.depth,
            context_predict: self.context_predict
                .instances_to_probabilities(description)
        }
    }
}

impl<NodeValue, Statistics> PathPredict<NodeValue, Statistics> where NodeValue: Eq + Hash + Clone {
    pub fn new(depth: usize,) -> Self {
        PathPredict {
            depth,
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

    fn tail<'a>(&self, path: &'a [IOPathItem]) -> &'a [IOPathItem] {
        let path = if path.len() <= self.depth {
            path
        } else {
            &path[path.len() - self.depth..]
        };
        path
    }
}
impl<NodeValue> PathPredict<NodeValue, Instances> where NodeValue: Eq + Hash + Clone {
    /// Register a value as being used at this path.
    pub fn add(&mut self, path: &[IOPathItem], value: NodeValue) {
        let tail = self.tail(path);
        let mut as_path = IOPath::new();
        as_path.extend_from_slice(tail);
        self.context_predict.add(as_path, value);
    }
}
impl<NodeValue> PathPredict<NodeValue, SymbolInfo> where NodeValue: Eq + Hash + Clone {
    /// Get a value by path and index.
    ///
    /// This method is only implemented when `Statistics=SymbolInfo` as the index is initialized
    /// by `instances_to_probabilities`.
    pub fn value_by_symbol_index(&mut self, path: &[IOPathItem], index: SymbolIndex) -> Option<&NodeValue> {
        let tail = self.tail(path);
        self.context_predict.value_by_symbol_index(tail, index)
    }


    pub fn stats_by_node_value(&mut self, path: &[IOPathItem], value: &NodeValue) -> Option<&SymbolInfo> {
        let tail = self.tail(path);
        self.context_predict.stats_by_node_value(tail, value)
    }

    pub fn stats_by_node_value_mut(&mut self, path: &[IOPathItem], value: &NodeValue) -> Option<&mut SymbolInfo> {
        let tail = self.tail(path);
        self.context_predict.stats_by_node_value_mut(tail, value)
    }


    /// Get frequency information for a given path.
    pub fn frequencies_at(&mut self, path: &[IOPathItem]) -> Option<&Rc<RefCell<range_encoding::CumulativeDistributionFrequency>>> {
        let tail = self.tail(path);
        let info =
            if let Some(info) = self.context_predict
                .by_context
                .get_mut(tail)
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




/// An index for a value in `WindowPredict`.
#[derive(Debug, PartialEq, Eq, Hash, Clone, Serialize, Deserialize, PartialOrd, Ord)]
pub enum WindowPrediction {
    /// A recently encountered value.
    ///
    /// `0` is the index of the latest value, `width - 1` is the index of the oldest
    /// value still in the window.
    ///
    /// Invariant: `0 <= BackReference < width`.
    BackReference(BackReference),

    /// An index into a global dictionary.
    DictionaryIndex(DictionaryIndex),
}

/// A prediction mechanism based on a sliding window.
///
/// Whenever encoding/decoding a value, if this value is one of the `width` latest
/// seen values, we use a backreference, otherwise, we use an index into a global
/// list of values. This strategy should work well when backreferences have a high
/// probability of happening.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct WindowPredict<NodeValue, Statistics> where NodeValue: Clone + Eq + Hash {
    /// The window width.
    width: usize,

    /// The `width` values seen most recently, indexed by `BackReference`. Note that
    /// this vector starts empty and may contain fewer than `width` values. A value
    /// may never appear twice in this vector.
    latest_values: Vec<NodeValue>,

    /// A dictionary of all values encountered so far.
    /// Whenever we encounter a value that had previously not been encountered,
    /// we append it at the end. Values never move.
    value_by_dictionary_index: Vec<NodeValue>,

    /// A mapping from NodeValue to the `DictionaryIndex` used to represent them
    /// in `value_by_dictionary_index`. Mapping for a given value never changes.
    dictionary_index_by_value: HashMap<NodeValue, DictionaryIndex>,

    /// Actual statistics on values.
    info: ContextInformation<WindowPrediction, Statistics>,
}
impl<NodeValue, Statistics> WindowPredict<NodeValue, Statistics> where NodeValue: Clone + Eq + Hash {
    pub fn new(width: usize) -> Self {
        WindowPredict {
            width,
            value_by_dictionary_index: Vec::with_capacity(1024), // FIXME: Magic number
            dictionary_index_by_value: HashMap::with_capacity(1024),
            latest_values: Vec::with_capacity(width),
            info: ContextInformation::new(),
        }
    }

    pub fn get_dictionary_index(&self, value: &NodeValue) -> Option<usize> {
        self.dictionary_index_by_value.get(value)
            .map(|x| x.0)
    }

    /// Update current window by moving the value at index `index` to the latest-seen
    /// position (index 0).
    ///
    /// Fails if the index is not in [0, self.latest_values.len()[
    fn window_move_to_front(&mut self, index: BackReference) -> Result<(), ()> {
        let as_usize = Into::<usize>::into(index);
        if as_usize == 0 {
            // Special case in which `rotate_right` doesn't work.
            return Ok(())
        }
        if as_usize >= self.latest_values.len() {
            return Err(());
        }
        let ref mut slice = self.latest_values
            .as_mut_slice()
            [0..as_usize];
        slice.rotate_right(1);
        Ok(())
    }

    /// Update current window by putting a value to the latest-seen position (index 0).
    ///
    /// If the value is already in the window, it is moved to the latest-seen position,
    /// otherwise, it is added directly in latest-seen position. If the resulting window
    /// is too large, it is truncated.
    ///
    /// Return the previous position of the value in the window, if any.
    fn window_insert_value(&mut self, value: &NodeValue) -> Option<BackReference> {
        // It's possible that the value was present in the window.
        if let Some(index) = self.latest_values
            .iter()
            .position(|v| v == value)
        {
            let index = BackReference(index);
            self.window_move_to_front(index)
                .unwrap(); // We have just checked that `index < self.latest_values.len()`.
            return Some(index)
        }
        if self.latest_values.len() < self.width {
            // If the window isn't full yet, simply add the value at start.
            self.latest_values.insert(0, value.clone());
        } else {
            // Otherwise, push front and remove last.
            let slice = self.latest_values
                .as_mut_slice();
            slice[slice.len() - 1] = value.clone();
            slice.rotate_right(1);
        }
        None
    }
}
impl<NodeValue> WindowPredict<NodeValue, Instances> where NodeValue: Clone + Eq + std::hash::Hash + std::fmt::Debug {
    pub fn add(&mut self, value: NodeValue) {
        // --- At this stage, we don't know whether the value is known.

        debug!(target: "predict", "WindowPredict: Inserting value {:?}", value);
        let number_of_values = self.value_by_dictionary_index.len();
        let dictionary_index = *self.dictionary_index_by_value.entry(value.clone())
            .or_insert(DictionaryIndex(number_of_values));

        if dictionary_index == DictionaryIndex(number_of_values) {
            // We've just inserted `value`.
            self.value_by_dictionary_index.push(value.clone());
        } else {
            // Value was already known.
            let index : usize = dictionary_index.into();
            debug_assert_eq!(value, self.value_by_dictionary_index[index]);
        };

        // --- We are now sure that the value is known.

        // Update window.
        let symbol = match self.window_insert_value(&value) {
            Some(backref) => {
                // If the value was already in the window, we'll
                // favor this window, as this should give us a tighter
                // set of common symbols.
                WindowPrediction::BackReference(backref)
            }
            None => {
                // Otherwise, fallback to the global dictionary.
                WindowPrediction::DictionaryIndex(dictionary_index)
            }
        };
        debug!(target: "predict_window", "Increasing probability of {:?}", symbol);
        self.info.add(symbol);
    }
}

impl<NodeValue> WindowPredict<NodeValue, SymbolInfo> where NodeValue: Clone + Eq + std::hash::Hash + std::fmt::Debug {
    pub fn frequencies(&mut self) -> Option<&Rc<RefCell<range_encoding::CumulativeDistributionFrequency>>> {
        self.info
            .stats_by_node_value_mut()
            .values_mut()
            .next()
            .map(|any| &any.distribution)
    }

    pub fn decode_symbol_index(&self, index: SymbolIndex) -> Option<&WindowPrediction> {
        self.info.value_by_symbol_index(index)
    }

    // FIXME: We should find a way to enforce a specific mapping between `index` and `WindowPredict`,
    // to make it easy to decode.
    pub fn value_by_symbol_index(&mut self, index: SymbolIndex) -> Option<NodeValue> {
        match self.info.value_by_symbol_index(index) {
            None => None,
            Some(&WindowPrediction::DictionaryIndex(dictionary_index)) => {
                // Global entry.
                let result = self.value_by_dictionary_index
                    .get(dictionary_index.0)?
                    .clone();
                self.window_insert_value(&result);
                Some(result)
            }
            Some(&WindowPrediction::BackReference(index)) => {
                let as_usize: usize = index.into();
                let result = self.latest_values
                    .get(as_usize)?
                    .clone();
                if let Err(_) = self.window_move_to_front(index) {
                    return None;
                }
                Some(result)
            }
        }
    }

    pub fn stats_by_node_value_mut(&mut self, value: &NodeValue) -> Option<(WindowPrediction, &mut SymbolInfo)> {
        // At this stage, the value may appear in both the dictionary
        // and the window. We'll favor the window if possible.
        debug!(target: "predict", "WindowPredict: Fetching {:?}", value);
        let prediction =
            match self.window_insert_value(value) {
                Some(backref) => WindowPrediction::BackReference(backref),
                None => {
                    debug!(target: "predict", "WindowPredict: Value {:?} is not in the window, let's look for it in the dictionary", value);
                    let index = self.dictionary_index_by_value
                        .get(value)?
                        .clone();
                    WindowPrediction::DictionaryIndex(index)
                }
            };

        debug!(target: "predict", "WindowPredict: {:?} has just been inserted and will be encoded as {:?}", value, prediction);
        self.info
            .stats_by_node_value_mut()
            .get_mut(&prediction)
            .map(|symbol| (prediction, symbol))
    }
}

impl<NodeValue> InstancesToProbabilities for WindowPredict<NodeValue, Instances> where NodeValue: Clone + Eq + Hash + Ord {
    type AsProbabilities = WindowPredict<NodeValue, SymbolInfo>;
    fn instances_to_probabilities(self, _description: &str) -> WindowPredict<NodeValue, SymbolInfo> {
        for i in 0..self.width {
            debug!(target: "predict_window", "{} Instances of backref {}: {}",
                _description,
                i,
                match self.info.instances_by_node_value().get(&WindowPrediction::BackReference(BackReference::new(i))) {
                    None => 0,
                    Some(instances) => Into::<usize>::into(*instances),
                }
            );
        }
        let info = self.info.instances_to_probabilities("WindowPredict::info");

        for i in 0..self.width {
            debug!(target: "predict_window", "{} Stats of backref {}: {}",
                _description,
                i,
                match info.stats_by_node_value().get(&WindowPrediction::BackReference(BackReference::new(i))) {
                    None => "0".to_string(),
                    Some(symbol) => {
                        let mut borrow = symbol.distribution.borrow_mut();
                        let width = { borrow.width() };
                        let data = borrow.at_index(symbol.index.into()).unwrap();
                        format!("{}/{} = {:.2}%",
                            data.width(),
                            width,
                            100. * (data.width() as f64) / (width as f64))
                    }
                }
            );
        }
        WindowPredict {
            width: self.width,
            value_by_dictionary_index: self.value_by_dictionary_index,
            dictionary_index_by_value: self.dictionary_index_by_value,
            latest_values: Vec::with_capacity(self.width),
            info,
        }
    }
}
