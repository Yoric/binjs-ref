//! Entropy prediction tables.

use entropy::probabilities::{InstancesToProbabilities, SymbolIndex, SymbolInfo};
pub use io::statistics::Instances;

use binjs_shared::{IOPath, IOPathItem};

use std;
use std::borrow::Borrow;
use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::Hash;
use std::rc::Rc;

#[allow(unused_imports)] // We keep enabling/disabling this.
use itertools::Itertools;
use range_encoding;

/// A newtype for `usize` used to represent an index in a dictionary of values.
#[derive(
    Add,
    Constructor,
    Eq,
    PartialEq,
    Ord,
    PartialOrd,
    Clone,
    Copy,
    From,
    Into,
    Debug,
    Hash,
    Serialize,
    Deserialize,
)]
struct DictionaryIndex(usize);

/// A newtype for `usize` used to represent a reference to a value already encountered.
///
/// By convention, `0` is the latest value, `1` the value before, etc.
#[derive(
    Constructor,
    Eq,
    PartialEq,
    Ord,
    PartialOrd,
    Clone,
    Copy,
    Hash,
    Into,
    Debug,
    Serialize,
    Deserialize,
)]
struct BackReference(usize);

#[derive(Clone, Debug)]
pub struct AddResult {
    pub new_context: bool,
    pub new_value: bool,
}

mod context_information {
    use super::Instances;
    use entropy::probabilities::{SymbolIndex, SymbolInfo};

    use std::cell::RefCell;
    use std::collections::HashMap;
    use std::hash::Hash;
    use std::rc::Rc;

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
    pub struct ContextInformation<NodeValue, Statistics>
    where
        NodeValue: Eq + Hash,
    {
        /// NodeValue => Statistics mapping, always valid
        stats_by_node_value: HashMap<NodeValue, Statistics>,

        /// SymbolIndex => NodeValue mapping.
        ///
        /// This vector is populated only when `Statistics = SymbolInfo`. When that is the case,
        /// `value_by_symbol_index` is effectively the reverse mapping from `stats_by_node_value` (using the
        /// index embedded in `SymbolInfo`).
        value_by_symbol_index: Vec<NodeValue>,
    }
    impl<NodeValue, Statistics> ContextInformation<NodeValue, Statistics>
    where
        NodeValue: Eq + Hash,
    {
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

        pub fn into_iter(self) -> impl Iterator<Item = (NodeValue, Statistics)> {
            self.stats_by_node_value.into_iter()
        }

        pub fn iter(&self) -> impl Iterator<Item = (&NodeValue, &Statistics)> {
            self.stats_by_node_value.iter()
        }
    }

    // Methods that make sense only when we have finished computing frequency information.
    impl<NodeValue> ContextInformation<NodeValue, SymbolInfo>
    where
        NodeValue: Eq + Hash,
    {
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

    impl<NodeValue> ContextInformation<NodeValue, Instances>
    where
        NodeValue: Eq + Hash,
    {
        /// Register a value as being used in this context.
        ///
        /// Return the number of instances.
        pub fn add(&mut self, node_value: NodeValue) -> Instances {
            *self
                .stats_by_node_value
                .entry(node_value)
                .and_modify(|instances| *instances += 1.into())
                .or_insert(1.into())
        }

        /// Register a value as being used in this context.
        ///
        /// If the value is already used, do not increase the number of instances.
        pub fn add_if_absent(&mut self, node_value: NodeValue) {
            self.stats_by_node_value
                .entry(node_value)
                .or_insert(1.into());
        }
    }

    impl<NodeValue> ::entropy::probabilities::InstancesToProbabilities
        for ContextInformation<NodeValue, Instances>
    where
        NodeValue: Clone + Eq + Hash + Ord,
    {
        type AsProbabilities = ContextInformation<NodeValue, SymbolInfo>;
        fn instances_to_probabilities(
            &self,
            _description: &str,
        ) -> ContextInformation<NodeValue, SymbolInfo> {
            let stats_by_node_value = self
                .stats_by_node_value
                .iter()
                .sorted_by(|(value_1, _), (value_2, _)| Ord::cmp(value_1, value_2)); // We need to ensure that the order remains stable across process restarts.

            let instances = stats_by_node_value
                .iter()
                .map(|(_, instances)| Into::<usize>::into(**instances) as u32)
                .collect();

            let distribution = std::rc::Rc::new(std::cell::RefCell::new(
                range_encoding::CumulativeDistributionFrequency::new(instances),
            ));

            let (stats_by_node_value, value_by_symbol_index): (HashMap<_, _>, Vec<_>) =
                stats_by_node_value
                    .into_iter()
                    .enumerate()
                    .map(|(index, (value, _))| {
                        let for_stats_by_node_value = (
                            value.clone(),
                            SymbolInfo {
                                index: index.into(),
                                distribution: distribution.clone(),
                            },
                        );
                        let for_value_by_symbol_index = value.clone();
                        (for_stats_by_node_value, for_value_by_symbol_index)
                    })
                    .unzip();
            ContextInformation {
                stats_by_node_value,
                value_by_symbol_index,
            }
        }
    }

    impl<NodeValue> ContextInformation<NodeValue, SymbolInfo>
    where
        NodeValue: Clone + Eq + Hash,
    {
        pub fn frequencies(
            &self,
        ) -> Option<&Rc<RefCell<range_encoding::CumulativeDistributionFrequency>>> {
            self.stats_by_node_value()
                .values()
                .next()
                .map(|any| &any.distribution)
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
///
/// As most methods of this struct can only be used if `Statistics = Instances` xor `Statistics = SymbolInfo`,
/// the implementation of these methods is only available respectively if `Statistics = Instances` or if
/// `Statistics = SymbolInfo`.
///
/// For most use cases, you probably want one of the more specialized predictors.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct ContextPredict<Context, NodeValue, Statistics>
where
    Context: Eq + Hash + Clone,
    NodeValue: Eq + Hash + Clone,
{
    by_context: HashMap<Context, ContextInformation<NodeValue, Statistics>>,
}
impl<Context, NodeValue, Statistics> ContextPredict<Context, NodeValue, Statistics>
where
    Context: Eq + Hash + Clone,
    NodeValue: Eq + Hash + Clone,
{
    pub fn new() -> Self {
        Self {
            by_context: HashMap::new(),
        }
    }

    /// All the contexts known to this predictor.
    ///
    /// Used mainly for debugging.
    pub fn contexts(&self) -> impl Iterator<Item = &Context> {
        self.by_context.keys()
    }

    /// Iterate through this predictor.
    pub fn iter_mut(
        &mut self,
    ) -> impl Iterator<Item = (&Context, &mut ContextInformation<NodeValue, Statistics>)> {
        self.by_context.iter_mut()
    }

    pub fn iter(
        &self,
    ) -> impl Iterator<Item = (&Context, &ContextInformation<NodeValue, Statistics>)> {
        self.by_context.iter()
    }

    /// Convert this predictor into an interator.
    pub fn into_iter(
        self,
    ) -> impl Iterator<Item = (Context, ContextInformation<NodeValue, Statistics>)> {
        self.by_context.into_iter()
    }

    /// The number of states in this predictor.
    pub fn len(&self) -> usize {
        self.by_context.values().map(ContextInformation::len).sum()
    }

    /// Iter through the information available in a given context.
    pub fn iter_at<C: ?Sized>(&self, context: &C) -> impl Iterator<Item = (&NodeValue, &Statistics)>
    where
        Context: std::borrow::Borrow<C>,
        C: Hash + Eq,
    {
        std::iter::Iterator::flatten(
            self.by_context
                .get(context)
                .into_iter()
                .map(|info| info.iter()),
        )
    }
}

impl<Context, NodeValue> ContextPredict<Context, NodeValue, Instances>
where
    Context: Eq + Hash + Clone,
    NodeValue: Eq + Hash + Clone,
{
    /// Register a value as being used in this context.
    pub fn add(&mut self, context: Context, value: NodeValue) -> AddResult {
        let stats_by_node_value = self
            .by_context
            .entry(context)
            .or_insert_with(|| ContextInformation::new());
        let new_context = stats_by_node_value.len() == 0;
        let new_value = Into::<usize>::into(stats_by_node_value.add(value)) == 1;
        AddResult {
            new_context,
            new_value,
        }
    }

    /// Register a value as being used in this context.
    ///
    /// If the value is already used, do not increase the number of instances.
    pub fn add_if_absent(&mut self, context: Context, value: NodeValue) {
        let stats_by_node_value = self
            .by_context
            .entry(context)
            .or_insert_with(|| ContextInformation::new());
        stats_by_node_value.add_if_absent(value)
    }
}

impl<Context, NodeValue> ContextPredict<Context, NodeValue, SymbolInfo>
where
    Context: Eq + Hash + Clone,
    NodeValue: Eq + Hash + Clone,
{
    /// Get a value by context and index.
    ///
    /// `candidates` is a list of contexts which may be known to the
    /// predictor. This method looks for the first context of `candidates`
    /// known to the predictor, and uses the index => value mapping for
    /// that context.
    ///
    /// This is typically used to provide fallback dictionaries.
    ///
    /// This method is only implemented when `Statistics=SymbolInfo` as the index is initialized
    /// by `instances_to_probabilities`. The index corresponds to the one defined in
    /// the `SymbolInfo`.
    pub fn value_by_symbol_index<C2: ?Sized>(
        &self,
        candidates: &[&C2],
        index: SymbolIndex,
    ) -> Option<&NodeValue>
    where
        Context: std::borrow::Borrow<C2>,
        C2: Hash + Eq,
    {
        for context in candidates {
            if let Some(table) = self.by_context.get(context) {
                return table.value_by_symbol_index(index);
            }
        }
        None
    }

    /// Get the frequency information in one of several contexts.
    ///
    /// `candidates` is a list of contexts which may be known to the
    /// predictor. This method looks for the first context of `candidates`
    /// known to the predictor, and gets the information that context.
    ///
    /// This is typically used to provide fallback dictionaries.
    pub fn frequencies_at<C2: ?Sized>(
        &self,
        candidates: &[&C2],
    ) -> Option<&Rc<RefCell<range_encoding::CumulativeDistributionFrequency>>>
    where
        Context: std::borrow::Borrow<C2>,
        C2: Hash + Eq,
    {
        let table = self.context_info_at(candidates)?;
        table.frequencies()
    }

    /// Get the stats for a specific value in one of several contexts.
    ///
    /// `candidates` is a list of contexts which may be known to the
    /// predictor. This method looks for the first context of `candidates`
    /// known to the predictor, and gets the stats for the value in
    /// that context.
    ///
    /// This is typically used to provide fallback dictionaries.
    pub fn stats_by_node_value<C2: ?Sized>(
        &self,
        candidates: &[&C2],
        value: &NodeValue,
    ) -> Option<&SymbolInfo>
    where
        Context: std::borrow::Borrow<C2>,
        C2: Hash + Eq,
    {
        let context_info = self.context_info_at(candidates)?;
        context_info.stats_by_node_value().get(value)
    }
    pub fn stats_by_node_value_mut<C2: ?Sized>(
        &mut self,
        candidates: &[&C2],
        value: &NodeValue,
    ) -> Option<&mut SymbolInfo>
    where
        Context: std::borrow::Borrow<C2>,
        C2: Hash + Eq,
    {
        let context_info = self.context_info_at_mut(candidates)?;
        context_info.stats_by_node_value_mut().get_mut(value)
    }

    fn context_info_at<C2: ?Sized>(
        &self,
        candidates: &[&C2],
    ) -> Option<&ContextInformation<NodeValue, SymbolInfo>>
    where
        Context: std::borrow::Borrow<C2>,
        C2: Hash + Eq,
    {
        // This is an ugly workaround, as we cannot call `get_mut`
        // from the loop.
        let mut found = None;
        for context in candidates {
            if self.by_context.get(context).is_some() {
                found = Some(context);
                break;
            }
        }
        match found {
            None => return None,
            Some(context) => return self.by_context.get(context),
        }
    }

    fn context_info_at_mut<C2: ?Sized>(
        &mut self,
        candidates: &[&C2],
    ) -> Option<&mut ContextInformation<NodeValue, SymbolInfo>>
    where
        Context: std::borrow::Borrow<C2>,
        C2: Hash + Eq,
    {
        // This is an ugly workaround, as we cannot call `get_mut`
        // from the loop.
        let mut found = None;
        for context in candidates {
            if self.by_context.get(context).is_some() {
                found = Some(context);
                break;
            }
        }
        match found {
            None => return None,
            Some(context) => return self.by_context.get_mut(context),
        }
    }
}

impl<Context, NodeValue> InstancesToProbabilities for ContextPredict<Context, NodeValue, Instances>
where
    Context: Eq + Hash + Clone + std::fmt::Debug,
    NodeValue: Eq + Hash + Clone + Ord,
{
    type AsProbabilities = ContextPredict<Context, NodeValue, SymbolInfo>;
    fn instances_to_probabilities(
        &self,
        description: &str,
    ) -> ContextPredict<Context, NodeValue, SymbolInfo> {
        debug!(target: "entropy", "Converting ContextPredict {} to probabilities", description);
        let by_context = self
            .by_context
            .iter()
            .map(|(context, info)| {
                (
                    context.clone(),
                    info.instances_to_probabilities("ContextInformation"),
                )
            })
            .collect();
        ContextPredict { by_context }
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
pub struct PathPredict<NodeValue, Statistics>
where
    NodeValue: Eq + Hash + Clone,
{
    /// The amount of context to use.
    ///
    /// With a depth of 0, paths are ignored. With a depth of 1, we only take into account
    /// the node/field. With a depth of 2, we also take into account the node/field of the
    /// grand parent, etc.
    depth: usize,

    /// Actual information stored.
    context_predict: ContextPredict<IOPath, NodeValue, Statistics>,
}

impl<NodeValue> InstancesToProbabilities for PathPredict<NodeValue, Instances>
where
    NodeValue: Eq + Hash + Clone + Ord,
{
    type AsProbabilities = PathPredict<NodeValue, SymbolInfo>;
    fn instances_to_probabilities(&self, description: &str) -> PathPredict<NodeValue, SymbolInfo> {
        PathPredict {
            depth: self.depth,
            context_predict: self.context_predict.instances_to_probabilities(description),
        }
    }
}

impl<NodeValue, Statistics> PathPredict<NodeValue, Statistics>
where
    NodeValue: Eq + Hash + Clone,
{
    pub fn new(depth: usize) -> Self {
        PathPredict {
            depth,
            context_predict: ContextPredict::new(),
        }
    }

    /// The number of states in this predictor.
    pub fn len(&self) -> usize {
        self.context_predict.len()
    }

    /// The depth of this predictor.
    ///
    /// A depth of 1 means that the predictor uses only information on the `(interface, field)`
    /// to predict values. A depth of 2 means that the predictor also uses the `(interface, field)`
    /// of the parent interface, etc.
    pub fn depth(&self) -> usize {
        self.depth
    }

    /// All the paths known to this predictor.
    ///
    /// Used mainly for debugging.
    pub fn paths(&self) -> impl Iterator<Item = &IOPath> {
        self.context_predict.contexts()
    }

    /// Convert this predictor into an interator.
    pub fn into_iter(
        self,
    ) -> impl Iterator<Item = (IOPath, ContextInformation<NodeValue, Statistics>)> {
        self.context_predict.into_iter()
    }

    pub fn iter(
        &self,
    ) -> impl Iterator<Item = (&IOPath, &ContextInformation<NodeValue, Statistics>)> {
        self.context_predict.iter()
    }

    /// Return a tail of a path with the depth adapted to this predictor, as an IOPath.
    pub fn path_tail(&self, path: &[IOPathItem]) -> IOPath {
        let tail = Self::tail_of(path, self.depth);
        let mut result = IOPath::new();
        result.extend_from_slice(tail);
        result
    }

    /// Return a tail of a path with the depth adapted to this predictor.
    fn tail<'a>(&self, path: &'a [IOPathItem]) -> &'a [IOPathItem] {
        Self::tail_of(path, self.depth)
    }

    /// Return a tail of a path with the specified depth.
    ///
    /// If the path is already shorter than the specified depth, return the path unchanged.
    fn tail_of<'a>(path: &'a [IOPathItem], depth: usize) -> &'a [IOPathItem] {
        let path = if path.len() <= depth {
            path
        } else {
            &path[path.len() - depth..]
        };
        path
    }

    pub fn iter_at(&self, path: &[IOPathItem]) -> impl Iterator<Item = (&NodeValue, &Statistics)> {
        let tail = self.tail(path);
        self.context_predict.iter_at(tail)
    }
}
impl<NodeValue> PathPredict<NodeValue, Instances>
where
    NodeValue: Eq + Hash + Clone,
{
    /// Register a value as being used at this path.
    pub fn add(&mut self, path: &[IOPathItem], value: NodeValue) -> AddResult {
        let tail = self.tail(path);
        let mut as_path = IOPath::new();
        as_path.extend_from_slice(tail);
        self.context_predict.add(as_path, value)
    }

    pub fn add_fallback(&mut self, other: &Self) {
        debug_assert!(
            !other.paths().any(|path| path.len() > 1),
            "The fallback dictionary should only contain paths of length 0 or 1."
        );

        debug!(target: "dictionary", "Adding fallback of length {}", other.len());
        // This dictionary already contains a number of paths, presumably
        // built from sampling. We need to make sure that each of these paths
        // maps to a number of instances > 0 to each value that may possibly
        // appear at this path.
        //
        // Therefore, we extend each known path with the values available in `other`.
        // Note that `other` only contains paths of length 0 or 1. We'll deal
        // with paths of length 0 in a further step. To deal with paths of length
        // 1, we must restrict ourselves to the tail of know paths.
        for (path, stats_by_node_value) in self.context_predict.iter_mut() {
            if path.len() == 0 {
                continue;
            }

            let tail = path.tail(1);
            for (value, statistics) in other.iter_at(tail) {
                debug_assert_eq!(Into::<usize>::into(*statistics), 1);
                stats_by_node_value.add_if_absent(value.clone());
            }
        }

        // If the sampling was insufficient, the dictionary may not know *all* possible paths.
        // We now insert wall the paths of `other` as (shorter) paths. The lookup methods know
        // that if they fail to find a path of full length, they should look for its length 1 tail.
        // This copy also handles any length 0 path.
        for (path, information) in other.iter() {
            for (value, statistics) in information.iter() {
                debug_assert_eq!(Into::<usize>::into(*statistics), 1);
                self.add_if_absent(path.borrow(), value.clone());
            }
        }
    }

    pub fn add_if_absent(&mut self, path: &[IOPathItem], value: NodeValue) {
        let tail = self.tail(path);
        let mut as_path = IOPath::new();
        as_path.extend_from_slice(tail);
        self.context_predict.add_if_absent(as_path, value);
    }
}
impl<NodeValue> PathPredict<NodeValue, SymbolInfo>
where
    NodeValue: Eq + Hash + Clone,
{
    /// Get a value by path and index.
    ///
    /// This method is only implemented when `Statistics=SymbolInfo` as the index is initialized
    /// by `instances_to_probabilities`.
    pub fn value_by_symbol_index(
        &self,
        path: &[IOPathItem],
        index: SymbolIndex,
    ) -> Option<&NodeValue> {
        if path.len() >= 2 {
            let candidates = [
                // Case 1: If the path has been encountered during sampling.
                self.tail(path),
                // Case 2: If the path has not been encountered during sampling, fallback to its length 1 suffix.
                Self::tail_of(path, 1),
            ];
            self.context_predict
                .value_by_symbol_index(&candidates, index)
        } else {
            // The path has length 0 or 1, there is only one way to represent it in this PathPredict.
            let candidates = [path];
            self.context_predict
                .value_by_symbol_index(&candidates, index)
        }
    }

    /// Get the stats for a specific value at a specific path.
    pub fn stats_by_node_value(&self, path: &[IOPathItem], value: &NodeValue) -> Option<&SymbolInfo>
    where
        NodeValue: std::fmt::Debug,
    {
        if path.len() >= 2 {
            let candidates = [
                // Case 1: If the path has been encountered during sampling.
                self.tail(path),
                // Case 2: If the path has not been encountered during sampling, fallback to its length 1 suffix.
                Self::tail_of(path, 1),
            ];
            self.context_predict.stats_by_node_value(&candidates, value)
        } else {
            // The path has length 0 or 1, there is only one way to represent it in this PathPredict.
            let candidates = [path];
            self.context_predict.stats_by_node_value(&candidates, value)
        }
    }
    pub fn stats_by_node_value_mut(
        &mut self,
        path: &[IOPathItem],
        value: &NodeValue,
    ) -> Option<&mut SymbolInfo>
    where
        NodeValue: std::fmt::Debug,
    {
        if path.len() >= 2 {
            let candidates = [
                // Case 1: If the path has been encountered during sampling.
                self.tail(path),
                // Case 2: If the path has not been encountered during sampling, fallback to its length 1 suffix.
                Self::tail_of(path, 1),
            ];
            self.context_predict
                .stats_by_node_value_mut(&candidates, value)
        } else {
            // The path has length 0 or 1, there is only one way to represent it in this PathPredict.
            let candidates = [path];
            self.context_predict
                .stats_by_node_value_mut(&candidates, value)
        }
    }

    /// Get frequency information for a given path.
    pub fn frequencies_at(
        &self,
        path: &[IOPathItem],
    ) -> Option<&Rc<RefCell<range_encoding::CumulativeDistributionFrequency>>> {
        if path.len() >= 2 {
            let candidates = [
                // Case 1: If the path has been encountered during sampling.
                self.tail(path),
                // Case 2: If the path has not been encountered during sampling, fallback to its length 1 suffix.
                Self::tail_of(path, 1),
            ];
            self.context_predict.frequencies_at(&candidates)
        } else {
            // The path has length 0 or 1, there is only one way to represent it in this PathPredict.
            let candidates = [path];
            self.context_predict.frequencies_at(&candidates)
        }
    }
}

/// An index for a value in `WindowPredict`.
#[derive(Debug, PartialEq, Eq, Hash, Clone, Serialize, Deserialize, PartialOrd, Ord)]
enum WindowPrediction {
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
pub struct WindowPredict<NodeValue, Statistics>
where
    NodeValue: Clone + Eq + Hash,
{
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
impl<NodeValue, Statistics> WindowPredict<NodeValue, Statistics>
where
    NodeValue: Clone + Eq + Hash,
{
    /// Create a window predictor for a given window width.
    pub fn new(width: usize) -> Self {
        WindowPredict {
            width,
            value_by_dictionary_index: Vec::with_capacity(1024), // FIXME: Magic number
            dictionary_index_by_value: HashMap::with_capacity(1024),
            latest_values: Vec::with_capacity(width),
            info: ContextInformation::new(),
        }
    }

    /// Update current window by moving the value at index `index` to the latest-seen
    /// position (index 0).
    ///
    /// Fails if the index is not in [0, self.latest_values.len()[
    fn window_move_to_front(&mut self, index: BackReference) -> Result<(), ()> {
        let as_usize = Into::<usize>::into(index);
        if as_usize == 0 {
            // `rotate_right` doesn't work on an empty slice, so we need to handle
            // this case manually.
            return Ok(());
        }
        if as_usize >= self.latest_values.len() {
            return Err(());
        }
        let ref mut slice = self.latest_values.as_mut_slice()[0..as_usize];
        slice.rotate_right(1);
        Ok(())
    }

    /// Update current window by putting a value to the latest-seen position (index 0).
    ///
    /// If the value is already in the window, it is moved to the latest-seen position,
    /// otherwise, it is added directly in latest-seen position. If the resulting window
    /// is too large, it is truncated.
    fn window_insert_value(&mut self, value: &NodeValue) -> Option<BackReference> {
        // It's possible that the value was present in the window.
        if let Some(index) = self.latest_values.iter().position(|v| v == value) {
            let index = BackReference(index);
            self.window_move_to_front(index).unwrap(); // We have just checked that `index < self.latest_values.len()`.
            return Some(index);
        }
        if self.latest_values.len() < self.width {
            // If the window isn't full yet, simply add the value at start.
            self.latest_values.insert(0, value.clone());
        } else {
            // Otherwise, push front and remove last.
            let slice = self.latest_values.as_mut_slice();
            slice[slice.len() - 1] = value.clone();
            slice.rotate_right(1);
        }
        None
    }
}
impl<NodeValue> WindowPredict<NodeValue, Instances>
where
    NodeValue: Clone + Eq + std::hash::Hash + std::fmt::Debug,
{
    pub fn add(&mut self, value: NodeValue) {
        // --- At this stage, we don't know whether the value is known.

        debug!(target: "predict", "WindowPredict: Inserting value {:?}", value);
        let number_of_values = self.value_by_dictionary_index.len();
        let dictionary_index = *self
            .dictionary_index_by_value
            .entry(value.clone())
            .or_insert(DictionaryIndex(number_of_values));

        if dictionary_index == DictionaryIndex(number_of_values) {
            // We've just inserted `value`.
            self.value_by_dictionary_index.push(value.clone());
        } else {
            // Value was already known.
            let index: usize = dictionary_index.into();
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
        self.info.add(symbol);
    }
}

impl<NodeValue> WindowPredict<NodeValue, SymbolInfo>
where
    NodeValue: Clone + Eq + std::hash::Hash + std::fmt::Debug,
{
    /// Return all the frequencies in the window.
    pub fn frequencies(
        &self,
    ) -> Option<&Rc<RefCell<range_encoding::CumulativeDistributionFrequency>>> {
        self.info
            .stats_by_node_value()
            .values()
            .next()
            .map(|any| &any.distribution)
    }

    /// Access value from its index.
    // FIXME: We should find a way to enforce a specific mapping between `index` and `WindowPredict`,
    // to make it easy to decode.
    pub fn value_by_symbol_index(&mut self, index: SymbolIndex) -> Option<NodeValue> {
        match self.info.value_by_symbol_index(index) {
            None => None,
            Some(&WindowPrediction::DictionaryIndex(dictionary_index)) => {
                // Global entry.
                let result = self
                    .value_by_dictionary_index
                    .get(dictionary_index.0)?
                    .clone();
                self.window_insert_value(&result);
                Some(result)
            }
            Some(&WindowPrediction::BackReference(index)) => {
                let as_usize: usize = index.into();
                let result = self.latest_values.get(as_usize)?.clone();
                if let Err(_) = self.window_move_to_front(index) {
                    return None;
                }
                Some(result)
            }
        }
    }

    /// Access information for a value.
    pub fn stats_by_node_value_mut(&mut self, value: &NodeValue) -> Option<&mut SymbolInfo> {
        // At this stage, the value may appear in both the dictionary
        // and the window. We'll favor the window if possible.
        debug!(target: "predict", "WindowPredict: Fetching {:?}", value);
        let prediction = match self.window_insert_value(value) {
            Some(backref) => WindowPrediction::BackReference(backref),
            None => {
                debug!(target: "predict", "WindowPredict: Value {:?} is not in the window, let's look for it in the dictionary", value);
                let index = self.dictionary_index_by_value.get(value)?.clone();
                WindowPrediction::DictionaryIndex(index)
            }
        };

        debug!(target: "predict", "WindowPredict: {:?} has just been inserted and will be encoded as {:?}", value, prediction);
        self.info.stats_by_node_value_mut().get_mut(&prediction)
    }
}

impl<NodeValue> InstancesToProbabilities for WindowPredict<NodeValue, Instances>
where
    NodeValue: Clone + Eq + Hash + Ord,
{
    type AsProbabilities = WindowPredict<NodeValue, SymbolInfo>;
    fn instances_to_probabilities(
        &self,
        _description: &str,
    ) -> WindowPredict<NodeValue, SymbolInfo> {
        WindowPredict {
            width: self.width,
            value_by_dictionary_index: self.value_by_dictionary_index.clone(),
            dictionary_index_by_value: self.dictionary_index_by_value.clone(),
            latest_values: Vec::with_capacity(self.width),
            info: self.info.instances_to_probabilities("WindowPredict::info"),
        }
    }
}
