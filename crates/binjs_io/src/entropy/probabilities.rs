use std::cell::RefCell;
use std::rc::Rc;

/// A newtype for `usize` used to count the number of some item in a given file.
#[derive(Default, Serialize, Deserialize, From, Into, AddAssign, Clone, Copy, Constructor)]
pub struct SymbolIndex(usize);

/// Representation of a symbol in a Cumulative Distribution Frequency (CDF).
///
/// Note: We most likely don't want this struct to implement Serde's Deserialize/Serialize,
/// as this would lead to endless replication of `distribution`.
#[derive(Clone)]
pub struct SymbolInfo {
    /// The index of the symbol in the Cumulative Distribution Frequency (CDF).
    pub index: SymbolIndex,

    /// The Cumulative Distribution Frequency (CDF), shared between a number of symbols.
    pub distribution: Rc<RefCell<range_encoding::CumulativeDistributionFrequency>>,
}

/// A structure that may be converted into a probability distribution
/// or a set of probability distributions.
pub trait InstancesToProbabilities {
    type AsProbabilities;
    fn instances_to_probabilities(self, description: &str) -> Self::AsProbabilities;
}

pub trait IntoStatistics {
    type AsStatistics;
    fn into_statistics(self, description: &str) -> Self::AsStatistics;
}