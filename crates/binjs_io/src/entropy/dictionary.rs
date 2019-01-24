use entropy::predict::{PathPredict, WindowPredict};
use entropy::probabilities::{InstancesToProbabilities, SymbolIndex, SymbolInfo};

use io::TokenWriter;
use TokenWriterError;

use binjs_shared::{
    FieldName, IdentifierName, InterfaceName, Node, PropertyKey, SharedString, F64,
};

use itertools::Itertools;

use std;
use std::collections::HashMap;

pub type IOPath = binjs_shared::ast::Path<
    InterfaceName,
    (
        /* child index */ usize,
        /* field name */ FieldName,
    ),
>;

pub use entropy::predict::Instances;

/// A newtype for `usize` used to count the number of some item in a given file.
#[derive(
    Default,
    Serialize,
    Deserialize,
    From,
    Into,
    AddAssign,
    Clone,
    Copy,
    Debug,
    PartialOrd,
    Ord,
    PartialEq,
    Eq,
)]
pub struct InstancesInFile(pub usize);

/// A newtype for `usize` used to count the number of files containing some item.
#[derive(
    Default,
    Display,
    Serialize,
    Deserialize,
    From,
    Into,
    AddAssign,
    Clone,
    Copy,
    Debug,
    PartialOrd,
    Ord,
    PartialEq,
    Eq,
)]
pub struct FilesContaining(pub usize);

/// Add a single symbol to the table of probabilities for a given path.
///
/// Note that this macro could not be implemented as a simple method, as we need to adapt it to different field names.
///
/// Usage:
/// `update_in_context!(self, name_of_the_probability_table, "Description, used for debugging", value_to_encode, path_in_the_ast)`
macro_rules! update_in_context {
    ( $me: ident, $table: ident, $description: expr, $path:expr, $value: expr ) => {{
        use std::borrow::Borrow;

        let path = $path.borrow();
        $me.dictionary.$table.add(path, $value);

        Ok(())
    }};
}

/// Add a single symbol to the table of probabilities for a prediction window
/// (see `predict::WindowPredict` for details).
///
/// Note that this macro could not be implemented as a simple method, as we need to adapt it to different field names.
///
/// Usage:
/// `update_in_window!(self, name_of_the_probability_table, value_to_encode)`
macro_rules! update_in_window {
    ( $me: ident, $table:ident, $value: expr ) => {{
        $me.dictionary.$table.add($value);

        Ok(())
    }};
}

/// Count one instance of a user-extensible value in the AST. Used to count the number
/// of instances of e.g. each specific string, each specific number, etc.
///
/// Note that this macro could not be implemented as a simple method, as we need to adapt it to different field names.
///
/// Usage:
/// `increment_instance_count!(self, name_of_the_probability_table, value_to_count)`
macro_rules! increment_instance_count {
    ( $me: ident, $table: ident, $value: expr ) => {
        debug!(target: "dictionary", "Dictionary: Inserting instance {:?}", $value);
        $me.instances_of_user_extensible_data_in_current_file.$table
            .entry($value)
            .and_modify(|instances| {
                *instances += InstancesInFile(1) // We have already seen this string in this file, increment.
            }).or_insert(InstancesInFile(1));    // First time we see this string in this file, store 1.
    }
}

/// Result of a fetch operation in a IndexedTable.
#[derive(Clone, Copy, Debug)]
pub enum Fetch {
    /// The value was already in the cache at the given index.
    Hit(usize),

    /// The value was not in the cache. A slot has been allocated at the given index,
    /// but the definition still needs to be added.
    Miss(usize),
}

/// The initial size of IndexedTables, in elements.
const INDEXED_TABLE_INITIAL_CAPACITY: usize = 1024;

/// A data structure designed to cache information accessible
/// either by index or by value.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct IndexedTable<T>
where
    T: Eq + std::hash::Hash + Clone,
{
    values: Vec<T>,
    indices: HashMap<T, usize>,
}
impl<T> IndexedTable<T>
where
    T: Eq + std::hash::Hash + Clone + std::fmt::Debug + Ord,
{
    /// Create a new IndexedTable from a list of instances.
    ///
    /// Values appearing in `instances` that have `threshold` instances or less are ignored.
    pub fn new(
        value_to_instances: HashMap<T, FilesContaining>,
        threshold: FilesContaining,
    ) -> Self {
        debug!(target: "dictionary", "Creating a IndexedTable with a threshold of {}", Into::<usize>::into(threshold));
        let value_to_instances = value_to_instances.into_iter().sorted(); // We sort to enforce traversal order.
        let mut values = Vec::with_capacity(INDEXED_TABLE_INITIAL_CAPACITY);
        let mut indices = HashMap::with_capacity(INDEXED_TABLE_INITIAL_CAPACITY);
        for (value, instances) in value_to_instances {
            debug!(target: "dictionary", "Should we add {:?} to the IndexedTable?", value);
            if instances <= threshold {
                // Too few instances, skipping.
                debug!(target: "dictionary", "Too few instances: {} <= {} for {:?}", instances, threshold, value);
                continue;
            }
            let len = values.len();
            values.push(value.clone());
            let prev = indices.insert(value, len);
            assert!(prev.is_none());
        }
        let result = IndexedTable { values, indices };
        debug!(target: "dictionary", "Dictionary: IndexedTable contains {:?}", result);
        result
    }

    /// Create an empty `IndexedTable`.
    pub fn with_capacity(len: usize) -> Self {
        IndexedTable {
            values: Vec::with_capacity(len),
            indices: HashMap::with_capacity(len),
        }
    }

    /// Attempt to get the index for a value from the `IndexedTable`.
    ///
    /// If the value is already in the cache, return `Fetch::Hit(index)`, where `index` is the
    /// immutable index of the value. Otherwise, allocate a new slot `index` and return
    /// `Fetch::Miss(index)`.
    pub fn fetch_index(&mut self, value: &T) -> Fetch {
        use std::collections::hash_map::Entry::*;
        debug!(target: "dictionary", "Dictionary: 'I'm looking for {:?} in {:?}", value, self);
        let len = self.values.len();
        match self.indices.entry(value.clone()) {
            Occupied(slot) => return Fetch::Hit(*slot.get()),
            Vacant(slot) => {
                slot.insert(len);
            }
        }
        self.values.push(value.clone());
        Fetch::Miss(len)
    }

    /// Return the current state of the cache as a slice.
    ///
    /// It may be accessed by the index originally returned by the call to `fetch`.
    pub fn as_slice(&self) -> &[T] {
        &self.values
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Dictionary<T> {
    // --- Non-extensible sets of symbols, predicted by path.
    // Used for entropy coding.
    // ---
    /// All booleans appearing in the AST, predicted by path.
    pub bool_by_path: PathPredict<Option<bool>, T>,

    /// All string enumerations, predicted by path.
    pub string_enum_by_path: PathPredict<SharedString, T>,

    /// All interface names, predicted by path.
    pub interface_name_by_path: PathPredict<InterfaceName, T>,

    // --- Extensible sets of symbols, predicted by path.
    // Used for experiments with entropy coding, but so far, not very
    // good with extensibility. There are good chances that this section
    // will disappear in future versions.
    // ---
    /// All floats appearing in the AST.
    pub float_by_path: PathPredict<Option<F64>, T>,

    /// All unsigned longs appearing in the AST.
    pub unsigned_long_by_path: PathPredict<u32, T>,

    /// All property keys.
    pub property_key_by_path: PathPredict<Option<PropertyKey>, T>,

    /// All identifier names, predicted by path.
    pub identifier_name_by_path: PathPredict<Option<IdentifierName>, T>,

    /// All string literals, predicted by path.
    pub string_literal_by_path: PathPredict<Option<SharedString>, T>,

    /// All list lengths, predicted by path.
    pub list_length_by_path: PathPredict<Option<u32>, T>,

    // --- Extensible sets of symbols, predicted by window.
    // Used for experiments for extensibility of entropy coding, but so far,
    // not very good at compression, and might cause serious alignment issues.
    // There are good chances that this section will disappear in future versions.
    // ---
    /// All property keys, predicted by window.
    pub property_key_by_window: WindowPredict<Option<PropertyKey>, T>,

    /// All identifier names, predicted by window.
    pub identifier_name_by_window: WindowPredict<Option<IdentifierName>, T>,

    /// All string literals, predicted by window.
    pub string_literal_by_window: WindowPredict<Option<SharedString>, T>,

    // --- Extensible sets of symbols, as indexed tables.
    // Used to represent instances of extensible sets of symbols as indices in
    // a table. Pretty good for extensibility, experiments pending on
    // compression-level and performance.
    // ---
    /// All unsigned longs.
    pub unsigned_longs: IndexedTable<u32>,

    /// All string literals. `None` for `null`.
    pub string_literals: IndexedTable<Option<SharedString>>,

    /// All identifier names. `None` for `null`.
    pub identifier_names: IndexedTable<Option<IdentifierName>>,

    /// All property keys. `None` for `null`.
    pub property_keys: IndexedTable<Option<PropertyKey>>,

    /// All list lenghts. `None` for `null`.
    pub list_lengths: IndexedTable<Option<u32>>,

    /// All floats. `None` for `null`.
    pub floats: IndexedTable<Option<F64>>,
    // Missing:
    // - offsets (cannot be predicted?)
    // - directives?
}
impl<T> Dictionary<T> {
    /// Create a new dictionary using paths of `depth` depth
    /// and windows of `width` width.
    pub fn new(depth: usize, width: usize) -> Self {
        Dictionary {
            // By path.
            bool_by_path: PathPredict::new(depth),
            float_by_path: PathPredict::new(depth),
            unsigned_long_by_path: PathPredict::new(depth),
            string_enum_by_path: PathPredict::new(depth),
            property_key_by_path: PathPredict::new(depth),
            identifier_name_by_path: PathPredict::new(depth),
            string_literal_by_path: PathPredict::new(depth),
            list_length_by_path: PathPredict::new(depth),
            interface_name_by_path: PathPredict::new(depth),

            // By window.
            property_key_by_window: WindowPredict::new(width),
            identifier_name_by_window: WindowPredict::new(width),
            string_literal_by_window: WindowPredict::new(width),

            // Indexed tables.
            string_literals: IndexedTable::with_capacity(0),
            identifier_names: IndexedTable::with_capacity(0),
            property_keys: IndexedTable::with_capacity(0),
            list_lengths: IndexedTable::with_capacity(0),
            floats: IndexedTable::with_capacity(0),
            unsigned_longs: IndexedTable::with_capacity(0),
        }
    }

    pub fn depth(&self) -> usize {
        assert_eq!(self.bool_by_path.depth(), self.string_enum_by_path.depth());
        assert_eq!(self.bool_by_path.depth(), self.interface_name_by_path.depth());
        self.bool_by_path.depth()
    }

    /// Return the number of states in this dictionary.
    pub fn len(&self) -> usize {
        // Make sure that we don't forget a field.
        let Dictionary {
            ref bool_by_path,
            ref float_by_path,
            ref unsigned_long_by_path,
            ref string_enum_by_path,
            ref property_key_by_path,
            ref identifier_name_by_path,
            ref string_literal_by_path,
            ref list_length_by_path,
            ref interface_name_by_path,
            property_key_by_window: _,
            string_literal_by_window: _,
            identifier_name_by_window: _,
            string_literals: _,
            identifier_names: _,
            property_keys: _,
            list_lengths: _,
            floats: _,
            unsigned_longs: _,
        } = *self;

        bool_by_path.len()
            + float_by_path.len()
            + unsigned_long_by_path.len()
            + string_enum_by_path.len()
            + property_key_by_path.len()
            + identifier_name_by_path.len()
            + interface_name_by_path.len()
            + string_literal_by_path.len()
            + list_length_by_path.len()
            + interface_name_by_path.len()
    }
}

impl Dictionary<Instances> {
    pub fn with_grammar_fallback(self, fallback: Dictionary<Instances>) -> Self {
        use std::borrow::Borrow;
        let mut result = self;

        for (path, value, instances) in fallback.bool_by_path.into_iter() {
            assert_eq!(Into::<usize>::into(instances), 1); // Sanity check
            result.bool_by_path.add_if_absent(path.borrow(), value);
        }

        for (path, value, instances) in fallback.string_enum_by_path.into_iter() {
            assert_eq!(Into::<usize>::into(instances), 1); // Sanity check
            result.string_enum_by_path.add_if_absent(path.borrow(), value);
        }

        for (path, value, instances) in fallback.interface_name_by_path.into_iter() {
            assert_eq!(Into::<usize>::into(instances), 1); // Sanity check
            result.interface_name_by_path.add_if_absent(path.borrow(), value);
        }

        result
    }
}

impl InstancesToProbabilities for Dictionary<Instances> {
    type AsProbabilities = Dictionary<SymbolInfo>;

    /// Convert a dictionary counting instances into a dictionary that
    /// counts probabilities.
    fn instances_to_probabilities(self, _description: &str) -> Dictionary<SymbolInfo> {
        Dictionary {
            // By path.
            bool_by_path: self.bool_by_path.instances_to_probabilities("bool_by_path"),
            float_by_path: self
                .float_by_path
                .instances_to_probabilities("float_by_path"),
            unsigned_long_by_path: self
                .unsigned_long_by_path
                .instances_to_probabilities("unsigned_long_by_path"),
            string_enum_by_path: self
                .string_enum_by_path
                .instances_to_probabilities("string_enum_by_path"),
            property_key_by_path: self
                .property_key_by_path
                .instances_to_probabilities("property_key_by_path"),
            identifier_name_by_path: self
                .identifier_name_by_path
                .instances_to_probabilities("identifier_name_by_path"),
            interface_name_by_path: self
                .interface_name_by_path
                .instances_to_probabilities("interface_name_by_path"),
            string_literal_by_path: self
                .string_literal_by_path
                .instances_to_probabilities("string_literal_by_path"),
            list_length_by_path: self
                .list_length_by_path
                .instances_to_probabilities("list_length_by_path"),

            // By window.
            property_key_by_window: self
                .property_key_by_window
                .instances_to_probabilities("property_key_by_window"),
            identifier_name_by_window: self
                .identifier_name_by_window
                .instances_to_probabilities("identifier_name_by_window"),
            string_literal_by_window: self
                .string_literal_by_window
                .instances_to_probabilities("string_literal_by_window"),

            // Indexed tables.
            string_literals: self.string_literals,
            floats: self.floats,
            list_lengths: self.list_lengths,
            identifier_names: self.identifier_names,
            property_keys: self.property_keys,
            unsigned_longs: self.unsigned_longs,
        }
    }
}

/// A container for all the user-extensible data in files,
/// e.g. string literals or numbers.
///
/// This container is used to collect statistics, such as the number
/// of instances of a given string in a file, or the number of files
/// that contain a given string.
#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub struct UserExtensibleData<T> {
    /// Instances of IdentifierName.
    pub identifier_name_instances: HashMap<Option<IdentifierName>, T>,

    /// Instances of PropertyKey
    pub property_key_instances: HashMap<Option<PropertyKey>, T>,

    /// Instances of InterfaceName
    pub interface_name_instances: HashMap<InterfaceName, T>,

    /// Instances of string literals.
    pub string_literal_instances: HashMap<Option<SharedString>, T>,

    /// Instances of string enums.
    pub string_enum_instances: HashMap<SharedString, T>,

    /// Instances of list lengths.
    pub list_length_instances: HashMap<Option<u32>, T>,

    /// Instances of floating-point numbers.
    pub float_instances: HashMap<Option<F64>, T>,

    /// Instances of unsigned longs.
    pub unsigned_long_instances: HashMap<u32, T>,
}
impl<T> UserExtensibleData<T> {
    pub fn len(&self) -> usize {
        // Make sure that we don't forget a field.
        let UserExtensibleData {
            ref identifier_name_instances,
            ref property_key_instances,
            ref interface_name_instances,
            ref string_literal_instances,
            ref string_enum_instances,
            ref list_length_instances,
            ref float_instances,
            ref unsigned_long_instances,
        } = *self;
        identifier_name_instances.len()
            + property_key_instances.len()
            + string_literal_instances.len()
            + string_enum_instances.len()
            + interface_name_instances.len()
            + list_length_instances.len()
            + float_instances.len()
            + unsigned_long_instances.len()
    }
}

impl<K> InstancesToProbabilities for HashMap<K, FilesContaining>
where
    K: Eq + std::hash::Hash,
{
    type AsProbabilities = HashMap<K, SymbolInfo>;

    fn instances_to_probabilities(self, _description: &str) -> HashMap<K, SymbolInfo> {
        use std::cell::RefCell;
        use std::rc::Rc;

        let instances = self
            .values()
            .map(|x| {
                let x: usize = x.clone().into();
                x as u32
            })
            .collect();
        let distribution = Rc::new(RefCell::new(
            range_encoding::CumulativeDistributionFrequency::new(instances),
        ));

        self.into_iter()
            .enumerate()
            .map(|(index, (key, _))| {
                (
                    key,
                    SymbolInfo {
                        index: SymbolIndex::from(index),
                        distribution: distribution.clone(),
                    },
                )
            })
            .collect()
    }
}

/// A structure used to build a dictionary based on a sample of files.
pub struct DictionaryBuilder {
    /// A dictionary.
    ///
    /// This is a shared reference as we typically wish to
    /// access this field after the DictionaryBuilder
    /// has been consumed and released by a `Serializer`.
    dictionary: Dictionary<Instances>,

    /// Number of instances of each string in the current file.
    instances_of_user_extensible_data_in_current_file: UserExtensibleData<InstancesInFile>,

    /// Number of files in which each string appears.
    files_containing_user_extensible_data: UserExtensibleData<FilesContaining>,
}

impl DictionaryBuilder {
    /// Create a new dictionary builder using paths of `depth` depth
    /// and windows of `width` width.
    ///
    /// Use `DictionaryBuilder::done` to convert it into a `Dictionary`.
    pub fn new(depth: usize, width: usize) -> Self {
        DictionaryBuilder {
            dictionary: Dictionary::new(depth, width),
            instances_of_user_extensible_data_in_current_file: UserExtensibleData::default(),
            files_containing_user_extensible_data: UserExtensibleData::default(),
        }
    }

    /// Return a dictionary containing all the paths collected and all
    /// the user-extensible content that appear in more than one file.
    pub fn done(self, threshold: FilesContaining) -> Dictionary<Instances> {
        // Start with the dictionary we're currently building.
        let mut dictionary = self.dictionary;

        // FIXME: Inject escape values.
        // I need an API that walks all the possible grammar paths (given the maximal depth)
        // and injects one of each possible
        // InterfaceName/String Enum/Boolean.
        //

        // Generate indexed tables for user-extensible values.
        dictionary.identifier_names = IndexedTable::new(
            self.files_containing_user_extensible_data
                .identifier_name_instances,
            threshold,
        );
        dictionary.property_keys = IndexedTable::new(
            self.files_containing_user_extensible_data
                .property_key_instances,
            threshold,
        );
        dictionary.list_lengths = IndexedTable::new(
            self.files_containing_user_extensible_data
                .list_length_instances,
            threshold,
        );
        dictionary.floats = IndexedTable::new(
            self.files_containing_user_extensible_data.float_instances,
            threshold,
        );
        dictionary.unsigned_longs = IndexedTable::new(
            self.files_containing_user_extensible_data
                .unsigned_long_instances,
            threshold,
        );
        dictionary.string_literals = IndexedTable::new(
            self.files_containing_user_extensible_data
                .string_literal_instances,
            threshold,
        );

        dictionary
    }

    pub fn len(&self) -> usize {
        self.dictionary.len()
    }

    /// Access statistics on the number of files containing specific user-extensible values.
    pub fn files_containing(&self) -> &UserExtensibleData<FilesContaining> {
        &self.files_containing_user_extensible_data
    }

    /// Access statistics on the number of instances of a specific user-extensible value.
    pub fn instances_in_file(&self) -> &UserExtensibleData<InstancesInFile> {
        &self.instances_of_user_extensible_data_in_current_file
    }

    /// Take all strings of a given nature present in a file (as stored
    /// in `self.instances_of_user_extensible_data_in_current_file`) and mark them as
    /// appearing in one more file (as stored in `self.files_containing_user_extensible_data`).
    ///
    /// The caller is responsible for making sure that `source` is a
    /// `self.instances_of_user_extensible_data_in_current_file.XXX` and `destination`
    /// is the corresponding `self.files_containing_user_extensible_data.XXX`.
    ///
    /// Note: This is a function rather than a method because making it a method
    /// would require us to borrow mutably `source` *and* while calling into `self`.
    /// Not very borrow-checker-compatible.
    fn transfer_instances<V>(
        source: &mut HashMap<V, InstancesInFile>,
        destination: &mut HashMap<V, FilesContaining>,
    ) where
        V: std::hash::Hash + Eq + Clone + std::fmt::Debug,
    {
        for (k, _) in source.drain() {
            // Increase the number of files in `destination` that contain `k` by 1,
            // ignoring the number of instances of `k` in `source`.
            destination
                .entry(k)
                .and_modify(|instances| *instances += FilesContaining(1))
                .or_insert(FilesContaining(1));
        }
    }

    fn done_with_file(&mut self) {
        // Count the number of files in which user-extensible instances appear.
        Self::transfer_instances(
            &mut self
                .instances_of_user_extensible_data_in_current_file
                .identifier_name_instances,
            &mut self
                .files_containing_user_extensible_data
                .identifier_name_instances,
        );
        Self::transfer_instances(
            &mut self
                .instances_of_user_extensible_data_in_current_file
                .property_key_instances,
            &mut self
                .files_containing_user_extensible_data
                .property_key_instances,
        );
        Self::transfer_instances(
            &mut self
                .instances_of_user_extensible_data_in_current_file
                .interface_name_instances,
            &mut self
                .files_containing_user_extensible_data
                .interface_name_instances,
        );
        Self::transfer_instances(
            &mut self
                .instances_of_user_extensible_data_in_current_file
                .string_literal_instances,
            &mut self
                .files_containing_user_extensible_data
                .string_literal_instances,
        );
        Self::transfer_instances(
            &mut self
                .instances_of_user_extensible_data_in_current_file
                .string_enum_instances,
            &mut self
                .files_containing_user_extensible_data
                .string_enum_instances,
        );
        Self::transfer_instances(
            &mut self
                .instances_of_user_extensible_data_in_current_file
                .list_length_instances,
            &mut self
                .files_containing_user_extensible_data
                .list_length_instances,
        );
        Self::transfer_instances(
            &mut self
                .instances_of_user_extensible_data_in_current_file
                .float_instances,
            &mut self.files_containing_user_extensible_data.float_instances,
        );
        Self::transfer_instances(
            &mut self
                .instances_of_user_extensible_data_in_current_file
                .unsigned_long_instances,
            &mut self
                .files_containing_user_extensible_data
                .unsigned_long_instances,
        );
    }
}

impl<'a> TokenWriter for &'a mut DictionaryBuilder {
    type Data = [u8; 0]; // Placeholder

    fn done(self) -> Result<Self::Data, TokenWriterError> {
        self.done_with_file();
        debug!(target: "entropy", "Built a dictionary with len: {}", self.dictionary.len());
        Ok([])
    }

    fn bool_at(&mut self, value: Option<bool>, path: &IOPath) -> Result<(), TokenWriterError> {
        (*self).bool_at(value, path)
    }

    fn float_at(&mut self, value: Option<f64>, path: &IOPath) -> Result<(), TokenWriterError> {
        (*self).float_at(value, path)
    }

    fn unsigned_long_at(&mut self, value: u32, path: &IOPath) -> Result<(), TokenWriterError> {
        (*self).unsigned_long_at(value, path)
    }

    fn string_enum_at(
        &mut self,
        value: &SharedString,
        path: &IOPath,
    ) -> Result<(), TokenWriterError> {
        (*self).string_enum_at(value, path)
    }

    fn string_at(
        &mut self,
        value: Option<&SharedString>,
        path: &IOPath,
    ) -> Result<(), TokenWriterError> {
        (*self).string_at(value, path)
    }

    fn property_key_at(
        &mut self,
        value: Option<&PropertyKey>,
        path: &IOPath,
    ) -> Result<(), TokenWriterError> {
        (*self).property_key_at(value, path)
    }

    fn identifier_name_at(
        &mut self,
        value: Option<&IdentifierName>,
        path: &IOPath,
    ) -> Result<(), TokenWriterError> {
        (*self).identifier_name_at(value, path)
    }

    fn enter_list_at(&mut self, len: usize, path: &IOPath) -> Result<(), TokenWriterError> {
        (*self).enter_list_at(len, path)
    }

    fn enter_tagged_tuple_at(
        &mut self,
        node: &Node,
        tag: &InterfaceName,
        children: &[&FieldName],
        path: &IOPath,
    ) -> Result<(), TokenWriterError> {
        (*self).enter_tagged_tuple_at(node, tag, children, path)
    }

    fn offset_at(&mut self, path: &IOPath) -> Result<(), TokenWriterError> {
        (*self).offset_at(path)
    }
}

impl TokenWriter for DictionaryBuilder {
    type Data = [u8; 0]; // Placeholder

    fn done(mut self) -> Result<Self::Data, TokenWriterError> {
        self.done_with_file();
        debug!(target: "entropy", "Built a dictionary with len: {}", self.dictionary.len());
        Ok([])
    }

    fn bool_at(&mut self, value: Option<bool>, path: &IOPath) -> Result<(), TokenWriterError> {
        update_in_context!(self, bool_by_path, "bool_by_path", path, value)?;
        Ok(())
    }

    fn float_at(&mut self, value: Option<f64>, path: &IOPath) -> Result<(), TokenWriterError> {
        let value = value.map(|x| x.into());
        update_in_context!(self, float_by_path, "float_by_path", path, value)?;
        increment_instance_count!(self, float_instances, value);
        Ok(())
    }

    fn unsigned_long_at(&mut self, value: u32, path: &IOPath) -> Result<(), TokenWriterError> {
        update_in_context!(
            self,
            unsigned_long_by_path,
            "unsigned_long_by_path",
            path,
            value
        )?;
        increment_instance_count!(self, unsigned_long_instances, value);
        Ok(())
    }

    fn string_enum_at(
        &mut self,
        value: &SharedString,
        path: &IOPath,
    ) -> Result<(), TokenWriterError> {
        update_in_context!(
            self,
            string_enum_by_path,
            "string_enum_by_path",
            path,
            value.clone()
        )?;
        Ok(())
    }

    fn string_at(
        &mut self,
        value: Option<&SharedString>,
        path: &IOPath,
    ) -> Result<(), TokenWriterError> {
        update_in_context!(
            self,
            string_literal_by_path,
            "string_literal_by_path",
            path,
            value.cloned()
        )?;
        update_in_window!(self, string_literal_by_window, value.cloned())?;
        increment_instance_count!(self, string_literal_instances, value.cloned());
        Ok(())
    }

    fn property_key_at(
        &mut self,
        value: Option<&PropertyKey>,
        path: &IOPath,
    ) -> Result<(), TokenWriterError> {
        update_in_context!(
            self,
            property_key_by_path,
            "property_key_by_path",
            path,
            value.cloned()
        )?;
        update_in_window!(self, property_key_by_window, value.cloned())?;
        increment_instance_count!(self, property_key_instances, value.cloned());
        Ok(())
    }

    fn identifier_name_at(
        &mut self,
        value: Option<&IdentifierName>,
        path: &IOPath,
    ) -> Result<(), TokenWriterError> {
        update_in_context!(
            self,
            identifier_name_by_path,
            "identifier_name_by_path",
            path,
            value.cloned()
        )?;
        update_in_window!(self, identifier_name_by_window, value.cloned())?;
        increment_instance_count!(self, identifier_name_instances, value.cloned());
        Ok(())
    }

    fn enter_list_at(&mut self, len: usize, path: &IOPath) -> Result<(), TokenWriterError> {
        update_in_context!(
            self,
            list_length_by_path,
            "list_length_by_path",
            path,
            Some(len as u32)
        )?;
        increment_instance_count!(self, list_length_instances, Some(len as u32));
        Ok(())
    }

    fn enter_tagged_tuple_at(
        &mut self,
        _node: &Node,
        tag: &InterfaceName,
        _children: &[&FieldName],
        path: &IOPath,
    ) -> Result<(), TokenWriterError> {
        update_in_context!(
            self,
            interface_name_by_path,
            "interface_name_by_path",
            path,
            tag.clone()
        )?;
        increment_instance_count!(self, interface_name_instances, tag.clone());
        Ok(())
    }

    fn offset_at(&mut self, _path: &IOPath) -> Result<(), TokenWriterError> {
        Ok(())
    }
}

