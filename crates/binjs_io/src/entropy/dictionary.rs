use entropy::predict::{PathPredict, WindowPredict};
use entropy::probabilities::{InstancesToProbabilities, SymbolIndex, SymbolInfo};

use io::TokenWriter;
use TokenWriterError;

use binjs_shared::{
    FieldName, IdentifierName, InterfaceName, Node, PropertyKey, SharedString, F64,
};

use itertools::Itertools;

use std;
use std::cell::{Ref, RefCell, RefMut};
use std::collections::HashMap;
use std::rc::Rc;

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

        let addition = {
            let path = $path.borrow();
            let mut table = $me.probabilities.current_mut().$table.borrow_mut();
            table.add(path, $value)
        };

        let introduction = if addition.new_context {
            Introduction::NewTable
        } else if addition.new_value {
            Introduction::NewValueInExistingTable
        } else {
            Introduction::NothingNew
        };
        $me.probabilities.introductions.push(introduction);

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

/// Result of a fetch operation in a LinearTable.
#[derive(Clone, Copy, Debug)]
pub enum Fetch<T> {
    /// The value was already in the cache.
    Hit(T),

    /// The value was not in the cache.
    Miss(T),
}
impl<T> Fetch<T> {
    /// Return `true` if this result represents a hit.
    pub fn is_hit(&self) -> bool {
        match *self {
            Fetch::Hit(_) => true,
            _ => false,
        }
    }

    /// Return the slot represented by this fetch.
    pub fn slot(&self) -> &T {
        match *self {
            Fetch::Hit(ref result) => result,
            Fetch::Miss(ref result) => result,
        }
    }
}

/// An index in an LinearTable.
#[derive(Clone, Copy, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum TableRef {
    /// The index represents a value in a shared dictionary.
    Shared(usize),

    /// The index represents a value in a prelude dictionary.
    Prelude(usize),
}
impl TableRef {
    pub fn as_shared(&self) -> Option<usize> {
        match *self {
            TableRef::Shared(result) => Some(result),
            _ => None,
        }
    }
}

/// The initial size of LinearTables, in elements.
const INDEXED_TABLE_INITIAL_CAPACITY: usize = 1024;

/// A data structure designed to cache information accessible
/// either by index or by value.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct LinearTable<T>
where
    T: Eq + std::hash::Hash + Clone,
{
    /// The values in the table, in the order in which they were added.
    /// Used to perform lookup with `at_index`.
    values: Vec<T>,

    /// A mapping to value back to the indices used to access them.
    refs_by_value: HashMap<T, TableRef>,

    /// The number of values representing shared dictionary entries in this table.
    shared_len: usize,
}
impl<T> LinearTable<T>
where
    T: Eq + std::hash::Hash + Clone,
{
    /// The number of values representing shared dictionary entries in this table.
    pub fn shared_len(&self) -> usize {
        self.shared_len
    }

    /// The number of values representing prelude dictionary entries in this table.
    pub fn prelude_len(&self) -> usize {
        self.values.len() - self.shared_len
    }

    /// The number of values in this table.
    pub fn len(&self) -> usize {
        self.values.len()
    }
}

impl<T> LinearTable<T>
where
    T: Eq + std::hash::Hash + Clone + std::fmt::Debug + Ord + crate::entropy::util::ShowMe,
{
    /// Create a new LinearTable from a list of instances.
    ///
    /// Values appearing in `instances` that have `threshold` instances or less are ignored.
    pub fn new(
        value_to_instances: HashMap<T, FilesContaining>,
        threshold: FilesContaining,
    ) -> Self {
        debug!(target: "linear_table", "Creating a LinearTable with a threshold of {}", Into::<usize>::into(threshold));
        let value_to_instances = value_to_instances.into_iter().sorted(); // We sort to enforce traversal order.
        let mut values = Vec::with_capacity(INDEXED_TABLE_INITIAL_CAPACITY);
        let mut refs_by_value = HashMap::with_capacity(INDEXED_TABLE_INITIAL_CAPACITY);
        for (value, instances) in value_to_instances {
            if T::show() {
                debug!(target: "linear_table", "Should we add {:?} to the LinearTable ({} instances)?", value, instances);
            }
            if instances <= threshold {
                // Too few instances, skipping.
                if T::show() {
                    debug!(target: "linear_table", "Too few instances: {} <= {} for {:?}", instances, threshold, value);
                }
                continue;
            } else {
                if T::show() {
                    debug!(target: "linear_table", "Keeping {:?}, with {} instances", value, instances);
                }
            }
            let len = TableRef::Shared(values.len());
            values.push(value.clone());
            let prev = refs_by_value.insert(value, len);
            assert!(prev.is_none());
        }
        let shared_len = values.len();
        let result = LinearTable {
            values,
            refs_by_value,
            shared_len,
        };
        debug!(target: "linear_table", "Dictionary: LinearTable contains {:?}", result);
        result
    }

    /// Create an empty `LinearTable`.
    pub fn with_capacity(len: usize) -> Self {
        LinearTable {
            values: Vec::with_capacity(len),
            refs_by_value: HashMap::with_capacity(len),
            shared_len: 0,
        }
    }

    /// Attempt to get the index for a value from the `LinearTable`.
    ///
    /// If the value is already in the cache, return `Fetch::Hit(index)`, where `index` is the
    /// immutable index of the value. Otherwise, allocate a new slot `index` and return
    /// `Fetch::Miss(index)`.
    ///
    /// In both `Fetch::Hit(index)` and `Fetch::Miss(index)`, `0` is the first value, `1` the second
    /// value, etc.
    pub fn fetch_index(&mut self, value: &T) -> Fetch<TableRef> {
        use std::collections::hash_map::Entry::*;
        let len = self.values.len();
        let index = self.values.len() - self.shared_len;
        let result = match self.refs_by_value.entry(value.clone()) {
            Occupied(slot) => {
                if T::show() {
                    debug!(target: "linear_table", "LinearTable: Found {:?} at {:?}" , value, slot.get());
                }
                return Fetch::Hit(*slot.get());
            }
            Vacant(slot) => {
                let result = TableRef::Prelude(index);
                slot.insert(result.clone());
                if T::show() {
                    debug!(target: "linear_table", "LinearTable: Coulnd't find {:?} among {} values, assigning {}" , value, len, index);
                }
                result
            }
        };
        self.values.push(value.clone());
        Fetch::Miss(result)
    }

    /// Create a copy of the current LinearTable with an added prelude dictionary.
    pub fn with_prelude<'a>(&self, prelude: &'a [T]) -> Result<Self, &'a T> {
        debug!(target: "dictionary", "LinearTable with {} shared, {} prelude", self.shared_len(), prelude.len());
        let mut clone = self.clone();
        for item in prelude {
            if clone.fetch_index(item).is_hit() {
                // The prelude shouldn't duplicate anything from the dictionary.
                return Err(item);
            }
        }
        Ok(clone)
    }

    /// Access the contents of this table by index.
    pub fn at_index(&self, table_ref: &TableRef) -> Option<&T> {
        let index = match *table_ref {
            TableRef::Shared(raw) => raw,
            TableRef::Prelude(raw) => raw + self.shared_len,
        };
        self.values.get(index)
    }
}

/// Options for creating a dictionary.
#[derive(Clone, Debug, Serialize, Deserialize, Default)]
pub struct Options {
    /// Length of AST paths used to establish probabilities.
    depth: usize,

    /// Width of AST paths used to establish probabilities.
    width: usize,
}
impl Options {
    /// Set the length of AST paths used to establish probabilities.
    pub fn with_depth(self, depth: usize) -> Self {
        Options { depth, ..self }
    }

    /// Set the width of windows of references used to establish probabilities.
    pub fn with_width(self, width: usize) -> Self {
        Options { width, ..self }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Dictionary<T> {
    // --- Non-extensible sets of symbols, predicted by path.
    // Used for entropy coding.
    // ---
    /// All booleans appearing in the AST, predicted by path.
    bool_by_path: Rc<RefCell<PathPredict<Option<bool>, T>>>,

    /// All string enumerations, predicted by path.
    string_enum_by_path: Rc<RefCell<PathPredict<Option<SharedString>, T>>>,

    /// All interface names, predicted by path.
    interface_name_by_path: Rc<RefCell<PathPredict<Option<InterfaceName>, T>>>,


    // --- Extensible sets of symbols, predicted by path.
    // Used for experiments with entropy coding, but so far, not very
    // good with extensibility. There are good chances that this section
    // will disappear in future versions.
    // ---
    /// All floats appearing in the AST.
    float_by_path: Rc<RefCell<PathPredict<Option<F64>, T>>>,

    /// All unsigned longs appearing in the AST.
    unsigned_long_by_path: Rc<RefCell<PathPredict<u32, T>>>,

    /// All property keys.
    property_key_by_path: Rc<RefCell<PathPredict<Option<PropertyKey>, T>>>,

    /// All identifier names, predicted by path.
    identifier_name_by_path: Rc<RefCell<PathPredict<Option<IdentifierName>, T>>>,

    /// All string literals, predicted by path.
    string_literal_by_path: Rc<RefCell<PathPredict<Option<SharedString>, T>>>,

    /// All list lengths, predicted by path.
    list_length_by_path: Rc<RefCell<PathPredict<Option<u32>, T>>>,
}
impl<T> Dictionary<T> {
    /// Create a new dictionary using paths of `depth` depth
    /// and windows of `width` width.
    pub fn new(Options { depth, width }: Options) -> Self {
        Dictionary {
            bool_by_path: Rc::new(RefCell::new(PathPredict::new(depth))),
            float_by_path: Rc::new(RefCell::new(PathPredict::new(depth))),
            unsigned_long_by_path: Rc::new(RefCell::new(PathPredict::new(depth))),
            string_enum_by_path: Rc::new(RefCell::new(PathPredict::new(depth))),
            property_key_by_path: Rc::new(RefCell::new(PathPredict::new(depth))),
            identifier_name_by_path: Rc::new(RefCell::new(PathPredict::new(depth))),
            string_literal_by_path: Rc::new(RefCell::new(PathPredict::new(depth))),
            list_length_by_path: Rc::new(RefCell::new(PathPredict::new(depth))),
            interface_name_by_path: Rc::new(RefCell::new(PathPredict::new(depth))),
        }
    }

    // The following methods are read-only accessors and may be used regardless
    // of whether we're producing the dictionary or using it.

    pub fn bool_by_path(&self) -> Ref<PathPredict<Option<bool>, T>> {
        self.bool_by_path.as_ref().borrow()
    }

    pub fn string_enum_by_path(&self) -> Ref<PathPredict<Option<SharedString>, T>> {
        self.string_enum_by_path.as_ref().borrow()
    }

    pub fn interface_name_by_path(&self) -> Ref<PathPredict<Option<InterfaceName>, T>> {
        self.interface_name_by_path.as_ref().borrow()
    }

    pub fn float_by_path(&self) -> Ref<PathPredict<Option<F64>, T>> {
        self.float_by_path.as_ref().borrow()
    }

    pub fn unsigned_long_by_path(&self) -> Ref<PathPredict<u32, T>> {
        self.unsigned_long_by_path.as_ref().borrow()
    }

    pub fn property_key_by_path(&self) -> Ref<PathPredict<Option<PropertyKey>, T>> {
        self.property_key_by_path.as_ref().borrow()
    }

    pub fn identifier_name_by_path(&self) -> Ref<PathPredict<Option<IdentifierName>, T>> {
        self.identifier_name_by_path.as_ref().borrow()
    }

    pub fn string_literal_by_path(&self) -> Ref<PathPredict<Option<SharedString>, T>> {
        self.string_literal_by_path.as_ref().borrow()
    }

    pub fn list_length_by_path(&self) -> Ref<PathPredict<Option<u32>, T>> {
        self.list_length_by_path.as_ref().borrow()
    }

    /// Return the depth of the current dictionary.
    pub fn depth(&self) -> usize {
        assert_eq!(
            self.bool_by_path().depth(),
            self.string_enum_by_path().depth()
        );
        assert_eq!(
            self.bool_by_path().depth(),
            self.interface_name_by_path().depth()
        );
        self.bool_by_path().depth()
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
        } = *self;

        bool_by_path.borrow().len()
            + float_by_path.borrow().len()
            + unsigned_long_by_path.borrow().len()
            + string_enum_by_path.borrow().len()
            + property_key_by_path.borrow().len()
            + identifier_name_by_path.borrow().len()
            + interface_name_by_path.borrow().len()
            + string_literal_by_path.borrow().len()
            + list_length_by_path.borrow().len()
            + interface_name_by_path.borrow().len()
    }
}

impl Dictionary<Instances> {
    pub fn bool_by_path_mut(&mut self) -> RefMut<PathPredict<Option<bool>, Instances>> {
        self.bool_by_path.as_ref().borrow_mut()
    }

    pub fn string_enum_by_path_mut(&mut self) -> RefMut<PathPredict<Option<SharedString>, Instances>> {
        self.string_enum_by_path.as_ref().borrow_mut()
    }

    pub fn interface_name_by_path_mut(&mut self) -> RefMut<PathPredict<Option<InterfaceName>, Instances>> {
        self.interface_name_by_path.as_ref().borrow_mut()
    }

    pub fn float_by_path_mut(&mut self) -> RefMut<PathPredict<Option<F64>, Instances>> {
        self.float_by_path.as_ref().borrow_mut()
    }

    pub fn unsigned_long_by_path_mut(&mut self) -> RefMut<PathPredict<u32, Instances>> {
        self.unsigned_long_by_path.as_ref().borrow_mut()
    }

    pub fn property_key_by_path_mut(
        &mut self,
    ) -> RefMut<PathPredict<Option<PropertyKey>, Instances>> {
        self.property_key_by_path.as_ref().borrow_mut()
    }

    pub fn identifier_name_by_path_mut(
        &mut self,
    ) -> RefMut<PathPredict<Option<IdentifierName>, Instances>> {
        self.identifier_name_by_path.as_ref().borrow_mut()
    }

    pub fn string_literal_by_path_mut(
        &mut self,
    ) -> RefMut<PathPredict<Option<SharedString>, Instances>> {
        self.string_literal_by_path.as_ref().borrow_mut()
    }

    pub fn list_length_by_path_mut(&mut self) -> RefMut<PathPredict<Option<u32>, Instances>> {
        self.list_length_by_path.as_ref().borrow_mut()
    }


    /// Combine a dictionary obtained by sampling (`self`) and a baseline dictionary
    /// (obtained by `entropy::baseline`) to produce a dictionary able to handle
    /// values that grammatically correct but have not been witnessed during
    /// sampling.
    pub fn with_grammar_fallback(&self, fallback: Dictionary<Instances>) -> Self {
        let result = self.clone();
        let original_len = result.len();
        result
            .bool_by_path
            .borrow_mut()
            .add_fallback(&fallback.bool_by_path());
        result
            .string_enum_by_path
            .borrow_mut()
            .add_fallback(&fallback.string_enum_by_path());
        result
            .interface_name_by_path
            .borrow_mut()
            .add_fallback(&fallback.interface_name_by_path());

        debug!(target: "dictionary", "Added fallback to dictionary {} states => {} states",
            original_len,
            result.len());

        result
    }
}

impl InstancesToProbabilities for Dictionary<Instances> {
    type AsProbabilities = Dictionary<SymbolInfo>;

    /// Convert a dictionary counting instances into a dictionary that
    /// counts probabilities.
    fn instances_to_probabilities(&self, _description: &str) -> Dictionary<SymbolInfo> {
        Dictionary {
            // By path.
            bool_by_path: Rc::new(RefCell::new(
                self.bool_by_path()
                    .instances_to_probabilities("bool_by_path"),
            )),
            float_by_path: Rc::new(RefCell::new(
                self.float_by_path()
                    .instances_to_probabilities("float_by_path"),
            )),
            unsigned_long_by_path: Rc::new(RefCell::new(
                self.unsigned_long_by_path()
                    .instances_to_probabilities("unsigned_long_by_path"),
            )),
            string_enum_by_path: Rc::new(RefCell::new(
                self.string_enum_by_path()
                    .instances_to_probabilities("string_enum_by_path"),
            )),
            property_key_by_path: Rc::new(RefCell::new(
                self.property_key_by_path()
                    .instances_to_probabilities("property_key_by_path"),
            )),
            identifier_name_by_path: Rc::new(RefCell::new(
                self.identifier_name_by_path()
                    .instances_to_probabilities("identifier_name_by_path"),
            )),
            interface_name_by_path: Rc::new(RefCell::new(
                self.interface_name_by_path()
                    .instances_to_probabilities("interface_name_by_path"),
            )),
            string_literal_by_path: Rc::new(RefCell::new(
                self.string_literal_by_path()
                    .instances_to_probabilities("string_literal_by_path"),
            )),
            list_length_by_path: Rc::new(RefCell::new(
                self.list_length_by_path()
                    .instances_to_probabilities("list_length_by_path"),
            )),
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
pub struct UserExtensibleMaps<T> {
    /// Instances of IdentifierName.
    pub identifier_names: HashMap<Option<IdentifierName>, T>,

    /// Instances of PropertyKey
    pub property_keys: HashMap<Option<PropertyKey>, T>,

    /// Instances of InterfaceName
    pub interface_names: HashMap<Option<InterfaceName>, T>,

    /// Instances of string literals.
    pub string_literals: HashMap<Option<SharedString>, T>,

    /// Instances of string enums.
    pub string_enums: HashMap<Option<SharedString>, T>,

    /// Instances of list lengths.
    pub list_lengths: HashMap<Option<u32>, T>,

    /// Instances of floating-point numbers.
    pub floats: HashMap<Option<F64>, T>,

    /// Instances of unsigned longs.
    pub unsigned_longs: HashMap<u32, T>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct UserExtensibleTables {
    /// Instances of IdentifierName.
    pub identifier_names: LinearTable<Option<IdentifierName>>,

    /// Instances of PropertyKey
    pub property_keys: LinearTable<Option<PropertyKey>>,

    /// Instances of InterfaceName
    pub interface_names: LinearTable<Option<InterfaceName>>,

    /// Instances of string literals.
    pub string_literals: LinearTable<Option<SharedString>>,

    /// Instances of string enums.
    pub string_enums: LinearTable<Option<SharedString>>,

    /// Instances of list lengths.
    pub list_lengths: LinearTable<Option<u32>>,

    /// Instances of floating-point numbers.
    pub floats: LinearTable<Option<F64>>,

    /// Instances of unsigned longs.
    pub unsigned_longs: LinearTable<u32>,
}

macro_rules! for_field_in_user_extensible_data {
    ( $cb: ident ) => {
        $cb!(
          (identifier_names, "identifier_names", b"identifier_names"),
          (property_keys, "property_keys", b"property_keys"),
          (interface_names, "interface_names", b"interface_names"),
          (string_literals, "string_literals", b"string_literals"),
          (string_enums, "string_enums", b"string_enums"),
          (list_lengths, "list_lengths", b"list_lengths"),
          (floats, "floats", b"floats"),
          (unsigned_longs, "unsigned_longs", b"unsigned_longs")
        )
    };
}

impl<T> UserExtensibleMaps<T> {
    pub fn len(&self) -> usize {
        let mut len = 0;
        macro_rules! with_field { ($(($ident: ident, $name: expr, $bname: expr )),*) => {
            $(
                len += self.$ident.len();
            )*
        } };
        for_field_in_user_extensible_data!(with_field);
        len
    }
}

impl<K> InstancesToProbabilities for HashMap<K, FilesContaining>
where
    K: Eq + std::hash::Hash + Clone,
{
    type AsProbabilities = HashMap<K, SymbolInfo>;

    fn instances_to_probabilities(&self, _description: &str) -> HashMap<K, SymbolInfo> {
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
                    (*key).clone(),
                    SymbolInfo {
                        index: SymbolIndex::from(index),
                        distribution: distribution.clone(),
                    },
                )
            })
            .collect()
    }
}

/// A family of dictionaries.
///
/// By convention:
/// - key `""` maps to the starting dictionary;
/// - key `"*"` maps to the fallback dictionary, in which
///   all probabilities are equal.
#[derive(Clone, Serialize, Deserialize)]
pub struct DictionaryFamily<T> {
    introductions: Vec<Introduction>,

    probabilities: HashMap<SharedString, Dictionary<T>>,

    /// The stack of dictionaries.
    ///
    /// Note that we clone dictionaries on top of the stack. This assumes that dictionary
    /// cloning is cheap enough that this is a reasonable thing to do.
    probabilities_stack: Vec<(SharedString, Dictionary<T>)>,
}
impl<T> DictionaryFamily<T> {
    pub fn introductions(&self) -> &[Introduction] {
        &self.introductions
    }

    /// Exit the current dictionary, returning to the parent dictionary.
    ///
    /// # Failure
    ///
    /// Fails if `name` is not the name of the current dictionary. Used for assertion
    /// purposes.
    pub fn exit(&mut self, name: &SharedString) {
        assert!(self.probabilities_stack.len() > 1);
        let leaving = self.probabilities_stack.pop().unwrap();
        assert_eq!(&leaving.0, name);
    }

    pub fn depth(&self) -> usize {
        let dictionary = self.probabilities
            .values()
            .next()
            .unwrap();
        let depth = dictionary.depth();
        debug_assert!(self.probabilities.values().position(|dict| dict.depth() != depth).is_none());
        depth
    }

    /// Access the current dictionary, immutably.
    pub fn current(&self) -> &Dictionary<T> {
        &self
            .probabilities_stack
            .last()
            .expect("Cannot call `DictionaryFamily::current`, as there's no current dictionary.")
            .1
    }

    pub fn name(&self) -> &SharedString {
        &self.probabilities_stack.last().expect("Cannot call `DictionaryFamily::name` as there's no current dictionary").0
    }
}

impl DictionaryFamily<Instances> {
    /// Create an empty DictionaryFamily.
    ///
    /// Initially, this `DictionaryFamily` does not have a current
    /// dictionary. Before the first call to `Self::current` or `Self::current_mut`,
    /// clients MUST call either `enter_existing`or `enter_or_create` to enter a
    /// dictionary.
    pub fn new() -> Self {
        DictionaryFamily {
            probabilities: HashMap::new(),
            probabilities_stack: vec![],
            introductions: vec![],
        }
    }

    /// Push a dictionary on top of the stack.
    /// Create the dictionary if no such dictionary exists.
    pub fn enter_or_create(&mut self, name: &SharedString, options: Options) {
        let dictionary = self
            .probabilities
            .entry(name.clone())
            .or_insert_with(|| Dictionary::new(options));
        self.probabilities_stack
            .push((name.clone(), dictionary.clone()));
    }

    /// Access the current dictionary, mutably.
    pub fn current_mut(&mut self) -> &mut Dictionary<Instances> {
        &mut self
            .probabilities_stack
            .last_mut()
            .expect("Cannot call `DictionaryFamily::current`, as there's no current dictionary.")
            .1
    }

    /// Iterate mutably through all dictionaries in this family.
    pub fn values_mut(&mut self) -> impl Iterator<Item = &mut Dictionary<Instances>> {
        self.probabilities.values_mut()
    }

    /// Manually insert a dictionary in the family.
    ///
    /// Returns `true` if there was already a baseline dictionary, `false` otherwise.
    pub fn insert_baseline(&mut self, dictionary: Dictionary<Instances>) -> bool {
        // If there is no main dictionary, add one.
        self.probabilities
            .entry(SharedString::from_str(""))
            .or_insert_with(|| dictionary.clone());

        // Insert a dictionary for special key `*`.
        self.probabilities
            .insert(SharedString::from_str("*"), dictionary)
            .is_some()
    }
}

impl DictionaryFamily<SymbolInfo> {
    pub fn enter_existing(&mut self, name: &SharedString) -> Result<(), ()> {
        let dictionary = self.probabilities.get(name).ok_or(())?;
        self.probabilities_stack
            .push((name.clone(), dictionary.clone()));
        Ok(())
    }
}

impl InstancesToProbabilities for DictionaryFamily<Instances> {
    type AsProbabilities = DictionaryFamily<SymbolInfo>;
    fn instances_to_probabilities(&self, description: &str) -> DictionaryFamily<SymbolInfo> {
        assert!(self.probabilities_stack.len() == 1 || self.probabilities_stack.len() == 0); // We only want the toplevel dictionary.
        DictionaryFamily {
            probabilities: self
                .probabilities
                .iter()
                .map(|(name, dict)| (name.clone(), dict.instances_to_probabilities(description)))
                .collect(),
            probabilities_stack: vec![],
            introductions: self.introductions.clone(),
        }
    }
}

#[derive(Clone, Debug, Copy, Serialize, Deserialize)]
pub enum Introduction {
    NothingNew,
    NewValueInExistingTable,
    NewTable,
}

pub struct ValueCollector {
    /// Number of instances of each string in the current file.
    instances_of_user_extensible_data_in_current_file: UserExtensibleMaps<InstancesInFile>,

    /// Number of files in which each string appears.
    files_containing_user_extensible_data: UserExtensibleMaps<FilesContaining>,

    options: Options,
}
impl ValueCollector {
    pub fn new(options: Options) -> Self {
        ValueCollector {
            instances_of_user_extensible_data_in_current_file: UserExtensibleMaps::default(),
            files_containing_user_extensible_data: UserExtensibleMaps::default(),
            options
        }
    }

    pub fn files_containing(&self) -> &UserExtensibleMaps<FilesContaining> {
        &self.files_containing_user_extensible_data
    }

    pub fn into_tables(self, threshold: FilesContaining) -> UserExtensibleTables {
        macro_rules! with_field { ($(($ident: ident, $name: expr, $bname: expr )),*) => {
            UserExtensibleTables {
                $(
                    $ident: LinearTable::new(self.files_containing_user_extensible_data.$ident, threshold),
                )*
            }
        }};
        for_field_in_user_extensible_data!(with_field)
    }
}

impl<'a> TokenWriter for &'a mut ValueCollector {
    type Data = [u8; 0]; // We're not interested in data.
    fn done(self) -> Result<Self::Data, TokenWriterError> {
        // We're done with the file. Transfer data from `instances_of_user_extensible_data_in_current_file`
        // into `files_containing_user_extensible_data`, which is what we're interested in.
        macro_rules! with_field { ($(($ident: ident, $name: expr, $bname: expr )),*) => {
            $(
                for (value, _) in self.instances_of_user_extensible_data_in_current_file.$ident.drain() {
                    *self.files_containing_user_extensible_data.$ident.entry(value)
                        .or_insert(FilesContaining(0)) += FilesContaining(1);
                }
            )*
        } };
        for_field_in_user_extensible_data!(with_field);

        Ok([])
    }

    fn enter_tagged_tuple_at(
        &mut self,
        _node: &Node,
        tag: &InterfaceName,
        _children: &[&FieldName],
        _path: &IOPath,
    ) -> Result<(), TokenWriterError> {
        increment_instance_count!(self, interface_names, Some(tag.clone()));
        Ok(())
    }

    fn string_at(&mut self, value: Option<&SharedString>, _path: &IOPath) -> Result<(), TokenWriterError>
    {
        increment_instance_count!(self, string_literals, value.cloned());
        Ok(())
    }

    fn string_enum_at(&mut self, value: &SharedString, _path: &IOPath) -> Result<(), TokenWriterError>
    {
        increment_instance_count!(self, string_enums, Some(value.clone()));
        Ok(())
    }

    fn float_at(&mut self, value: Option<f64>, _path: &IOPath) -> Result<(), TokenWriterError>
    {
        let value = value.map(|x| x.into());
        increment_instance_count!(self, floats, value);
        Ok(())
    }

    fn unsigned_long_at(&mut self, value: u32, _path: &IOPath) -> Result<(), TokenWriterError>
    {
        increment_instance_count!(self, unsigned_longs, value);
        Ok(())
    }

    fn bool_at(&mut self, value: Option<bool>, _path: &IOPath) -> Result<(), TokenWriterError>
    {
        // We don't count bools.
        Ok(())
    }

    fn offset_at(&mut self, _path: &IOPath) -> Result<(), TokenWriterError>
    {
        // We don't count offsets.
        Ok(())
    }

    fn enter_list_at(&mut self, len: usize, _path: &IOPath) -> Result<(), TokenWriterError> {
        increment_instance_count!(self, list_lengths, Some(len as u32));
        Ok(())
    }
}
pub struct ProbabilityTableCollector {
    /// The family of probabilities being constructed.
    probabilities: DictionaryFamily<Instances>,

    options: Options,
}
impl ProbabilityTableCollector {
    pub fn new(options: Options) -> Self {
        let mut probabilities = DictionaryFamily::new();
        probabilities.enter_or_create(&SharedString::from_str(""), options.clone());
        ProbabilityTableCollector {
            probabilities,
            options,
        }
    }
}
impl<'a> TokenWriter for &'a mut ProbabilityTableCollector {
    type Data = [u8; 0]; // Placeholder

    fn done(self) -> Result<Self::Data, TokenWriterError> {
        Ok([])
    }

    // --- Fixed sets of values

    fn bool_at(&mut self, value: Option<bool>, path: &IOPath) -> Result<(), TokenWriterError> {
        update_in_context!(self, bool_by_path, "bool_by_path", path, value)?;
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
            Some(value.clone())
        )
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
            Some(tag.clone())
        )
    }

    // --- User extensible values

    fn float_at(&mut self, value: Option<f64>, path: &IOPath) -> Result<(), TokenWriterError> {
        let value = value.map(|x| x.into());
        update_in_context!(self, float_by_path, "float_by_path", path, value)
    }

    fn unsigned_long_at(&mut self, value: u32, path: &IOPath) -> Result<(), TokenWriterError> {
        update_in_context!(
            self,
            unsigned_long_by_path,
            "unsigned_long_by_path",
            path,
            value
        )
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
        )
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
        )
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
        )
    }

    fn enter_list_at(&mut self, len: usize, path: &IOPath) -> Result<(), TokenWriterError> {
        update_in_context!(
            self,
            list_length_by_path,
            "list_length_by_path",
            path,
            Some(len as u32)
        )
    }


    fn offset_at(&mut self, _path: &IOPath) -> Result<(), TokenWriterError> {
        Ok(())
    }

    fn enter_scoped_dictionary_at(
        &mut self,
        name: &SharedString,
        _path: &IOPath,
    ) -> Result<(), TokenWriterError> {
        self.probabilities
            .enter_or_create(name, self.options.clone());
        Ok(())
    }

    fn exit_scoped_dictionary_at(
        &mut self,
        name: &SharedString,
        _path: &IOPath,
    ) -> Result<(), TokenWriterError> {
        self.probabilities.exit(name);
        Ok(())
    }
}

/// A structure used to build a dictionary based on a sample of files.
pub struct DictionaryBuilder {
    /// The family of dictionaries being constructed.
    dictionaries: DictionaryFamily<Instances>,

    /// Number of instances of each string in the current file.
    instances_of_user_extensible_data_in_current_file: UserExtensibleMaps<InstancesInFile>,

    /// Number of files in which each string appears.
    files_containing_user_extensible_data: UserExtensibleMaps<FilesContaining>,

    /// Options used to create new dictionaries.
    options: Options,
}
