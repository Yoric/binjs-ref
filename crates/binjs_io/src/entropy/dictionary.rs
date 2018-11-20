use entropy::predict::{ PathPredict, WindowPredict };
use entropy::probabilities::{ InstancesToProbabilities, SymbolIndex, SymbolInfo };

use io::TokenWriter;
use ::TokenWriterError;

use binjs_shared::{ F64, FieldName, IdentifierName, InterfaceName, PropertyKey, SharedString };

use std;
use std::collections::HashMap;

pub type IOPath = binjs_shared::ast::Path<InterfaceName, (/* child index */ usize, /* field name */ FieldName)>;

pub use entropy::predict::Instances;

/// A newtype for `usize` used to count the number of some item in a given file.
#[derive(Default, Serialize, Deserialize, From, Into, AddAssign, Clone, Copy, Debug, PartialOrd, Ord, PartialEq, Eq)]
pub struct InstancesInFile(pub usize);

/// A newtype for `usize` used to count the number of files containing some item.
#[derive(Default, Serialize, Deserialize, From, Into, AddAssign, Clone, Copy, Debug, PartialOrd, Ord, PartialEq, Eq)]
pub struct FilesContaining(pub usize);

/// Add a single symbol to the table.
///
/// Used instead of a method as we need generality wrt the field name.
///
/// Usage:
/// `symbol!(self, name_of_the_probability_table, "Description, used for debugging", value_to_encode, path_in_the_ast)`
macro_rules! symbol {
    ( $me: ident, $table: ident, $description: expr, $path:expr, $value: expr ) => {
        {
            use std::borrow::Borrow;

            let path = $path.borrow();
            $me.dictionary
                .$table
                .add(path, $value);

            Ok(())
        }
    }
}

macro_rules! window {
    ( $me: ident, $table:ident, $value: expr ) => {
        {
            $me.dictionary
                .$table
                .add($value);

                Ok(())
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Dictionary<T> {
    /// All booleans appearing in the AST, predicted by path.
    pub bool_by_path: PathPredict<Option<bool>, T>,

    /// All floats appearing in the AST, predicted by path.
    pub float_by_path: PathPredict<Option<F64>, T>,

    /// All unsigned longs appearing in the AST, predicted by path.
    pub unsigned_long_by_path: PathPredict<u32, T>,

    /// All string enumerations, predicted by path.
    pub string_enum_by_path: PathPredict<SharedString, T>,

    /// All property keys, predicted by path.
    pub property_key_by_path: PathPredict<Option<PropertyKey>, T>,

    /// All property keys, predicted by window.
    pub property_key_by_window: WindowPredict<Option<PropertyKey>, T>,

    /// All identifier names, predicted by path.
    pub identifier_name_by_path: PathPredict<Option<IdentifierName>, T>,

    /// All identifier names, predicted by window.
    pub identifier_name_by_window: WindowPredict<Option<IdentifierName>, T>,

    /// All interface names, predicted by path.
    pub interface_name_by_path: PathPredict<InterfaceName, T>,

    /// All string literals, predicted by path.
    pub string_literal_by_path: PathPredict<Option<SharedString>, T>,

    /// All string literals, predicted by window.
    pub string_literal_by_window: WindowPredict<Option<SharedString>, T>,

    /// All list lengths, predicted by path.
    pub list_length_by_path: PathPredict<Option<u32>, T>,

    // Missing:
    // - offsets (cannot be predicted?)
    // - directives?
}
impl<T> Dictionary<T> {
    pub fn new(depth: usize, width: usize) -> Self {
        Dictionary {
            bool_by_path: PathPredict::new(depth),
            float_by_path: PathPredict::new(depth),
            unsigned_long_by_path: PathPredict::new(depth),
            string_enum_by_path: PathPredict::new(depth),
            property_key_by_path: PathPredict::new(depth),
            property_key_by_window: WindowPredict::new(width),
            identifier_name_by_path: PathPredict::new(depth),
            identifier_name_by_window: WindowPredict::new(width),
            string_literal_by_path: PathPredict::new(depth),
            string_literal_by_window: WindowPredict::new(width),
            list_length_by_path: PathPredict::new(depth),
            interface_name_by_path: PathPredict::new(depth),
        }
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
impl InstancesToProbabilities for Dictionary<Instances> {
    type AsProbabilities = Dictionary<SymbolInfo>;

    /// Convert a dictionary counting instances into a dictionary that
    /// counting probabilities.
    fn instances_to_probabilities(self, _description: &str) -> Dictionary<SymbolInfo> {
        Dictionary {
            bool_by_path: self.bool_by_path.instances_to_probabilities("bool_by_path"),
            float_by_path: self.float_by_path.instances_to_probabilities("float_by_path"),
            unsigned_long_by_path: self.unsigned_long_by_path.instances_to_probabilities("unsigned_long_by_path"),
            string_enum_by_path: self.string_enum_by_path.instances_to_probabilities("string_enum_by_path"),
            property_key_by_path: self.property_key_by_path.instances_to_probabilities("property_key_by_path"),
            property_key_by_window: self.property_key_by_window.instances_to_probabilities("property_key_by_window"),
            identifier_name_by_path: self.identifier_name_by_path.instances_to_probabilities("identifier_name_by_path"),
            identifier_name_by_window: self.identifier_name_by_window.instances_to_probabilities("identifier_name_by_window"),
            interface_name_by_path: self.interface_name_by_path.instances_to_probabilities("interface_name_by_path"),
            string_literal_by_path: self.string_literal_by_path.instances_to_probabilities("string_literal_by_path"),
            string_literal_by_window: self.string_literal_by_window.instances_to_probabilities("string_literal_by_window"),
            list_length_by_path: self.list_length_by_path.instances_to_probabilities("list_length_by_path"),
        }
    }
}

/// Maps from the various kinds of strings in the AST to T.
///
/// This container is used to collect statistics, such as the number
/// of instances of a given string in a file, or the number of files
/// that contain a given string.
#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub struct KindedStringMap<T> {
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
}
impl<T> KindedStringMap<T> {
    pub fn len(&self) -> usize {
        // Make sure that we don't forget a field.
        let KindedStringMap {
            ref identifier_name_instances,
            ref property_key_instances,
            ref interface_name_instances,
            ref string_literal_instances,
            ref string_enum_instances,
        } = *self;
        identifier_name_instances.len()
        + property_key_instances.len()
        + string_literal_instances.len()
        + string_enum_instances.len()
        + interface_name_instances.len()
    }
}

impl InstancesToProbabilities for KindedStringMap<FilesContaining> {
    type AsProbabilities = KindedStringMap<SymbolInfo>;

    /// Convert a dictionary counting instances into a dictionary
    /// counting probabilities.
    fn instances_to_probabilities(self, _description:&str) -> KindedStringMap<SymbolInfo> {
        KindedStringMap {
            identifier_name_instances: self.identifier_name_instances.instances_to_probabilities("identifier_name_instances"),
            property_key_instances: self.property_key_instances.instances_to_probabilities("property_key_instances"),
            interface_name_instances: self.interface_name_instances.instances_to_probabilities("interface_name_instances"),
            string_literal_instances: self.string_literal_instances.instances_to_probabilities("string_literal_instances"),
            string_enum_instances: self.string_enum_instances.instances_to_probabilities("string_enum_instances"),
        }
    }
}

impl<K> InstancesToProbabilities for HashMap<K, FilesContaining>
    where K: Eq + std::hash::Hash
{
    type AsProbabilities = HashMap<K, SymbolInfo>;

    fn instances_to_probabilities(self, _description: &str) -> HashMap<K, SymbolInfo> {
        use std::cell::RefCell;
        use std::rc::Rc;

        let instances = self.values()
            .map(|x| {
                let x: usize = x.clone().into();
                x as u32
            })
            .collect();
        let distribution = Rc::new(RefCell::new(range_encoding::CumulativeDistributionFrequency::new(instances)));

        self.into_iter()
            .enumerate()
            .map(|(index, (key, _))| {
                (key, SymbolInfo {
                    index: SymbolIndex::from(index),
                    distribution: distribution.clone()
                })
            })
            .collect()
    }
}

/// A structure used to build a dictionary based on a sample of files.
///
/// In this version, we use several dictionary lengths, to determine the
/// best version for a file.
pub struct DictionaryBuilder<'a> {
    /// A dictionary.
    ///
    /// This is a shared reference as we typically wish to
    /// access this field after the DictionaryBuilder
    /// has been consumed and released by a `Serializer`.
    dictionary: &'a mut Dictionary<Instances>,

    /// Number of instances of each string in the current file.
    instances_of_strings_in_current_file: KindedStringMap<InstancesInFile>,

    /// Number of files in which each string appears.
    files_containing_string: &'a mut KindedStringMap<FilesContaining>,
}

impl<'a> DictionaryBuilder<'a> {
    pub fn new(dictionary: &'a mut Dictionary<Instances>, files_containing_string: &'a mut KindedStringMap<FilesContaining>) -> Self {
        DictionaryBuilder {
            dictionary,
            instances_of_strings_in_current_file: KindedStringMap::default(),
            files_containing_string
        }
    }

    /// Count the string `value` as used in the current file.
    fn add_instance_to_strings<V>(value: V, bucket: &mut HashMap<V, InstancesInFile>)
        where
            V: std::hash::Hash + Eq + Clone + std::fmt::Debug
    {
        bucket.entry(value)
            .and_modify(|instances| {
                *instances += InstancesInFile(1) // We have already seen this string in this file, increment.
            }).or_insert(InstancesInFile(1));    // First time we see this string in this file, store 1.
    }

    /// Take all strings of a given nature present in a file (as stored
    /// in `self.instances_of_strings_in_current_file`) and mark them as
    /// appearing in one more file (as stored in `self.files_containing_string`).
    ///
    /// The caller is responsible for making sure that `source` is a
    /// `self.instances_of_strings_in_current_file.XXX` and `destination`
    /// is the corresponding `self.files_containing_string.XXX`.
    ///
    /// Note: This is a function rather than a method because making it a method
    /// would require us to borrow mutably `source` *and* while calling into `self`.
    /// Not very borrow-checker-compatible.
    fn transfer_instances_of_strings<V>(source: &mut HashMap<V, InstancesInFile>, destination: &mut HashMap<V, FilesContaining>)
        where
            V: std::hash::Hash + Eq + Clone + std::fmt::Debug
    {
        for (k, _) in source.drain() {
            // Increase the number of files in `destination` that contain `k` by 1,
            // ignoring the number of instances of `k` in `source`.
            destination.entry(k)
                .and_modify(|instances| {
                    *instances += FilesContaining(1)
                }).or_insert(FilesContaining(1));
        }
    }

    fn done_with_file(&mut self) {
        // Count the number of files in which string instances appear.
        Self::transfer_instances_of_strings(
                &mut self.instances_of_strings_in_current_file.identifier_name_instances,
                &mut self.files_containing_string.identifier_name_instances
        );
        Self::transfer_instances_of_strings(
                &mut self.instances_of_strings_in_current_file.property_key_instances,
                &mut self.files_containing_string.property_key_instances
        );
        Self::transfer_instances_of_strings(
                &mut self.instances_of_strings_in_current_file.interface_name_instances,
                &mut self.files_containing_string.interface_name_instances
        );
        Self::transfer_instances_of_strings(
                &mut self.instances_of_strings_in_current_file.string_literal_instances,
                &mut self.files_containing_string.string_literal_instances
        );
        Self::transfer_instances_of_strings(
                &mut self.instances_of_strings_in_current_file.string_enum_instances,
                &mut self.files_containing_string.string_enum_instances
        );
    }
}

impl<'a> TokenWriter for DictionaryBuilder<'a> {
    type Data = [u8;0]; // Placeholder

    fn done(mut self) -> Result<Self::Data, TokenWriterError> {
        self.done_with_file();
        debug!(target: "entropy", "Built a dictionary with len: {}", self.dictionary.len());
        Ok([])
    }

    fn bool_at(&mut self, value: Option<bool>, path: &IOPath) -> Result<(), TokenWriterError> {
        symbol!(self, bool_by_path, "bool_by_path", path, value)?;
        Ok(())
    }

    fn float_at(&mut self, value: Option<f64>, path: &IOPath) -> Result<(), TokenWriterError> {
        let value = value.map(|x| x.into());
        symbol!(self, float_by_path, "float_by_path", path, value)?;
        Ok(())
    }

    fn unsigned_long_at(&mut self, value: u32, path: &IOPath) -> Result<(), TokenWriterError> {
        symbol!(self, unsigned_long_by_path, "unsigned_long_by_path", path, value)?;
        Ok(())
    }

    fn string_enum_at(&mut self, value: &SharedString, path: &IOPath) -> Result<(), TokenWriterError> {
        symbol!(self, string_enum_by_path, "string_enum_by_path", path, value.clone())?;
        Self::add_instance_to_strings(value.clone(), &mut self.instances_of_strings_in_current_file.string_enum_instances);
        Ok(())
    }

    fn string_at(&mut self, value: Option<&SharedString>, path: &IOPath) -> Result<(), TokenWriterError> {
        symbol!(self, string_literal_by_path, "string_literal_by_path", path, value.cloned())?;
        window!(self, string_literal_by_window, value.cloned())?;
        Self::add_instance_to_strings(value.cloned(), &mut self.instances_of_strings_in_current_file.string_literal_instances);
        Ok(())
    }

    fn property_key_at(&mut self, value: Option<&PropertyKey>, path: &IOPath) -> Result<(), TokenWriterError> {
        symbol!(self, property_key_by_path, "property_key_by_path", path, value.cloned())?;
        window!(self, property_key_by_window, value.cloned())?;
        Self::add_instance_to_strings(value.cloned(), &mut self.instances_of_strings_in_current_file.property_key_instances);
        Ok(())
    }

    fn identifier_name_at(&mut self, value: Option<&IdentifierName>, path: &IOPath) -> Result<(), TokenWriterError> {
        symbol!(self, identifier_name_by_path, "identifier_name_by_path", path, value.cloned())?;
        window!(self, identifier_name_by_window, value.cloned())?;
        Self::add_instance_to_strings(value.cloned(), &mut self.instances_of_strings_in_current_file.identifier_name_instances);
        Ok(())
    }

    fn enter_list_at(&mut self, len: usize, path: &IOPath) -> Result<(), TokenWriterError> {
        symbol!(self, list_length_by_path, "list_length_by_path", path, Some(len as u32))?;
        Ok(())
    }

    fn enter_tagged_tuple_at(&mut self, tag: &InterfaceName, _children: &[&FieldName], path: &IOPath)  -> Result<(), TokenWriterError> {
        symbol!(self, interface_name_by_path, "interface_name_by_path", path, tag.clone())?;
        Self::add_instance_to_strings(tag.clone(), &mut self.instances_of_strings_in_current_file.interface_name_instances);
        Ok(())
    }

    fn offset_at(&mut self, _path: &IOPath) -> Result<(), TokenWriterError> {
        Ok(())
    }
}
