//! Minimal implementation of encoding/decoding to binary.
//! Used for testing purposes. Not meant to be
//! included in release builds.

use bytes;
use io::*;
use util::{PoisonLock, Pos, ReadConst};
use {TokenReaderError, TokenWriterError};

use binjs_shared::{FieldName, InterfaceName, SharedString};

use std;
use std::cell::RefCell;
use std::io::{Read, Seek};
use std::rc::Rc;

use clap;

/// The state of the `TreeTokenReader`.
///
/// Use a `PoisonLock` to access this state.
struct ReaderState<R>
where
    R: Read + Seek,
{
    reader: R,
}
impl<R> ReaderState<R>
where
    R: Read + Seek,
{
    pub fn read_u32(&mut self) -> Result<u32, TokenReaderError> {
        let mut buf: [u8; 4] = [0, 0, 0, 0];
        debug_assert!(std::mem::size_of::<u32>() == std::mem::size_of_val(&buf));
        self.reader
            .read(&mut buf)
            .map_err(TokenReaderError::ReadError)?;

        let result =
            buf[0] as u32 | (buf[1] as u32) << 8 | (buf[2] as u32) << 16 | (buf[3] as u32) << 24;
        Ok(result)
    }

    fn read_string(&mut self) -> Result<String, TokenReaderError> {
        let mut bytes = Vec::new();
        let mut buf: [u8; 1] = [0];
        loop {
            self.reader
                .read(&mut buf)
                .map_err(TokenReaderError::ReadError)?;
            if buf[0] == 0 {
                return String::from_utf8(bytes).map_err(TokenReaderError::Encoding);
            }
            bytes.push(buf[0])
        }
    }
}

impl<R> Pos for PoisonLock<ReaderState<R>>
where
    R: Read + Seek,
{
    fn pos(&mut self) -> usize {
        let result: Result<usize, std::io::Error> = self.try(|state| Ok(state.reader.pos()));
        result.unwrap() // closure cannot fail.
    }
    fn size(&mut self) -> usize {
        let result: Result<usize, std::io::Error> = self.try(|state| Ok(state.reader.size()));
        result.unwrap() // closure cannot fail.
    }
}

pub struct TreeTokenReader<R>
where
    R: Read + Seek,
{
    owner: Rc<RefCell<PoisonLock<ReaderState<R>>>>,
}

impl<R> TreeTokenReader<R>
where
    R: Read + Seek,
{
    pub fn new(reader: R) -> Self {
        let owner = ReaderState { reader };
        TreeTokenReader {
            owner: Rc::new(RefCell::new(PoisonLock::new(owner))),
        }
    }
}

impl<R> FileStructurePrinter for TreeTokenReader<R> where R: Read + Seek {}

impl<R> TokenReader for TreeTokenReader<R>
where
    R: Read + Seek,
{
    fn poison(&mut self) {
        self.owner.borrow_mut().poison();
    }

    fn bool_at(&mut self, _path: &Path) -> Result<Option<bool>, TokenReaderError> {
        debug!(target: "simple_reader", "bool");
        let mut buf: [u8; 1] = [0];
        let mut owner = self.owner.borrow_mut();
        owner.try(|state| {
            state
                .reader
                .read(&mut buf)
                .map_err(TokenReaderError::ReadError)?;
            match bytes::bool::bool_of_bytes(&buf) {
                Ok(x) => Ok(x),
                Err(_) => Err(TokenReaderError::invalid_value(&"bool")),
            }
        })
    }

    fn offset_at(&mut self, _path: &Path) -> Result<u32, TokenReaderError> {
        debug!(target: "simple_reader", "offset");
        let mut owner = self.owner.borrow_mut();
        owner.try(|state| state.read_u32())
    }

    fn float_at(&mut self, _path: &Path) -> Result<Option<f64>, TokenReaderError> {
        debug!(target: "simple_reader", "float");
        let mut owner = self.owner.borrow_mut();
        owner.try(|state| {
            let mut buf: [u8; 8] = [0, 0, 0, 0, 0, 0, 0, 0];
            state
                .reader
                .read(&mut buf)
                .map_err(TokenReaderError::ReadError)?;
            Ok(bytes::float::float_of_bytes(&buf))
        })
    }

    fn unsigned_long_at(&mut self, _path: &Path) -> Result<u32, TokenReaderError> {
        debug!(target: "simple_reader", "unsigned_long");
        let mut owner = self.owner.borrow_mut();
        owner.try(|state| {
            let result = state.read_u32()?;
            Ok(result)
        })
    }

    fn string_at(&mut self, _path: &Path) -> Result<Option<SharedString>, TokenReaderError> {
        debug!(target: "simple_reader", "string");
        let mut owner = self.owner.borrow_mut();
        owner.try(|state| {
            state
                .reader
                .read_const(b"<string>")
                .map_err(TokenReaderError::ReadError)?;
            let byte_len = state.read_u32()?;

            let mut bytes: Vec<u8> = vec![0 as u8; byte_len as usize];
            state
                .reader
                .read(&mut bytes)
                .map_err(TokenReaderError::ReadError)?;

            state
                .reader
                .read_const(b"</string>")
                .map_err(TokenReaderError::ReadError)?;

            if byte_len == 2 && bytes[0] == 255 && bytes[1] == 0 {
                return Ok(None);
            }
            match String::from_utf8(bytes) {
                Ok(x) => Ok(Some(SharedString::from_string(x))),
                Err(err) => Err(TokenReaderError::Encoding(err)),
            }
        })
    }

    fn enter_list_at(&mut self, _path: &Path) -> Result<u32, TokenReaderError> {
        debug!(target: "simple_reader", "list");
        self.owner.borrow_mut().try(|state| {
            state
                .reader
                .read_const(b"<list>")
                .map_err(TokenReaderError::ReadError)?;

            let list_len = state.read_u32()?;
            debug!(target: "simple_writer", "TreeTokenReader: list has {} items", list_len);
            Ok(list_len)
        })
    }
    fn exit_list_at(&mut self, _path: &Path) -> Result<(), TokenReaderError> {
        let mut owner = self.owner.borrow_mut();
        if owner.is_poisoned() {
            return Ok(());
        }

        owner.try(|state| {
            state
                .reader
                .read_const(b"</list>")
                .map_err(TokenReaderError::ReadError)
        })?;
        Ok(())
    }

    fn enter_tagged_tuple_at(
        &mut self,
        _path: &Path,
    ) -> Result<(InterfaceName, Option<Rc<Box<[FieldName]>>>), TokenReaderError> {
        debug!(target: "simple_reader", "tagged tuple");
        let mut owner = self.owner.borrow_mut();
        owner.try(|state| {
            state
                .reader
                .read_const(b"<tuple>")
                .map_err(TokenReaderError::ReadError)?;
            state
                .reader
                .read_const(b"<head>")
                .map_err(TokenReaderError::ReadError)?;

            // Read the kind.
            let kind_name = state.read_string()?;

            // Read the field names
            let len = state.read_u32()?;
            let mut fields = Vec::with_capacity(len as usize);
            for _ in 0..len {
                let name = FieldName::from_string(state.read_string()?);
                fields.push(name);
            }

            state
                .reader
                .read_const(b"</head>")
                .map_err(TokenReaderError::ReadError)?;

            debug!(
                "TreeTokenReader: tagged_tuple has name {:?}, fields {:?}",
                kind_name, fields
            );
            debug!(target: "simple_reader", "/tagged tuple");
            Ok((
                InterfaceName::from_string(kind_name),
                Some(Rc::new(fields.into_boxed_slice())),
            ))
        })
    }
    fn exit_tagged_tuple_at(&mut self, _path: &Path) -> Result<(), TokenReaderError> {
        let mut owner = self.owner.borrow_mut();
        if owner.is_poisoned() {
            return Ok(());
        }

        owner.try(|state| {
            state
                .reader
                .read_const(b"</tuple>")
                .map_err(TokenReaderError::ReadError)
        })?;
        Ok(())
    }

    fn enter_untagged_tuple_at(&mut self, _path: &Path) -> Result<(), TokenReaderError> {
        debug!(target: "simple_reader", "untagged tuple");
        let mut owner = self.owner.borrow_mut();
        owner.try(|state| {
            state
                .reader
                .read_const(b"<tuple>")
                .map_err(TokenReaderError::ReadError)
        })?;
        debug!(target: "simple_reader", "/untagged tuple");
        Ok(())
    }
    fn exit_untagged_tuple_at(&mut self, _path: &Path) -> Result<(), TokenReaderError> {
        let mut owner = self.owner.borrow_mut();
        if owner.is_poisoned() {
            return Ok(());
        }

        owner.try(|state| {
            state
                .reader
                .read_const(b"</tuple>")
                .map_err(TokenReaderError::ReadError)
        })?;
        Ok(())
    }
}

/// A trivial tree writer, without any kind of optimization.
pub struct TreeTokenWriter {
    root: Rc<TreeItem>,
}
impl TreeTokenWriter {
    pub fn new() -> Self {
        TreeTokenWriter {
            root: Rc::new(TreeItem::Bytes(Vec::new())),
        }
    }
    pub fn data(&self) -> Option<&[u8]> {
        match *self.root {
            TreeItem::Bytes(ref bytes) => Some(bytes),
            _ => None,
        }
    }

    fn register(&mut self, data: Vec<u8>) -> AbstractTree {
        let result = Rc::new(TreeItem::Bytes(data));
        self.root = result.clone();
        AbstractTree(result)
    }
}

pub struct Data(pub Rc<Vec<u8>>);
impl AsRef<[u8]> for Data {
    fn as_ref(&self) -> &[u8] {
        self.0.as_ref().as_ref()
    }
}

#[derive(Clone)]
enum TreeItem {
    Bytes(Vec<u8>),
    Offset,
}

/// Abstract type for the contents of the tree.
#[derive(Clone)]
pub struct AbstractTree(Rc<TreeItem>);

impl TreeTokenWriter {
    fn untagged_tuple(
        &mut self,
        children: &[AbstractTree],
    ) -> Result<AbstractTree, TokenWriterError> {
        debug!(target: "simple_writer", "TreeTokenWriter: untagged_tuple");
        let mut result = Vec::new();
        result.extend_from_str("<tuple>"); // Sole purpose of this constant is testing

        const FOOTER: &'static [u8; 8] = b"</tuple>";

        // To be substituted to any Offset field.
        let mut byte_len = FOOTER.len() as u32;

        // First check if children[1] is Offset. If so, compute the length of the rest
        // of the children and substitute that Length to Offset. Note that Offset at any
        // other position than children[1] is an error.
        for (i, item) in children.iter().enumerate() {
            match *(item.0) {
                TreeItem::Bytes(_) if i < 2 => {
                    // That's before any instance of Offset, ignore it.
                }
                TreeItem::Bytes(ref bytes) => {
                    byte_len += bytes.len() as u32;
                }
                TreeItem::Offset if i == 1 => {
                    // Ok, that's the only place where `Offset` is valid.
                }
                TreeItem::Offset => return Err(TokenWriterError::InvalidOffsetField),
            }
        }

        for item in children {
            match *(item.0) {
                TreeItem::Bytes(ref bytes) => {
                    result.extend_from_slice(bytes);
                }
                TreeItem::Offset => {
                    let buf: [u8; 4] = unsafe { std::mem::transmute(byte_len) };
                    result.extend_from_slice(&buf);
                }
            }
        }
        result.extend_from_slice(FOOTER); // Sole purpose of this constant is testing
        Ok(self.register(result))
    }
}
impl TokenWriterWithTree for TreeTokenWriter {
    type Tree = AbstractTree;
    type Data = Vec<u8>;

    fn done(self) -> Result<Self::Data, TokenWriterError> {
        let unwrapped = Rc::try_unwrap(self.root).unwrap_or_else(|e| {
            panic!(
                "We still have {} references to the root",
                Rc::strong_count(&e)
            )
        });
        match unwrapped {
            TreeItem::Bytes(bytes) => Ok(bytes),
            TreeItem::Offset => Err(TokenWriterError::InvalidOffsetField),
        }
    }

    fn float(&mut self, data: Option<f64>) -> Result<Self::Tree, TokenWriterError> {
        let bytes = bytes::float::bytes_of_float(data);
        Ok(self.register(bytes.iter().cloned().collect()))
    }

    fn bool(&mut self, data: Option<bool>) -> Result<Self::Tree, TokenWriterError> {
        debug!(target: "simple_writer", "TreeTokenWriter: bool");
        let result = bytes::bool::bytes_of_bool(data).iter().cloned().collect();
        Ok(self.register(result))
    }

    fn offset(&mut self) -> Result<Self::Tree, TokenWriterError> {
        let tree = Rc::new(TreeItem::Offset);
        self.root = tree.clone();
        Ok(AbstractTree(tree))
    }

    fn unsigned_long(&mut self, data: u32) -> Result<Self::Tree, TokenWriterError> {
        let u1 = (data & 0xff) as u8;
        let u2 = ((data >> 8) & 0xff) as u8;
        let u3 = ((data >> 16) & 0xff) as u8;
        let u4 = ((data >> 24) & 0xff) as u8;
        let bytes = [u1, u2, u3, u4].to_vec();
        Ok(self.register(bytes.iter().cloned().collect()))
    }

    // Strings are represented as len + UTF-8
    // The None string is represented as len + [255, 0]
    fn string(&mut self, data: Option<&SharedString>) -> Result<Self::Tree, TokenWriterError> {
        debug!(target: "simple_writer", "TreeTokenWriter: string {:?}", data);
        const EMPTY_STRING: [u8; 2] = [255, 0];
        let byte_len = match data {
            None => EMPTY_STRING.len(),
            Some(ref x) => x.len(),
        } as u32;
        let buf_len: [u8; 4] = unsafe { std::mem::transmute(byte_len) }; // FIXME: Make this little-endian
        assert!(std::mem::size_of_val(&buf_len) == std::mem::size_of_val(&byte_len));

        let mut buf = Vec::new();
        buf.extend_from_str("<string>");
        buf.extend_from_slice(&buf_len);
        match data {
            None => buf.extend_from_slice(&EMPTY_STRING),
            Some(ref x) => buf.extend(x.bytes()),
        }
        buf.extend_from_str("</string>");

        Ok(self.register(buf))
    }

    /// Lists are represented as:
    /// - "<list>"
    /// - number of items (u32);
    /// - items
    /// - "</list>"
    ///
    /// The number of bytes is the total size of
    /// - number of items;
    /// - items.
    fn list(&mut self, items: Vec<Self::Tree>) -> Result<Self::Tree, TokenWriterError> {
        debug!(target: "simple_writer", "TreeTokenWriter: list");
        let prefix = "<list>";
        let suffix = "</list>";
        let mut result = Vec::new();
        result.extend_from_str(prefix); // Sole purpose of this constant is testing

        let number_of_items = items.len() as u32;
        let buf: [u8; 4] = unsafe { std::mem::transmute(number_of_items) };
        assert!(std::mem::size_of_val(&buf) == std::mem::size_of_val(&number_of_items));

        // Actual number of items
        result.extend_from_slice(&buf);

        // Put actual data
        for item in items {
            match *item.0 {
                TreeItem::Bytes(ref bytes) => result.extend_from_slice(bytes),
                TreeItem::Offset => {
                    // An offset field makes no sense in a list.
                    return Err(TokenWriterError::InvalidOffsetField);
                }
            }
        }

        for i in 0..buf.len() {
            result[i + prefix.len()] = buf[i];
        }

        result.extend_from_str(suffix); // Sole purpose of this constant is testing
        debug!(target: "simple_writer", "TreeTokenWriter: list has {} items", number_of_items);
        Ok(self.register(result))
    }

    /// For this example, we use a very, very, very suboptimal encoding.
    /// - <head>
    ///   - kind (string, \0 terminated)
    ///   - number of items (varnum)
    ///   - field names (string, \0 terminated)
    /// - </head>
    /// - contents
    fn tagged_tuple(
        &mut self,
        tag: &InterfaceName,
        children: &[(&FieldName, Self::Tree)],
    ) -> Result<Self::Tree, TokenWriterError> {
        debug!(target: "simple_writer", "TreeTokenWriter: tagged_tuple");
        let mut prefix = Vec::new();
        prefix.extend_from_str("<head>");
        prefix.extend_from_str(tag.as_str());
        prefix.push(0);

        let number_of_items = children.len() as u32;
        let buf: [u8; 4] = unsafe { std::mem::transmute(number_of_items) };
        assert!(std::mem::size_of_val(&buf) == std::mem::size_of_val(&number_of_items));
        prefix.extend_from_slice(&buf);

        for &(ref field, _) in children.iter() {
            prefix.extend_from_str(field.as_str());
            prefix.push(0);
        }
        prefix.extend_from_str("</head>");

        let mut untagged = Vec::new();
        untagged.push(AbstractTree(Rc::new(TreeItem::Bytes(prefix))));
        for &(_, ref child) in children.iter() {
            untagged.push(child.clone())
        }

        self.untagged_tuple(&untagged)
    }
}

trait ExtendFromUTF8 {
    fn extend_from_str(&mut self, &str);
}

impl ExtendFromUTF8 for Vec<u8> {
    fn extend_from_str(&mut self, data: &str) {
        self.extend_from_slice(data.as_bytes());
    }
}

/// Command-line management.
pub struct FormatProvider;
impl ::FormatProvider for FormatProvider {
    fn subcommand<'a, 'b>(&self) -> clap::App<'a, 'b> {
        use clap::*;
        SubCommand::with_name("expanded")
            .about("(EXPERIMENTAL) Use the expanded (aka 'simple') format. This format is designed to help with debugging decoders, but has no other good properties.")
    }

    fn handle_subcommand(
        &self,
        _spec: &binjs_meta::spec::Spec,
        _: Option<&clap::ArgMatches>,
    ) -> Result<::Format, ::std::io::Error> {
        Ok(::Format::Simple)
    }
}

#[test]
fn test_simple_io() {
    use binjs_shared::ast::Path;
    use binjs_shared::{FieldName, InterfaceName, SharedString};
    use io::TokenWriterWithTree;
    use std::fs::*;

    use std::io::{Cursor, Write};

    let path = Path::new();

    eprintln!("Testing string I/O");

    {
        let mut writer = TreeTokenWriter::new();
        writer
            .string(Some(&SharedString::from_str("simple string")))
            .expect("Writing simple string");

        let data = writer.data().unwrap();
        File::create("/tmp/test-simple-string.binjs")
            .unwrap()
            .write_all(data)
            .unwrap();

        let mut reader = TreeTokenReader::new(Cursor::new(data));
        let simple_string = reader
            .string_at(&path)
            .expect("Reading simple string")
            .expect("Non-null string");
        assert_eq!(&simple_string, "simple string");
    }

    {
        let data = SharedString::from_str("string with escapes \u{0}\u{1}\u{0}");
        let mut writer = TreeTokenWriter::new();
        writer
            .string(Some(&data))
            .expect("Writing string with escapes");

        let result = writer.data().unwrap();
        File::create("/tmp/test-string-with-escapes.binjs")
            .unwrap()
            .write_all(result)
            .unwrap();

        let mut reader = TreeTokenReader::new(Cursor::new(result));
        let escapes_string = reader
            .string_at(&path)
            .expect("Reading string with escapes")
            .expect("Non-null string");
        assert_eq!(escapes_string, data);
    }

    eprintln!("Testing untagged tuple I/O");

    {
        let mut writer = TreeTokenWriter::new();
        writer
            .untagged_tuple(&[])
            .expect("Writing empty untagged tuple");

        let data = writer.data().unwrap();
        File::create("/tmp/test-empty-untagged-tuple.binjs")
            .unwrap()
            .write_all(data)
            .unwrap();

        let mut reader = TreeTokenReader::new(Cursor::new(data));
        reader
            .enter_untagged_tuple_at(&path)
            .expect("Reading empty untagged tuple");

        reader
            .exit_untagged_tuple_at(&path)
            .expect("Empty untagged tuple read properly");
    }

    {
        let mut writer = TreeTokenWriter::new();
        let item_0 = writer.string(Some(&SharedString::from_str("foo"))).unwrap();
        let item_1 = writer.string(Some(&SharedString::from_str("bar"))).unwrap();
        writer
            .untagged_tuple(&[item_0, item_1])
            .expect("Writing trivial untagged tuple");

        let data = writer.data().unwrap();
        File::create("/tmp/test-trivial-untagged-tuple.binjs")
            .unwrap()
            .write_all(data)
            .unwrap();

        let mut reader = TreeTokenReader::new(Cursor::new(data));
        reader
            .enter_untagged_tuple_at(&path)
            .expect("Reading trivial untagged tuple");
        let simple_string = reader
            .string_at(&path)
            .expect("Reading trivial tuple[0]")
            .expect("Non-null string");
        assert_eq!(&simple_string, "foo");
        let simple_string = reader
            .string_at(&path)
            .expect("Reading trivial tuple[1]")
            .expect("Non-null string");
        assert_eq!(&simple_string, "bar");

        reader
            .exit_untagged_tuple_at(&path)
            .expect("Untagged tuple read properly");
    }

    eprintln!("Testing tagged tuple I/O");

    {
        let mut writer = TreeTokenWriter::new();
        let item_0 = writer.string(Some(&SharedString::from_str("foo"))).unwrap();
        let item_1 = writer.float(Some(3.1415)).unwrap();
        writer
            .tagged_tuple(
                &InterfaceName::from_str("BindingIdentifier"),
                &[
                    (&FieldName::from_str("label"), item_0),
                    (&FieldName::from_str("value"), item_1),
                ],
            )
            .expect("Writing trivial tagged tuple");

        let data = writer.data().unwrap();
        File::create("/tmp/test-simple-tagged-tuple.binjs")
            .unwrap()
            .write_all(data)
            .unwrap();

        let mut reader = TreeTokenReader::new(Cursor::new(data));
        let (name, fields) = reader
            .enter_tagged_tuple_at(&path)
            .expect("Reading trivial tagged tuple");
        assert_eq!(name, "BindingIdentifier");

        // Order of fields is deterministic
        let fields = fields.expect("Missing fields");
        assert!(fields[0] == "label");
        assert!(fields[1] == "value");
        let simple_string = reader
            .string_at(&path)
            .expect("Reading trivial tagged tuple[0]")
            .expect("Reading a non-null string");
        assert_eq!(simple_string, "foo");
        let simple_float = reader
            .float_at(&path)
            .expect("Reading trivial tagged tuple[1]")
            .expect("Reading a non-null float");
        assert_eq!(simple_float, 3.1415);

        reader
            .exit_tagged_tuple_at(&path)
            .expect("Trivial tagged tuple read properly");
    }

    eprintln!("Testing list I/O");

    {
        let mut writer = TreeTokenWriter::new();
        writer.list(vec![]).expect("Writing empty list");

        let data = writer.data().unwrap();
        File::create("/tmp/test-empty-list.binjs")
            .unwrap()
            .write_all(data)
            .unwrap();

        let mut reader = TreeTokenReader::new(Cursor::new(data));
        let len = reader.enter_list_at(&path).expect("Reading empty list");
        assert_eq!(len, 0);

        reader
            .exit_list_at(&path)
            .expect("Empty list read properly");
    }

    {
        let mut writer = TreeTokenWriter::new();
        let item_0 = writer.string(Some(&SharedString::from_str("foo"))).unwrap();
        let item_1 = writer.string(Some(&SharedString::from_str("bar"))).unwrap();
        writer
            .list(vec![item_0, item_1])
            .expect("Writing trivial list");

        let data = writer.data().unwrap();
        File::create("/tmp/test-trivial-list.binjs")
            .unwrap()
            .write_all(data)
            .unwrap();

        let mut reader = TreeTokenReader::new(Cursor::new(data));
        let len = reader.enter_list_at(&path).expect("Reading trivial list");
        assert_eq!(len, 2);

        let simple_string = reader
            .string_at(&path)
            .expect("Reading trivial list[0]")
            .expect("Non-null string");
        assert_eq!(&simple_string, "foo");
        let simple_string = reader
            .string_at(&path)
            .expect("Reading trivial list[1]")
            .expect("Non-null string");
        assert_eq!(&simple_string, "bar");

        reader
            .exit_list_at(&path)
            .expect("Trivial list read properly");
    }

    {
        let mut writer = TreeTokenWriter::new();
        let item_0 = writer.string(Some(&SharedString::from_str("foo"))).unwrap();
        let item_1 = writer.string(Some(&SharedString::from_str("bar"))).unwrap();
        let list = writer
            .list(vec![item_0, item_1])
            .expect("Writing inner list");
        writer.list(vec![list]).expect("Writing outer list");

        let data = writer.data().unwrap();
        File::create("/tmp/test-nested-lists.binjs")
            .unwrap()
            .write_all(data)
            .unwrap();

        let mut reader = TreeTokenReader::new(Cursor::new(data));
        let len = reader.enter_list_at(&path).expect("Reading outer list");
        assert_eq!(len, 1);

        let len = reader.enter_list_at(&path).expect("Reading inner list");
        assert_eq!(len, 2);

        let simple_string = reader
            .string_at(&path)
            .expect("Reading trivial list[0]")
            .expect("Non-null string");
        assert_eq!(&simple_string, "foo");
        let simple_string = reader
            .string_at(&path)
            .expect("Reading trivial list[1]")
            .expect("Non-null string");
        assert_eq!(&simple_string, "bar");

        reader
            .exit_list_at(&path)
            .expect("Inner list read properly");
        reader
            .exit_list_at(&path)
            .expect("Inner list read properly");
    }
}
