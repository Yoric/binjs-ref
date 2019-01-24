//! A trivial exporter to xml.
//!
//! Used mainly to extract statistics and/or to compare with XML-based compression mechanisms.

use {TokenWriterError, TokenWriterWithTree};

use binjs_shared::{FieldName, InterfaceName, SharedString};

use std::io::Write;
use std::rc::Rc;

use clap;
use xml_rs;

#[derive(Debug)]
pub enum SubTree {
    String(Option<SharedString>),
    Bool(Option<bool>),
    Float(Option<f64>),
    U32(u32),
    List(Vec<Rc<SubTree>>),
    Node {
        name: SharedString,
        children: Vec<(SharedString, Rc<SubTree>)>,
    },
}

impl SubTree {
    fn write<W: Write>(
        &self,
        out: &mut xml_rs::writer::EventWriter<W>,
    ) -> xml_rs::writer::Result<()> {
        use self::SubTree::*;
        use xml_rs::writer::*;
        match *self {
            String(Some(ref s)) => {
                out.write(XmlEvent::characters(s.as_str()))?;
            }
            String(None) => {}
            Bool(Some(true)) => {
                out.write(XmlEvent::characters("true"))?;
            }
            Bool(Some(false)) => {
                out.write(XmlEvent::characters("false"))?;
            }
            Bool(None) => {}
            Float(Some(ref x)) => {
                out.write(XmlEvent::characters(&format!("{}", x)))?;
            }
            Float(None) => {}
            U32(ref value) => {
                out.write(XmlEvent::characters(&format!("{}", value)))?;
            }
            List(ref children) => {
                for c in children {
                    out.write(XmlEvent::start_element("_item"))?;
                    c.write(out)?;
                    out.write(XmlEvent::end_element())?;
                }
            }
            Node {
                ref name,
                ref children,
            } => {
                if name.len() == 0 {
                    assert_eq!(children.len(), 0);
                // Nothing to write
                } else {
                    out.write(XmlEvent::start_element(name.as_str()))?;
                    for (ref name, ref c) in children {
                        out.write(XmlEvent::start_element(name.as_str()))?;
                        c.write(out)?;
                        out.write(XmlEvent::end_element())?;
                    }
                    out.write(XmlEvent::end_element())?;
                }
            }
        }
        Ok(())
    }
}

pub struct Encoder {
    root: Rc<SubTree>,
}
impl Encoder {
    pub fn new() -> Self {
        Self {
            root: Rc::new(SubTree::Bool(None)),
        }
    }

    fn register(&mut self, tree: SubTree) -> Result<Rc<SubTree>, TokenWriterError> {
        let result = Rc::new(tree);
        self.root = result.clone();
        Ok(result)
    }
}
impl TokenWriterWithTree for Encoder {
    type Tree = Rc<SubTree>;
    type Data = Vec<u8>;

    fn bool(&mut self, data: Option<bool>) -> Result<Self::Tree, TokenWriterError> {
        self.register(SubTree::Bool(data))
    }

    fn unsigned_long(&mut self, data: u32) -> Result<Self::Tree, TokenWriterError> {
        self.register(SubTree::U32(data))
    }

    fn float(&mut self, data: Option<f64>) -> Result<Self::Tree, TokenWriterError> {
        self.register(SubTree::Float(data))
    }

    fn string(&mut self, data: Option<&SharedString>) -> Result<Self::Tree, TokenWriterError> {
        self.register(SubTree::String(data.map(Clone::clone)))
    }

    fn tagged_tuple(
        &mut self,
        tag: &InterfaceName,
        items: &[(&FieldName, Self::Tree)],
    ) -> Result<Self::Tree, TokenWriterError> {
        self.register(SubTree::Node {
            name: tag.as_shared_string().clone(),
            children: items
                .iter()
                .map(|(attribute, tree)| (attribute.as_shared_string().clone(), tree.clone()))
                .collect(),
        })
    }

    fn list(&mut self, items: Vec<Self::Tree>) -> Result<Self::Tree, TokenWriterError> {
        self.register(SubTree::List(items))
    }

    fn offset(&mut self) -> Result<Self::Tree, TokenWriterError> {
        // FIXME: We'll want to build a forest and put skippable stuff after the rest.
        unimplemented!()
    }

    fn done(self) -> Result<Self::Data, TokenWriterError> {
        use xml_rs::writer::*;
        let mut buf = vec![];
        {
            let mut writer = EmitterConfig::new()
                .perform_indent(true)
                .create_writer(&mut buf);
            self.root.write(&mut writer).unwrap();
        }
        Ok(buf)
    }
}

/// Command-line management.
pub struct FormatProvider;
impl ::FormatProvider for FormatProvider {
    fn subcommand<'a, 'b>(&self) -> clap::App<'a, 'b> {
        use clap::*;
        SubCommand::with_name("xml")
            .about("(EXPERIMENTAL) Encode to xml. This format is designed to help gather statistics, and is not considered useful for any other reason.")
    }

    fn handle_subcommand(
        &self,
        _spec: &binjs_meta::spec::Spec,
        _matches: Option<&clap::ArgMatches>,
    ) -> Result<::Format, ::std::io::Error> {
        Ok(::Format::XML)
    }
}
