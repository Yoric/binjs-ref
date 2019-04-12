//! Data structures used to read the content streams of a compressed file,
//! i.e. sequences of indices that map either into a static dictionary
//! or into a dynamic dictionary.

use bytes::varnum::ReadVarNum;
use entropy::dictionary::LinearTable;
use entropy::rw::TableRefStreamState;
use TokenReaderError;

use binjs_shared::SharedString;

use std::hash::Hash;
use std::io::Cursor;

/// A data structure used to read a content stream, sequences of indices
/// that map either into a static dictionary or into a dynamic dictionary.
///
/// `T` is the type of items in the dictionary, e.g. `Option<u32>`, `IdentifierName`,
/// ...
pub struct DictionaryStreamDecoder<T>
where
    T: Eq + Hash + Clone + Ord,
{
    indexed_dictionary: LinearTable<T>,

    /// A stream of varnums. Each varnum `n` is an index into either the shared_dictionary
    /// (if `n < shared_dictionary.len()`) or the prelude dictionary (otherwise).
    ///
    /// May be `None` if the file does not actually contain a content stream.
    stream: Option<Cursor<Vec<u8>>>,

    /// The name of this dictionary. Used for debugging/error reporting.
    name: SharedString,

    index_stream_state: TableRefStreamState<T>,
}
impl<T> DictionaryStreamDecoder<T>
where
    T: Eq + Hash + Clone + Ord,
{
    /// Create a decoder from a shared dictionary, a prelude dictionary and a stream of varnum-encoded values.
    pub fn try_new(
        indexed_dictionary: LinearTable<T>,
        name: SharedString,
        maybe_input: Option<Vec<u8>>,
    ) -> Result<Self, TokenReaderError> {
        debug!(target: "read", "DictionaryStreamDecoder::new {} is a {}",
            name.as_str(),
            match maybe_input {
                None => "EMPTY stream".to_string(),
                Some(ref vec) => format!("non-empty ({} bytes) stream", vec.len())
            }
        );
        let mut maybe_stream = maybe_input.map(Cursor::new);

        // Determine the window length for the TableRefStreamState.
        //
        // If the stream is empty, we arbitrarily default to 0, as the TableRefStreamState won't be used.
        let window_len = match maybe_stream {
            Some(ref mut stream) => {
                stream.read_varnum().map_err(TokenReaderError::ReadError)? as usize
            }
            None => 0,
        };
        let index_stream_state = TableRefStreamState::new(window_len, &indexed_dictionary);
        Ok(Self {
            indexed_dictionary,
            stream: maybe_stream,
            name,
            index_stream_state,
        })
    }
}
impl<T> Iterator for DictionaryStreamDecoder<T>
where
    T: Hash + Eq + Clone + std::fmt::Debug + Ord + crate::entropy::util::ShowMe,
{
    type Item = Result<T, TokenReaderError>;
    fn next(&mut self) -> Option<Self::Item> {
        debug!(target: "read", "DictionaryStreamDecoder::next {} on a {} stream",
            self.name,
            match self.stream {
                None => "EMPTY",
                _ => "non-empty"
            }
        );
        match self.stream {
            None => return None,
            Some(ref mut stream) => {
                debug!(target: "read", "DictionaryStreamDecoder::next {} position: {} / {}",
                    self.name,
                    stream.position(),
                    stream.get_ref().len());
                if stream.position() == stream.get_ref().len() as u64 {
                    // We have reached the end of this stream.
                    return None;
                }
                let as_u32 = match stream.read_varnum() {
                    Ok(result) => result,
                    Err(err) => return Some(Err(TokenReaderError::ReadError(err))),
                };
                let index = match self.index_stream_state.from_u32(as_u32) {
                    Some(index) => index,
                    None => {
                        return Some(Err(TokenReaderError::BadDictionaryIndex {
                            index: as_u32,
                            dictionary: self.name.clone(),
                        }));
                    }
                };
                let result = self.indexed_dictionary.at_index(&index).unwrap(); // We have checked just above that the `index` is correct.
                Some(Ok(result.clone()))
            }
        }
    }
}
