#[derive(Default, Display, Add, AddAssign, Into, From, Clone, Copy)]
pub struct Bytes(usize);

impl std::iter::Sum for Bytes {
    fn sum<I>(iter: I) -> Self
    where
        I: Iterator<Item = Bytes>,
    {
        iter.fold(Default::default(), std::ops::Add::add)
    }
}

/// A newtype for `usize` used to count the number of instances of some item.
#[derive(Default, Display, Serialize, Deserialize, From, Into, Add, AddAssign, Clone, Copy)]
pub struct Instances(usize);

impl std::iter::Sum for Instances {
    fn sum<I>(iter: I) -> Self
    where
        I: Iterator<Item = Instances>,
    {
        iter.fold(Default::default(), std::ops::Add::add)
    }
}

pub struct BytesAndInstances {
    bytes: Bytes,
    instances: Instances,
}
impl BytesAndInstances {
    pub fn new(bytes: Bytes, instances: Instances) -> Self {
        BytesAndInstances { bytes, instances }
    }
}

/// A macro used to generate code that will operate on all fields of a `PerUserExtensibleKind`.
#[macro_export]
macro_rules! for_field_in_user_extensible {
    ( $cb: ident ) => {
        $cb!(
            (floats, "floats", b"floats"),
            (unsigned_longs, "unsigned_longs", b"unsigned_longs"),
            (property_keys, "property_keys", b"property_keys"),
            (identifier_names, "identifier_names", b"identifier_names"),
            (string_literals, "string_literals", b"string_literals"),
            (list_lengths, "list_lengths", b"list_lengths"),
            (string_enums, "string_enums", b"string_enums"),
            (interface_names, "interface_names", b"interface_names")
        )
    };
}

/// During compression, we typically deal with both grammar-fixed data
/// (e.g. the list of possible values in a string enum) and user-extensible
/// data (e.g. string literals).
///
/// This container is meant to store data associated with user-extensible data.
/// This serves typically to store per-kind compression settings, per-kind
/// compressed/decompressed data, dictionaries, etc.
#[derive(Debug, Default, Add, Clone, AddAssign)]
pub struct PerUserExtensibleKind<T> {
    pub floats: T,
    pub unsigned_longs: T,
    pub property_keys: T,
    pub identifier_names: T,
    pub string_literals: T,
    pub list_lengths: T,
    pub string_enums: T,
    pub interface_names: T,
}
impl<T> PerUserExtensibleKind<T> {
    /// Initialize a new `PerUserExtensibleKind`.
    pub fn with<F>(f: F) -> Self
    where
        F: Fn(&str) -> T,
    {
        // Generate a PerUserExtensibleKind, where for each field:
        //    `foo: f("foo")`.
        //
        // This macro doubles as a static checks that we haven't forgotten
        // any field in `for_field_in_user_extensible`.
        macro_rules! with_field { ($(($ident: ident, $name: expr, $bname: expr )),*) => {
            PerUserExtensibleKind {
                $(
                    $ident: f($name),
                )*
            }
        } };
        for_field_in_user_extensible!(with_field)
    }

    /// Convert a `PerUserExtensibleKind` into another one.
    pub fn into_with<F, U>(self, f: F) -> PerUserExtensibleKind<U>
    where
        F: Fn(&str, T) -> U,
    {
        // Generate a PerUserExtensibleKind, where for each field:
        //    `foo: f("foo", self.foo)`.
        macro_rules! with_field { ($(($ident: ident, $name: expr, $bname: expr )),*) => {
            PerUserExtensibleKind {
                $(
                    $ident: f($name, self.$ident),
                )*
            }
        } };
        for_field_in_user_extensible!(with_field)
    }

    pub fn iter(&self) -> impl Iterator<Item = (&'static str, &T)> {
        // Generate a vector with one item per field
        //    `(foo, &self.foo)`.
        macro_rules! with_field { ($(($ident: ident, $name: expr, $bname: expr )),*) => {
            vec![
                $(
                    ($name, &self.$ident),
                )*
            ]
        } };
        for_field_in_user_extensible!(with_field).into_iter()
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = (&'static str, &mut T)> {
        // Generate a vector with one item per field
        //    `(foo, &mut self.foo)`.
        macro_rules! with_field { ($(($ident: ident, $name: expr, $bname: expr )),*) => {
            vec![
                $(
                    ($name, &mut self.$ident),
                )*
            ]
        } };
        for_field_in_user_extensible!(with_field).into_iter()
    }

    pub fn into_iter(self) -> impl Iterator<Item = (&'static str, T)> {
        // Generate a vector with one item per field
        //    `(foo, self.foo)`.
        macro_rules! with_field { ($(($ident: ident, $name: expr, $bname: expr )),*) => {
            vec![
                $(
                    ($name, self.$ident),
                )*
            ]
        } };
        for_field_in_user_extensible!(with_field).into_iter()
    }

    /// Access a field by its name.
    ///
    /// This method is typically used to simplify parsing a file that
    /// contains sections explicitly labelled "bools", "floats", etc.
    /// In such case, `field_name` is expected to be a user input.
    ///
    /// Return `None` if `field_name` is not one of the field names.
    pub fn get(&self, field_name: &str) -> Option<&T> {
        self.get_b(field_name.as_bytes())
    }
    pub fn get_mut(&mut self, field_name: &str) -> Option<&mut T> {
        self.get_mut_b(field_name.as_bytes())
    }

    /// Access a field by its name, specified as a sequence of bytes.
    ///
    /// This method is typically used to simplify parsing a file that
    /// contains sections explicitly labelled "bools", "floats", etc.
    /// In such case, `field_name` is expected to be a user input.
    ///
    /// Return `None` if `field_name` is not one of the field names.
    pub fn get_mut_b(&mut self, field_name: &[u8]) -> Option<&mut T> {
        // Generate a `match` with for each field
        //    `b"foo" => Some(&mut self.foo)`
        macro_rules! with_field { ($(($ident: ident, $name: expr, $bname: expr )),*) => {
            match field_name {
                $(
                    $bname => Some(&mut self.$ident),
                )*
                _ => None,
            }
        } };
        for_field_in_user_extensible!(with_field)
    }
    pub fn get_b(&self, field_name: &[u8]) -> Option<&T> {
        // Generate a `match` with for each field
        //    `b"foo" => Some(&self.foo)`
        macro_rules! with_field { ($(($ident: ident, $name: expr, $bname: expr )),*) => {
            match field_name {
                $(
                    $bname => Some(&self.$ident),
                )*
                _ => None,
            }
        } };
        for_field_in_user_extensible!(with_field)
    }
}

/// A macro used to generate code that will operate on all fields of a `PerStaticKind`.
#[macro_export]
macro_rules! for_field_in_per_static {
    ( $cb: ident ) => {
        $cb!(
            (bools, "bools", "bools", b"bools"),
            (interface_names, "interfaces", "interfaces", b"interfaces"),
            (
                string_enums,
                "string_enums",
                "string enums",
                b"string_enums"
            )
        )
    };
}

/// During compression, we typically deal with both grammar-fixed data
/// (e.g. the list of possible values in a string enum) and user-extensible
/// data (e.g. string literals).
///
/// This container is meant to store data associated with grammar-fixed data.
/// This serves typically to store per-kind compression settings, per-kind
/// compressed/decompressed data, dictionaries, etc.
#[derive(Debug, Default)]
pub struct PerStaticKind<T> {
    pub bools: T,
    pub interface_names: T,
    pub string_enums: T,
}

/// A default number of buckets for histograms.
const DEFAULT_HISTOGRAM_BUCKETS: usize = 32;

/// Simple representation of a rational number.
///
/// Used to avoid some fixed-point computation when it's not really necessary.
pub struct Rational<T> {
    /// The numerator.
    pub num: T,

    /// The denominator.
    pub den: T,
}

/// A histogram designed to store information on how hoften we perform an operation
/// marked by a given probability.
pub struct ProbabilityHistogram {
    buckets: Vec<usize>,
}
impl ProbabilityHistogram {
    pub fn with_capacity(len: usize) -> Self {
        assert!(len > 1);
        let mut buckets = Vec::with_capacity(len);
        buckets.resize(len, 0);
        Self { buckets }
    }

    pub fn new() -> Self {
        Self::with_capacity(DEFAULT_HISTOGRAM_BUCKETS)
    }

    /// Add a probability to the corresponding bucket in the histogram.
    pub fn add_probability(&mut self, probability: Rational<usize>) {
        assert!(probability.num <= probability.den);
        let len = self.buckets.len();
        let index = (probability.num * (len - 1)) / probability.den;
        self.buckets[index] += 1;
    }
}
impl Default for ProbabilityHistogram {
    fn default() -> Self {
        Self::new()
    }
}

impl std::fmt::Display for ProbabilityHistogram {
    fn fmt(&self, formatter: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        let total: usize = self.buckets.iter().sum();
        write!(formatter, "  bucket ")?;
        for i in 0..32 {
            write!(formatter, " | {:3}", i)?;
        }
        write!(formatter, "\n --------")?;
        for _ in 0..32 {
            write!(formatter, "-|----")?;
        }
        write!(formatter, "\n       % ")?;
        for bucket in &self.buckets {
            write!(formatter, " | {:3.0}", 100. * *bucket as f64 / total as f64)?;
        }
        write!(formatter, "\n")?;
        Ok(())
    }
}

impl std::fmt::Display for PerStaticKind<ProbabilityHistogram> {
    fn fmt(&self, formatter: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(formatter, "Static:\n")?;
        macro_rules! with_field { ($(($ident: ident, $name: expr, $user_readable: expr, $bname: expr )),*) => {
                $(
                    write!(formatter, "    {name}:\n", name = $user_readable)?;
                    write!(formatter, "{content}", content = self.$ident)?;
                )*
        } };
        for_field_in_per_static!(with_field);
        Ok(())
    }
}

pub trait DisplayWith<T> {
    fn fmt(&self, formatter: &mut std::fmt::Formatter, data: &T) -> Result<(), std::fmt::Error>;
}

impl DisplayWith</* Total */ BytesAndInstances> for BytesAndInstances {
    fn fmt(
        &self,
        formatter: &mut std::fmt::Formatter,
        total: &BytesAndInstances,
    ) -> Result<(), std::fmt::Error> {
        let bytes = Into::<usize>::into(self.bytes);
        let symbols = Into::<usize>::into(self.instances);
        let total_bytes = Into::<usize>::into(total.bytes);
        let total_symbols = Into::<usize>::into(total.instances);
        write!(formatter, "symbols {symbols} = {symbols_percent:.2}, bytes {bytes} = {bytes_percent:.2} ({bits_per_symbol:.2} bits/symbol)",
            symbols = symbols,
            bytes = bytes,
            symbols_percent = 100.* symbols as f64 / total_symbols as f64,
            bytes_percent = 100.* bytes as f64 / total_bytes as f64,
            bits_per_symbol = 8. * bytes as f64 / symbols as f64,
        )
    }
}

impl std::fmt::Display for PerUserExtensibleKind<BytesAndInstances> {
    fn fmt(&self, formatter: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        let total = BytesAndInstances {
            bytes: self.iter().map(|(_, data)| data.bytes.clone()).sum(),
            instances: self.iter().map(|(_, data)| data.instances.clone()).sum(),
        };

        write!(formatter, "  User-extensible:\n")?;
        for (field, name) in &[
            (&self.identifier_names, "identifier names"),
            (&self.string_literals, "string literals"),
            (&self.floats, "floats"),
            (&self.unsigned_longs, "unsigned longs"),
            (&self.list_lengths, "list lengths"),
        ] {
            write!(formatter, "    {name}: ", name = name)?;
            field.fmt(formatter, &total)?;
            write!(formatter, "\n")?;
        }
        write!(
            formatter,
            "Total: {} bytes, {} symbols",
            total.bytes, total.instances
        )?;
        Ok(())
    }
}
