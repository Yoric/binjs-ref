
/// A container for information associated with a type of data we write to the stream
/// as part of the content (i.e. not the header).
///
/// Typically used to collect/display the number of bytes written in each category.
#[derive(Debug, Default, Add, Clone, AddAssign)]
pub struct ContentInfo<T> {
    pub bools: T,
    pub floats: T,
    pub unsigned_longs: T,
    pub string_enums: T,
    pub property_keys: T,
    pub identifier_names: T,
    pub interface_names: T,
    pub string_literals: T,
    pub list_lengths: T,
}
impl<T> ContentInfo<T> {
    /// Initialize a new `ContentInfo`.
    pub fn with<F>(f: F) -> Self
        where F: Fn(&str) -> T
    {
        ContentInfo {
            bools: f("bools"),
            floats: f("floats"),
			unsigned_longs: f("unsigned_longs"),
			string_enums: f("string_enums"),
			property_keys: f("property_keys"),
			identifier_names: f("identifier_names"),
			interface_names: f("interface_names"),
			string_literals: f("string_literals"),
			list_lengths: f("list_lengths"),
        }
    }

    /// Convert a `ContentInfo` into another one.
    pub fn into_with<F, U>(self, f: F) -> ContentInfo<U>
        where F: Fn(T, &str) -> U
    {
        ContentInfo {
			bools: f(self.bools, "bools"),
			floats: f(self.floats, "floats"),
			unsigned_longs: f(self.unsigned_longs, "unsigned_longs"),
			string_enums: f(self.string_enums, "string_enums"),
			property_keys: f(self.property_keys, "property_keys"),
			identifier_names: f(self.identifier_names, "identifier_names"),
			interface_names: f(self.interface_names, "interface_names"),
			string_literals: f(self.string_literals, "string_literals"),
			list_lengths: f(self.list_lengths, "list_lengths"),
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = (&T, &'static str)> {
        vec![
            (&self.bools, "bools"),
            (&self.floats, "floats"),
			(&self.unsigned_longs, "unsigned_longs"),
			(&self.string_enums, "string_enums"),
			(&self.property_keys, "property_keys"),
			(&self.identifier_names, "identifier_names"),
			(&self.interface_names, "interface_names"),
			(&self.string_literals, "string_literals"),
			(&self.list_lengths, "list_lengths"),
        ].into_iter()
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = (&mut T, &'static str)> {
        vec![
            (&mut self.bools, "bools"),
            (&mut self.floats, "floats"),
			(&mut self.unsigned_longs, "unsigned_longs"),
			(&mut self.string_enums, "string_enums"),
			(&mut self.property_keys, "property_keys"),
			(&mut self.identifier_names, "identifier_names"),
			(&mut self.interface_names, "interface_names"),
			(&mut self.string_literals, "string_literals"),
			(&mut self.list_lengths, "list_lengths"),
        ].into_iter()
    }

    pub fn into_iter(self) -> impl Iterator<Item = (T, &'static str)> {
        vec![
            (self.bools, "bools"),
            (self.floats, "floats"),
			(self.unsigned_longs, "unsigned_longs"),
			(self.string_enums, "string_enums"),
			(self.property_keys, "property_keys"),
			(self.identifier_names, "identifier_names"),
			(self.interface_names, "interface_names"),
			(self.string_literals, "string_literals"),
			(self.list_lengths, "list_lengths"),
        ].into_iter()
    }
}

impl<T> std::iter::FromIterator<(T, &'static str)> for ContentInfo<Option<T>> {
    fn from_iter<U>(iter: U) -> Self
    where
        U: IntoIterator<Item = (T, &'static str)>
    {
        let mut container = Self::with(|_| None);
        for (value, name) in iter.into_iter() {
            let field = match name {
                "bools" => Some(&mut container.bools),
    			"floats" => Some(&mut container.floats),
    			"unsigned_longs" => Some(&mut container.unsigned_longs),
    			"string_enums" => Some(&mut container.string_enums),
    			"property_keys" => Some(&mut container.property_keys),
    			"identifier_names" => Some(&mut container.identifier_names),
    			"interface_names" => Some(&mut container.interface_names),
    			"string_literals" => Some(&mut container.string_literals),
    			"list_lengths" => Some(&mut container.list_lengths),
                _ => None
            }.unwrap_or_else(|| panic!("This field doesn't exist {}", name));
            let prev = field.replace(value);
            assert!(prev.is_none(), "We have two definitions for field {}", name);
        }
        container
    }
}