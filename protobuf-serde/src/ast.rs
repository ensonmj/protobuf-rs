//! This file can be seen as a rust transcription of the
//! [descriptor.proto](https://github.com/google/protobuf/blob/master/src/google/protobuf/descriptor.proto) file

#[derive(Debug, Default, Clone)]
pub struct FileDescriptor {
    /// Protobuf Syntax
    pub syntax: Syntax,
    /// Package
    pub package: String,
    /// Import
    pub imports: Vec<String>,
    /// Not-builtin options
    pub options: Vec<ProtobufOption>,
    /// Enums
    pub enums: Vec<Enumeration>,
    /// Top level messages
    pub messages: Vec<Message>,
    /// Extend
    pub extends: Vec<Extend>,
    /// Services
    pub services: Vec<Service>,
}

/// Protobuf syntax
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum Syntax {
    /// Protobuf syntax 2(default)
    Proto2,
    /// Protobuf syntax 3
    Proto3,
}

impl Default for Syntax {
    fn default() -> Syntax {
        Syntax::Proto2
    }
}

/// Protobuf message
#[derive(Debug, Clone, Default)]
pub struct Message {
    /// Message name
    pub name: String,
    /// Message `Field`s
    pub fields: Vec<Field>,
    /// Message `OneOf`s
    pub oneofs: Vec<OneOf>,
    /// Message reserved numbers
    /// TODO: use RangeInclusive once stable
    pub reserved_nums: Vec<FieldNumberRange>,
    /// Message reserved names
    pub reserved_names: Vec<String>,
    /// Nested enums
    pub enums: Vec<Enumeration>,
    /// Nested messages
    pub messages: Vec<Message>,
    /// Not-builtin options
    pub options: Vec<ProtobufOption>,
}

/// A Protobuf Field
#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    /// Field name
    pub name: String,
    /// Field `Rule`
    pub rule: FieldLabel,
    /// Field type
    pub typ: FieldType,
    /// Tag number
    pub number: i32,
    /// Non-builtin options
    pub options: Vec<ProtobufOption>,
}

/// A field rule
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum FieldLabel {
    /// A well-formed message can have zero or one of this field (but not more than one).
    Optional,
    /// This field can be repeated any number of times (including zero) in a well-formed message.
    /// The order of the repeated values will be preserved.
    Repeated,
    /// A well-formed message must have exactly one of this field.
    Required,
}

/// Protobuf supported field types
#[derive(Debug, Clone, PartialEq)]
pub enum FieldType {
    /// Protobuf int32
    ///
    /// Uses variable-length encoding. Inefficient for encoding negative numbers – if
    /// your field is likely to have negative values, use sint32 instead.
    Int32,
    /// Protobuf int64
    ///
    /// Uses variable-length encoding. Inefficient for encoding negative numbers – if
    /// your field is likely to have negative values, use sint64 instead.
    Int64,
    /// Protobuf uint32
    ///
    /// Uses variable-length encoding.
    Uint32,
    /// Protobuf uint64
    ///
    /// Uses variable-length encoding.
    Uint64,
    /// Protobuf sint32
    ///
    /// Uses ZigZag variable-length encoding. Signed int value. These more efficiently
    /// encode negative numbers than regular int32s.
    Sint32,
    /// Protobuf sint64
    ///
    /// Uses ZigZag variable-length encoding. Signed int value. These more efficiently
    /// encode negative numbers than regular int32s.
    Sint64,
    /// Protobuf bool
    Bool,
    /// Protobuf fixed64
    ///
    /// Always eight bytes. More efficient than uint64 if values are often greater than 2^56.
    Fixed64,
    /// Protobuf sfixed64
    ///
    /// Always eight bytes.
    Sfixed64,
    /// Protobuf double
    Double,
    /// Protobuf string
    ///
    /// A string must always contain UTF-8 encoded or 7-bit ASCII text.
    String,
    /// Protobuf bytes
    ///
    /// May contain any arbitrary sequence of bytes.
    Bytes,
    /// Protobut fixed32
    ///
    /// Always four bytes. More efficient than uint32 if values are often greater than 2^28.
    Fixed32,
    /// Protobut sfixed32
    ///
    /// Always four bytes.
    Sfixed32,
    /// Protobut float
    Float,
    /// Protobuf message or enum (holds the name)
    MessageOrEnum(String),
    /// Protobut map
    Map(Box<(FieldType, FieldType)>),
    /// Protobuf group (deprecated)
    Group(Vec<Field>),
}

/// Extension range
#[derive(Default, Debug, Eq, PartialEq, Copy, Clone)]
pub struct FieldNumberRange {
    /// First number
    pub from: i32,
    /// Inclusive
    pub to: i32,
}

/// A protobuf enumerator
#[derive(Debug, Clone)]
pub struct Enumeration {
    /// enum name
    pub name: String,
    /// enum options
    pub options: Vec<ProtobufOption>,
    /// Message reserved numbers
    /// TODO: use RangeInclusive once stable
    pub reserved_nums: Vec<FieldNumberRange>,
    /// Message reserved names
    pub reserved_names: Vec<String>,
    /// enum values
    pub values: Vec<EnumValue>,
}

/// A protobuf enumeration field
#[derive(Debug, Clone)]
pub struct EnumValue {
    /// enum value name
    pub name: String,
    /// enum value number
    pub number: i32,
}

/// A OneOf
#[derive(Debug, Clone, Default)]
pub struct OneOf {
    /// OneOf name
    pub name: String,
    /// OneOf fields
    pub fields: Vec<Field>,
}

#[derive(Debug, Clone)]
pub struct Extend {
    /// Extend this type with field
    pub extendee: String,
    /// Extension field
    pub field: Field,
}

/// Service definition
#[derive(Debug, Clone)]
pub struct Service {
    /// Service name
    pub name: String,
    pub options: Vec<ProtobufOption>,
    pub methods: Vec<Method>,
}

/// Service method
#[derive(Debug, Clone)]
pub struct Method {
    /// Method name
    pub name: String,
    /// Input type
    pub input_type: String,
    /// Output type
    pub output_type: String,
    /// If this method is client streaming
    pub client_streaming: bool,
    /// If this method is server streaming
    pub server_streaming: bool,
    /// Method options
    pub options: Vec<ProtobufOption>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ProtobufOption {
    pub name: String,
    pub value: UninterpretedOptionValue,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UninterpretedOptionValue {
    U64(u64),
    I64(i64),
    F64(f64), // TODO: eq
    Ident(String),
    String(String),
    BracedExpr(String),
}
