use super::token::Token;
use super::tokenizer::Tokenizer;
use super::tokenizer::TokenizerError;
use crate::ast::*;

/// Basic information about parsing error.
#[derive(Debug)]
pub enum ParserError {
    TokenizerError(TokenizerError),
    IncorrectInput, // TODO: something better than this
    ExpectConstant,
    IntegerOverflow,
    UnknownSyntax,
    LabelNotAllowed,
    LabelRequired,
    GroupNameShouldStartWithUpperCase,
    MapFieldNotAllowed,
}

impl From<TokenizerError> for ParserError {
    fn from(e: TokenizerError) -> Self {
        ParserError::TokenizerError(e)
    }
}

pub type ParserResult<T> = Result<T, ParserError>;

#[derive(Debug)]
pub struct ParserErrorWithLocation {
    pub error: ParserError,
    /// 1-based
    pub line: u32,
    /// 1-based
    pub col: u32,
}

#[derive(Clone)]
pub struct Parser<'a> {
    pub tokenizer: Tokenizer<'a>,
    syntax: Syntax,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Parser<'a> {
        Parser {
            tokenizer: Tokenizer::new(input),
            syntax: Syntax::Proto2,
        }
    }

    // Proto file
    // proto = syntax { import | package | option | topLevelDef | emptyStatement }
    // topLevelDef = message | enum | extend | service
    pub fn next_proto(&mut self) -> ParserResult<FileDescriptor> {
        let syntax = self.next_syntax_opt()?.unwrap_or(Syntax::Proto2);
        self.syntax = syntax;

        let mut package = String::new();
        let mut imports = Vec::new();
        let mut options = Vec::new();
        let mut messages = Vec::new();
        let mut enums = Vec::new();
        let mut extends = Vec::new();
        let mut services = Vec::new();

        // parse top level statement
        while !self.tokenizer.syntax_eof()? {
            // emptyStatment
            if self.tokenizer.next_symbol_if_eq(';')? {
                continue;
            }

            if let Some(next_package) = self.next_package_opt()? {
                package = next_package.to_owned();
                continue;
            }

            if let Some(import) = self.next_import_opt()? {
                imports.push(import);
                continue;
            }

            if let Some(option) = self.next_option_opt()? {
                options.push(option);
                continue;
            }

            if let Some(enumeration) = self.next_enum_opt()? {
                enums.push(enumeration);
                continue;
            }

            if let Some(message) = self.next_message_opt()? {
                messages.push(message);
                continue;
            }

            if let Some(extend) = self.next_extend_opt()? {
                extends.extend(extend);
                continue;
            }

            if let Some(service) = self.next_service_opt()? {
                services.push(service);
                continue;
            }

            return Err(ParserError::IncorrectInput);
        }
        Ok(FileDescriptor {
            syntax,
            package,
            imports,
            options,
            enums,
            messages,
            extends,
            services,
        })
    }

    // Syntax
    // syntax = "syntax" "=" quote "proto2" quote ";" (proto2 only)
    // syntax = "syntax" "=" quote "proto3" quote ";" (proto3 only)
    pub fn next_syntax_opt(&mut self) -> ParserResult<Option<Syntax>> {
        if !self.tokenizer.next_ident_if_eq("syntax")? {
            return Ok(None);
        }
        self.tokenizer.next_symbol_expect_eq('=')?;
        let syntax_str = self.tokenizer.next_str_lit()?;
        let syntax = if syntax_str == "proto2" {
            Syntax::Proto2
        } else if syntax_str == "proto3" {
            Syntax::Proto3
        } else {
            return Err(ParserError::UnknownSyntax);
        };
        self.tokenizer.next_symbol_expect_eq(';')?;
        Ok(Some(syntax))
    }

    // Package
    // package = "package" fullIdent ";"
    pub fn next_package_opt(&mut self) -> ParserResult<Option<String>> {
        if !self.tokenizer.next_ident_if_eq("package")? {
            return Ok(None);
        }
        let package = self.next_full_ident()?;
        self.tokenizer.next_symbol_expect_eq(';')?;
        Ok(Some(package))
    }

    // fullIdent = ident { "." ident }
    fn next_full_ident(&mut self) -> ParserResult<String> {
        let mut full_ident = String::new();
        full_ident.push_str(&self.tokenizer.next_ident()?);
        while self.tokenizer.next_symbol_if_eq('.')? {
            full_ident.push('.');
            full_ident.push_str(&self.tokenizer.next_ident()?);
        }
        Ok(full_ident)
    }

    // Import Statement
    // import = "import" [ "weak" | "public" ] strLit ";"
    pub fn next_import_opt(&mut self) -> ParserResult<Option<String>> {
        if !self.tokenizer.next_ident_if_eq("import")? {
            return Ok(None);
        }
        // TODO: support "weak" and "public"
        if self
            .tokenizer
            .lookahead_ident_if_in(&["weak", "public"])?
            .is_some()
        {
            self.tokenizer.advance()?;
        }
        let import_path = self.tokenizer.next_str_lit()?;
        self.tokenizer.next_symbol_expect_eq(';')?;
        Ok(Some(import_path))
    }

    // Option
    // option = "option" optionName "=" constant ";"
    pub fn next_option_opt(&mut self) -> ParserResult<Option<ProtobufOption>> {
        if !self.tokenizer.next_ident_if_eq("option")? {
            return Ok(None);
        }
        let option = self.next_option_assignment()?;
        self.tokenizer.next_symbol_expect_eq(';')?;
        Ok(Some(option))
    }

    // optionName "=" constant
    fn next_option_assignment(&mut self) -> ParserResult<ProtobufOption> {
        let name = self.next_option_name()?;
        self.tokenizer.next_symbol_expect_eq('=')?;
        let value = self.next_uninterpreted_option_value()?;
        Ok(ProtobufOption { name, value })
    }

    // optionName = ( ident | "(" fullIdent ")" ) { "." ident }
    fn next_option_name(&mut self) -> ParserResult<String> {
        let mut option_name = String::new();
        option_name.push_str(&self.next_option_name_part()?);
        while self.tokenizer.next_symbol_if_eq('.')? {
            option_name.push('.');
            option_name.push_str(&self.next_option_name_part()?);
        }
        Ok(option_name)
    }

    // https://github.com/google/protobuf/issues/4563
    fn next_option_name_part(&mut self) -> ParserResult<String> {
        let mut option_name_part = String::new();
        if self.tokenizer.next_symbol_if_eq('(')? {
            option_name_part.push('(');
            // from protoc ParserOptionNamePart
            // An extension name consists of dot-separated identifiers,
            // and may begin with a dot.
            if self.tokenizer.next_symbol_if_eq('.')? {
                option_name_part.push('.');
            }
            option_name_part.push_str(&self.tokenizer.next_ident()?);
            while self.tokenizer.next_symbol_if_eq('.')? {
                option_name_part.push('.');
                option_name_part.push_str(&self.tokenizer.next_ident()?);
            }
            self.tokenizer.next_symbol_expect_eq(')')?;
            option_name_part.push(')');
        } else {
            option_name_part.push_str(&self.tokenizer.next_ident()?);
        }
        Ok(option_name_part)
    }

    // Constant
    // constant = fullIdent | ( [ "-" | "+" ] intLit ) | ( [ "-" | "+" ] floatLit ) | strLit | boolLit
    //
    // protoc does not support '+' and use 'Ident' instead of 'fullIdent'
    fn next_uninterpreted_option_value(&mut self) -> ParserResult<UninterpretedOptionValue> {
        match self.tokenizer.lookahead_some()? {
            Token::IntLit(..) | Token::FloatLit(..) => {
                return self.next_num_lit(true);
            }
            Token::Ident(..) => {
                // use 'Ident' instead of 'fullIdent'
                return Ok(UninterpretedOptionValue::Ident(
                    self.tokenizer.next_ident()?,
                ));
            }
            Token::StrLit(..) => {
                return Ok(UninterpretedOptionValue::String(
                    self.tokenizer.next_str_lit()?,
                ))
            }
            // protoc does not support '+'
            Token::Symbol('-') => {
                self.tokenizer.advance()?;
                return Ok(self.next_num_lit(false)?);
            }
            Token::Symbol('{') => {
                // https://github.com/google/protobuf/blob/a21f225824e994ebd35e8447382ea4e0cd165b3c/src/google/protobuf/unittest_custom_options.proto#L350
                self.tokenizer.advance()?;
                return Ok(UninterpretedOptionValue::BracedExpr(
                    self.next_brace_expr()?,
                ));
            }
            _ => Err(ParserError::ExpectConstant),
        }
    }

    fn next_num_lit(&mut self, sign_is_plus: bool) -> ParserResult<UninterpretedOptionValue> {
        /// Negate `u64` checking for overflow.
        pub fn neg(value: u64) -> Result<i64, ParserError> {
            if value <= 0x7fff_ffff_ffff_ffff {
                Ok(-(value as i64))
            } else if value == 0x8000_0000_0000_0000 {
                Ok(-0x8000_0000_0000_0000)
            } else {
                Err(ParserError::IntegerOverflow)
            }
        }

        Ok(self.tokenizer.next_token_expect_map(|token| match token {
            &Token::IntLit(i) => {
                let u = if sign_is_plus {
                    UninterpretedOptionValue::U64(i)
                } else {
                    UninterpretedOptionValue::I64(neg(i)?)
                };
                return Ok(u);
            }
            &Token::FloatLit(f) => {
                let f = if sign_is_plus {
                    UninterpretedOptionValue::F64(f)
                } else {
                    UninterpretedOptionValue::F64(-f)
                };
                return Ok(f);
            }
            _ => return Err(ParserError::IncorrectInput),
        })?)
    }

    // Note that enclosing braces are not added to *value.
    fn next_brace_expr(&mut self) -> ParserResult<String> {
        let mut r = String::new();
        let mut brace_depth = 1;
        loop {
            if self.tokenizer.lookahead_symbol_if_eq('{')? {
                brace_depth += 1;
            } else if self.tokenizer.lookahead_symbol_if_eq('}')? {
                brace_depth -= 1;
                if brace_depth == 0 {
                    self.tokenizer.advance()?;
                    return Ok(r);
                }
            }
            if !r.is_empty() {
                r.push(' ');
            }
            r.push_str(&self.tokenizer.next_some()?.to_string());
        }
    }

    // Enum definition
    // enum = "enum" enumName enumBody
    // enumBody = "{" { option | enumField | emptyStatement } "}"
    pub fn next_enum_opt(&mut self) -> ParserResult<Option<Enumeration>> {
        if !self.tokenizer.next_ident_if_eq("enum")? {
            return Ok(None);
        }

        let name = self.tokenizer.next_ident()?.to_owned();
        let mut options = Vec::new();
        let mut reserved_nums = Vec::new();
        let mut reserved_names = Vec::new();
        let mut values = Vec::new();

        self.tokenizer.next_symbol_expect_eq('{')?;
        while !self.tokenizer.lookahead_symbol_if_eq('}')? {
            // emptyStatment
            if self.tokenizer.next_symbol_if_eq(';')? {
                continue;
            }

            if let Some(o) = self.next_option_opt()? {
                options.push(o);
                continue;
            }

            if let Some((field_nums, field_names)) = self.next_reserved_opt()? {
                reserved_nums.extend(field_nums);
                reserved_names.extend(field_names);
                continue;
            }

            values.push(self.next_enum_field()?);
        }
        self.tokenizer.advance()?;

        // TODO: validate enum
        Ok(Some(Enumeration {
            name,
            values,
            options,
            reserved_nums,
            reserved_names,
        }))
    }

    // Grammar is incorrect: https://github.com/google/protobuf/issues/4558
    // reserved = "reserved" ( ranges | fieldNames ) ";"
    // fieldNames = fieldName { "," fieldName }
    fn next_reserved_opt(&mut self) -> ParserResult<Option<(Vec<FieldNumberRange>, Vec<String>)>> {
        if !self.tokenizer.next_ident_if_eq("reserved")? {
            return Ok(None);
        }

        let (ranges, names) = if let &Token::StrLit(..) = self.tokenizer.lookahead_some()? {
            let mut names = Vec::new();
            names.push(self.tokenizer.next_str_lit()?);
            while self.tokenizer.next_symbol_if_eq(',')? {
                names.push(self.tokenizer.next_str_lit()?);
            }
            (Vec::new(), names)
        } else {
            (self.next_ranges()?, Vec::new())
        };

        self.tokenizer.next_symbol_expect_eq(';')?;

        Ok(Some((ranges, names)))
    }

    // ranges = range { "," range }
    fn next_ranges(&mut self) -> ParserResult<Vec<FieldNumberRange>> {
        let mut ranges = Vec::new();
        ranges.push(self.next_range()?);
        while self.tokenizer.next_symbol_if_eq(',')? {
            ranges.push(self.next_range()?);
        }
        Ok(ranges)
    }

    // range =  intLit [ "to" ( intLit | "max" ) ]
    fn next_range(&mut self) -> ParserResult<FieldNumberRange> {
        let from = self.next_signed_integer()?;
        let to = if self.tokenizer.next_ident_if_eq("to")? {
            if self.tokenizer.next_ident_if_eq("max")? {
                i32::max_value()
            } else {
                self.next_signed_integer()?
            }
        } else {
            from
        };
        Ok(FieldNumberRange { from, to })
    }

    // equal to protoc's ConsumeSignedInteger
    fn next_signed_integer(&mut self) -> ParserResult<i32> {
        let minus = self.tokenizer.next_symbol_if_eq('-')?;
        let lit = self.tokenizer.next_int_lit()?;
        // protoc limit enum value to i32
        Ok(if minus {
            let unsigned = lit.to_i64()?;
            match unsigned.checked_neg() {
                Some(neg) => neg.to_i32()?,
                None => return Err(ParserError::IntegerOverflow),
            }
        } else {
            lit.to_i32()?
        })
    }

    // enumField = ident "=" intLit [ "[" enumValueOption { ","  enumValueOption } "]" ]";"
    // enumValueOption = optionName "=" constant
    fn next_enum_field(&mut self) -> ParserResult<EnumValue> {
        let name = self.tokenizer.next_ident()?.to_owned();
        self.tokenizer.next_symbol_expect_eq('=')?;
        let number = self.next_enum_value()?;

        if self.tokenizer.next_symbol_if_eq('[')? {
            self.next_option_assignment()?;
            while self.tokenizer.next_symbol_if_eq(',')? {
                self.next_option_assignment()?;
            }
            self.tokenizer.next_symbol_expect_eq(']')?;
        }
        self.tokenizer.next_symbol_expect_eq(';')?;
        Ok(EnumValue { name, number })
    }

    // https://github.com/google/protobuf/issues/4561
    fn next_enum_value(&mut self) -> ParserResult<i32> {
        self.next_signed_integer()
    }

    // Message definition
    // message = "message" messageName messageBody
    pub fn next_message_opt(&mut self) -> ParserResult<Option<Message>> {
        if !self.tokenizer.next_ident_if_eq("message")? {
            return Ok(None);
        }
        let name = self.tokenizer.next_ident()?.to_owned();

        let mode = match self.syntax {
            Syntax::Proto2 => MessageBodyParseMode::MessageProto2,
            Syntax::Proto3 => MessageBodyParseMode::MessageProto3,
        };

        Ok(Some(Message {
            name,
            ..self.next_message_body(mode)?
        }))
    }

    // message body without name
    // messageBody = "{" { field | enum | message | extend | extensions | group |
    //               option | oneof | mapField | reserved | emptyStatement } "}"
    fn next_message_body(&mut self, mode: MessageBodyParseMode) -> ParserResult<Message> {
        self.tokenizer.next_symbol_expect_eq('{')?;
        let mut r = Message::default();
        while !self.tokenizer.lookahead_symbol_if_eq('}')? {
            // emptyStatement
            if self.tokenizer.next_symbol_if_eq(';')? {
                continue;
            }

            if mode.is_most_non_fields_allowed() {
                if let Some((field_nums, field_names)) = self.next_reserved_opt()? {
                    r.reserved_nums.extend(field_nums);
                    r.reserved_names.extend(field_names);
                    continue;
                }

                if let Some(oneof) = self.next_oneof_opt()? {
                    r.oneofs.push(oneof);
                    continue;
                }

                if let Some(_extensions) = self.next_extensions_opt()? {
                    continue;
                }

                if let Some(_extend) = self.next_extend_opt()? {
                    continue;
                }

                if let Some(nested_message) = self.next_message_opt()? {
                    r.messages.push(nested_message);
                    continue;
                }

                if let Some(nested_enum) = self.next_enum_opt()? {
                    r.enums.push(nested_enum);
                    continue;
                }
            } else {
                if let Some(..) = self.tokenizer.lookahead_ident_if_in(&[
                    "reserved",
                    "oneof",
                    "extensions",
                    "extend",
                    "message",
                    "enum",
                ])? {
                    return Err(ParserError::IncorrectInput);
                }
            }

            if mode.is_option_allowed() {
                if let Some(option) = self.next_option_opt()? {
                    r.options.push(option);
                    continue;
                }
            } else {
                if self.tokenizer.lookahead_ident_if_eq("option")? {
                    return Err(ParserError::IncorrectInput);
                }
            }

            r.fields.push(self.next_field(mode)?);
        }
        self.tokenizer.advance()?;

        Ok(r)
    }

    // Field
    // field = label type fieldName "=" fieldNumber [ "[" fieldOptions "]" ] ";"
    // group = label "group" groupName "=" fieldNumber messageBody
    fn next_field(&mut self, mode: MessageBodyParseMode) -> ParserResult<Field> {
        let rule = if self.tokenizer.lookahead_ident_if_eq("map")? {
            if !mode.map_allowed() {
                return Err(ParserError::MapFieldNotAllowed);
            }
            FieldLabel::Optional
        } else {
            self.next_label(mode)?
        };
        if self.tokenizer.next_ident_if_eq("group")? {
            let name = self.next_group_name()?.to_owned();
            self.tokenizer.next_symbol_expect_eq('=')?;
            let number = self.next_field_number()?;

            let mode = match self.syntax {
                Syntax::Proto2 => MessageBodyParseMode::MessageProto2,
                Syntax::Proto3 => MessageBodyParseMode::MessageProto3,
            };

            let Message { fields, .. } = self.next_message_body(mode)?;
            Ok(Field {
                name,
                rule,
                typ: FieldType::Group(fields),
                number,
                options: Vec::new(),
            })
        } else {
            let typ = self.next_field_type()?;
            let name = self.tokenizer.next_ident()?.to_owned();
            self.tokenizer.next_symbol_expect_eq('=')?;
            let number = self.next_field_number()?;

            let mut options = Vec::new();

            if self.tokenizer.next_symbol_if_eq('[')? {
                for o in self.next_field_options()? {
                    options.push(o);
                }
                self.tokenizer.next_symbol_expect_eq(']')?;
            }
            self.tokenizer.next_symbol_expect_eq(';')?;
            Ok(Field {
                name,
                rule,
                typ,
                number,
                options,
            })
        }
    }

    // label = "required" | "optional" | "repeated"
    fn next_label(&mut self, mode: MessageBodyParseMode) -> ParserResult<FieldLabel> {
        let map = &[
            ("optional", FieldLabel::Optional),
            ("required", FieldLabel::Required),
            ("repeated", FieldLabel::Repeated),
        ];

        for &(name, value) in map {
            let mut clone = self.clone();
            if clone.tokenizer.next_ident_if_eq(name)? {
                if !mode.label_allowed(value) {
                    return Err(ParserError::LabelNotAllowed);
                }
                *self = clone;
                return Ok(value);
            }
        }

        if mode.some_label_required() {
            Err(ParserError::LabelRequired)
        } else {
            Ok(FieldLabel::Optional)
        }
    }

    // groupName = capitalLetter { letter | decimalDigit | "_" } (proto2 only)
    fn next_group_name(&mut self) -> ParserResult<String> {
        // lexer cannot distinguish between group name and other ident
        let mut clone = self.clone();
        let ident = clone.tokenizer.next_ident()?;
        if !ident.chars().next().unwrap().is_ascii_uppercase() {
            return Err(ParserError::GroupNameShouldStartWithUpperCase);
        }
        *self = clone;
        Ok(ident)
    }

    // type = "double" | "float" | "int32" | "int64" | "uint32" | "uint64"
    //   | "sint32" | "sint64" | "fixed32" | "fixed64" | "sfixed32" | "sfixed64"
    //   | "bool" | "string" | "bytes" | messageType | enumType
    fn next_field_type(&mut self) -> ParserResult<FieldType> {
        let simple = &[
            ("int32", FieldType::Int32),
            ("int64", FieldType::Int64),
            ("uint32", FieldType::Uint32),
            ("uint64", FieldType::Uint64),
            ("sint32", FieldType::Sint32),
            ("sint64", FieldType::Sint64),
            ("fixed32", FieldType::Fixed32),
            ("sfixed32", FieldType::Sfixed32),
            ("fixed64", FieldType::Fixed64),
            ("sfixed64", FieldType::Sfixed64),
            ("bool", FieldType::Bool),
            ("string", FieldType::String),
            ("bytes", FieldType::Bytes),
            ("float", FieldType::Float),
            ("double", FieldType::Double),
        ];
        for &(ref n, ref t) in simple {
            if self.tokenizer.next_ident_if_eq(n)? {
                return Ok(t.clone());
            }
        }

        if let Some(t) = self.next_map_field_type_opt()? {
            return Ok(t);
        }

        let message_or_enum = self.next_message_or_enum_type()?;
        Ok(FieldType::MessageOrEnum(message_or_enum))
    }

    // fieldNumber = intLit;
    fn next_field_number(&mut self) -> ParserResult<i32> {
        self.tokenizer.next_token_expect_map(|token| match token {
            // protoc limit field number to i32
            &Token::IntLit(i) => i.to_i32(),
            _ => Err(ParserError::IncorrectInput),
        })
    }

    // fieldOptions = fieldOption { ","  fieldOption }
    // fieldOption = optionName "=" constant
    fn next_field_options(&mut self) -> ParserResult<Vec<ProtobufOption>> {
        let mut options = Vec::new();
        options.push(self.next_option_assignment()?);

        while self.tokenizer.next_symbol_if_eq(',')? {
            options.push(self.next_option_assignment()?);
        }
        Ok(options)
    }

    // oneof = "oneof" oneofName "{" { oneofField | emptyStatement } "}"
    // oneofField = type fieldName "=" fieldNumber [ "[" fieldOptions "]" ] ";"
    fn next_oneof_opt(&mut self) -> ParserResult<Option<OneOf>> {
        if !self.tokenizer.next_ident_if_eq("oneof")? {
            return Ok(None);
        }
        let name = self.tokenizer.next_ident()?.to_owned();
        let Message { fields, .. } = self.next_message_body(MessageBodyParseMode::Oneof)?;
        Ok(Some(OneOf { name, fields }))
    }

    // Map
    // mapField = "map" "<" keyType "," type ">" mapName "=" fieldNumber [ "[" fieldOptions "]" ] ";"
    // keyType = "int32" | "int64" | "uint32" | "uint64" | "sint32" | "sint64" |
    //           "fixed32" | "fixed64" | "sfixed32" | "sfixed64" | "bool" | "string"
    fn next_map_field_type_opt(&mut self) -> ParserResult<Option<FieldType>> {
        if !self.tokenizer.next_ident_if_eq("map")? {
            return Ok(None);
        }

        self.tokenizer.next_symbol_expect_eq('<')?;
        // TODO: restrict key types
        let key = self.next_field_type()?;
        self.tokenizer.next_symbol_expect_eq(',')?;
        let value = self.next_field_type()?;
        self.tokenizer.next_symbol_expect_eq('>')?;
        Ok(Some(FieldType::Map(Box::new((key, value)))))
    }

    // Extensions and Reserved
    // extensions = "extensions" ranges ";"
    fn next_extensions_opt(&mut self) -> ParserResult<Option<Vec<FieldNumberRange>>> {
        if !self.tokenizer.next_ident_if_eq("extensions")? {
            return Ok(None);
        }
        Ok(Some(self.next_ranges()?))
    }

    // Extend
    // extend = "extend" messageType "{" {field | group | emptyStatement} "}"
    pub fn next_extend_opt(&mut self) -> ParserResult<Option<Vec<Extend>>> {
        let mut clone = self.clone();
        if !clone.tokenizer.next_ident_if_eq("extend")? {
            return Ok(None);
        }

        // According to spec `extend` is only for `proto2`, but it is used in `proto3`
        // https://github.com/google/protobuf/issues/4610
        *self = clone;
        let extendee = self.next_message_or_enum_type()?;
        let mode = match self.syntax {
            Syntax::Proto2 => MessageBodyParseMode::ExtendProto2,
            Syntax::Proto3 => MessageBodyParseMode::ExtendProto3,
        };
        let Message { fields, .. } = self.next_message_body(mode)?;
        let extensions = fields
            .into_iter()
            .map(|field| {
                let extendee = extendee.clone();
                Extend { extendee, field }
            })
            .collect();

        Ok(Some(extensions))
    }

    // Service
    // proto2:
    // service = "service" serviceName "{" { option | rpc | stream | emptyStatement } "}"
    //
    // proto3:
    // service = "service" serviceName "{" { option | rpc | emptyStatement } "}"
    pub fn next_service_opt(&mut self) -> ParserResult<Option<Service>> {
        if !self.tokenizer.next_ident_if_eq("service")? {
            return Ok(None);
        }

        let name = self.tokenizer.next_ident()?;
        let mut options = Vec::new();
        let mut methods = Vec::new();
        self.tokenizer.next_symbol_expect_eq('{')?;
        while !self.tokenizer.lookahead_symbol_if_eq('}')? {
            // emptyStatment
            if self.tokenizer.next_symbol_if_eq(';')? {
                continue;
            }

            if let Some(o) = self.next_option_opt()? {
                options.push(o);
                continue;
            }

            if let Some(method) = self.next_rpc_opt()? {
                methods.push(method);
                continue;
            }

            if self.syntax == Syntax::Proto2 {
                if let Some(method) = self.next_stream_opt()? {
                    methods.push(method);
                    continue;
                }
            }

            return Err(ParserError::IncorrectInput);
        }
        self.tokenizer.advance()?;

        Ok(Some(Service {
            name,
            methods,
            options,
        }))
    }

    // rpc = "rpc" rpcName "(" [ "stream" ] messageType ")"
    //     "returns" "(" [ "stream" ] messageType ")"
    //     (( "{" { option | emptyStatement } "}" ) | ";" )
    pub fn next_rpc_opt(&mut self) -> ParserResult<Option<Method>> {
        if !self.tokenizer.next_ident_if_eq("rpc")? {
            return Ok(None);
        }

        let name = self.tokenizer.next_ident()?;
        self.tokenizer.next_symbol_expect_eq('(')?;
        let client_streaming = self.tokenizer.next_ident_if_eq("stream")?;
        let input_type = self.next_message_or_enum_type()?;
        self.tokenizer.next_symbol_expect_eq(')')?;
        self.tokenizer.next_ident_expect_eq("returns")?;
        self.tokenizer.next_symbol_expect_eq('(')?;
        let server_streaming = self.tokenizer.next_ident_if_eq("stream")?;
        let output_type = self.next_message_or_enum_type()?;
        self.tokenizer.next_symbol_expect_eq(')')?;
        let options = self.next_options_or_colon()?;
        Ok(Some(Method {
            name,
            input_type,
            output_type,
            client_streaming,
            server_streaming,
            options,
        }))
    }

    // messageName = ident
    // enumName = ident
    // messageType = [ "." ] { ident "." } messageName
    // enumType = [ "." ] { ident "." } enumName
    fn next_message_or_enum_type(&mut self) -> ParserResult<String> {
        let mut full_name = String::new();
        if self.tokenizer.next_symbol_if_eq('.')? {
            full_name.push('.');
        }
        full_name.push_str(&self.tokenizer.next_ident()?);
        while self.tokenizer.next_symbol_if_eq('.')? {
            full_name.push('.');
            full_name.push_str(&self.tokenizer.next_ident()?);
        }
        Ok(full_name)
    }

    fn next_options_or_colon(&mut self) -> ParserResult<Vec<ProtobufOption>> {
        let mut options = Vec::new();
        if self.tokenizer.next_symbol_if_eq('{')? {
            while !self.tokenizer.lookahead_symbol_if_eq('}')? {
                // emptyStatment
                if self.tokenizer.next_symbol_if_eq(';')? {
                    continue;
                }

                if let Some(option) = self.next_option_opt()? {
                    options.push(option);
                    continue;
                }

                return Err(ParserError::IncorrectInput);
            }
            self.tokenizer.advance()?;
        } else {
            self.tokenizer.next_symbol_expect_eq(';')?;
        }
        Ok(options)
    }

    // stream = "stream" streamName "(" messageType "," messageType ")"
    //        (( "{" { option | emptyStatement } "}") | ";" )
    fn next_stream_opt(&mut self) -> ParserResult<Option<Method>> {
        assert_eq!(Syntax::Proto2, self.syntax);
        if !self.tokenizer.next_ident_if_eq("stream")? {
            return Ok(None);
        }

        let name = self.tokenizer.next_ident()?;
        self.tokenizer.next_symbol_expect_eq('(')?;
        let input_type = self.tokenizer.next_ident()?;
        self.tokenizer.next_symbol_expect_eq(',')?;
        let output_type = self.tokenizer.next_ident()?;
        self.tokenizer.next_symbol_expect_eq(')')?;
        let options = self.next_options_or_colon()?;
        Ok(Some(Method {
            name,
            input_type,
            output_type,
            client_streaming: true,
            server_streaming: true,
            options,
        }))
    }
}

#[derive(Copy, Clone)]
pub enum MessageBodyParseMode {
    MessageProto2,
    MessageProto3,
    Oneof,
    ExtendProto2,
    ExtendProto3,
}

impl MessageBodyParseMode {
    fn label_allowed(&self, label: FieldLabel) -> bool {
        match label {
            FieldLabel::Repeated => match *self {
                MessageBodyParseMode::MessageProto2
                | MessageBodyParseMode::MessageProto3
                | MessageBodyParseMode::ExtendProto2
                | MessageBodyParseMode::ExtendProto3 => true,
                MessageBodyParseMode::Oneof => false,
            },
            FieldLabel::Optional | FieldLabel::Required => match *self {
                MessageBodyParseMode::MessageProto2 | MessageBodyParseMode::ExtendProto2 => true,
                MessageBodyParseMode::MessageProto3
                | MessageBodyParseMode::ExtendProto3
                | MessageBodyParseMode::Oneof => false,
            },
        }
    }

    fn some_label_required(&self) -> bool {
        match *self {
            MessageBodyParseMode::MessageProto2 | MessageBodyParseMode::ExtendProto2 => true,
            MessageBodyParseMode::MessageProto3
            | MessageBodyParseMode::ExtendProto3
            | MessageBodyParseMode::Oneof => false,
        }
    }

    fn map_allowed(&self) -> bool {
        match *self {
            MessageBodyParseMode::MessageProto2
            | MessageBodyParseMode::MessageProto3
            | MessageBodyParseMode::ExtendProto2
            | MessageBodyParseMode::ExtendProto3 => true,
            MessageBodyParseMode::Oneof => false,
        }
    }

    fn is_most_non_fields_allowed(&self) -> bool {
        match *self {
            MessageBodyParseMode::MessageProto2 | MessageBodyParseMode::MessageProto3 => true,
            MessageBodyParseMode::ExtendProto2
            | MessageBodyParseMode::ExtendProto3
            | MessageBodyParseMode::Oneof => false,
        }
    }

    fn is_option_allowed(&self) -> bool {
        match *self {
            MessageBodyParseMode::MessageProto2
            | MessageBodyParseMode::MessageProto3
            | MessageBodyParseMode::Oneof => true,
            MessageBodyParseMode::ExtendProto2 | MessageBodyParseMode::ExtendProto3 => false,
        }
    }
}

trait ToI32 {
    fn to_i32(&self) -> ParserResult<i32>;
}

trait ToI64 {
    fn to_i64(&self) -> ParserResult<i64>;
}

impl ToI32 for u64 {
    fn to_i32(&self) -> ParserResult<i32> {
        if *self <= i32::max_value() as u64 {
            Ok(*self as i32)
        } else {
            Err(ParserError::IntegerOverflow)
        }
    }
}

impl ToI32 for i64 {
    fn to_i32(&self) -> ParserResult<i32> {
        if *self <= i32::max_value() as i64 && *self >= i32::min_value() as i64 {
            Ok(*self as i32)
        } else {
            Err(ParserError::IntegerOverflow)
        }
    }
}

impl ToI64 for u64 {
    fn to_i64(&self) -> Result<i64, ParserError> {
        if *self <= i64::max_value() as u64 {
            Ok(*self as i64)
        } else {
            Err(ParserError::IntegerOverflow)
        }
    }
}

#[cfg(test)]
mod test {
    use super::Parser;
    use crate::ast::*;

    #[test]
    fn test_syntax() {
        let mut p = Parser::new(
            r#"
            syntax = "proto3";
        "#,
        );
        let res = p.next_syntax_opt().unwrap().unwrap();
        assert_eq!(res, Syntax::Proto3);
    }

    #[test]
    fn text_package() {
        let mut p = Parser::new(
            r#"
            package foo.bar;
        "#,
        );
        let res = p.next_package_opt().unwrap().unwrap();
        assert_eq!(res, "foo.bar");
    }

    #[test]
    fn test_import() {
        let mut p = Parser::new(
            r#"
            import public "other.proto";
        "#,
        );
        let res = p.next_import_opt().unwrap().unwrap();
        assert_eq!(res, "other.proto");
    }

    #[test]
    fn test_option() {
        let mut p = Parser::new(
            r#"
            option java_package = "com.example.foo";
        "#,
        );
        let res = p.next_option_opt().unwrap().unwrap();
        assert_eq!(res.name, "java_package");
        assert_eq!(
            res.value,
            UninterpretedOptionValue::String("com.example.foo".to_owned())
        );
    }

    #[test]
    fn test_enum() {
        let mut p = Parser::new(
            r#"
            enum EnumAllowingAlias {
                option allow_alias = true;
                UNKNOWN = 0;
                STARTED = 1;
                RUNNING = 2 [(custom_option) = "hello world"];
            }
        "#,
        );
        let res = p.next_enum_opt().unwrap().unwrap();
        assert_eq!(res.name, "EnumAllowingAlias");
        assert_eq!(res.values[0].name, "UNKNOWN");
        assert_eq!(res.values[0].number, 0);
        assert_eq!(res.values[1].name, "STARTED");
        assert_eq!(res.values[1].number, 1);
        assert_eq!(res.values[2].name, "RUNNING");
        assert_eq!(res.values[2].number, 2);
        assert_eq!(res.options[0].name, "allow_alias");
        assert_eq!(
            res.options[0].value,
            UninterpretedOptionValue::Ident("true".to_owned())
        );
    }

    #[test]
    fn test_proto2_message() {
        let mut p = Parser::new(
            r#"
            message Outer {
                option (my_option).a = true;
                message Inner {   // Level 2
                    required int64 ival = 1 [default = -8e-28];
                }
                map<int32, string> my_map = 2;
                extensions 20 to 30;
            }
        "#,
        );
        let res = p.next_message_opt().unwrap().unwrap();
        assert_eq!(res.name, "Outer");
        assert_eq!(res.options[0].name, "(my_option).a");
        assert_eq!(
            res.options[0].value,
            UninterpretedOptionValue::Ident("true".to_owned())
        );
        assert_eq!(res.messages[0].name, "Inner");
        assert_eq!(res.messages[0].fields[0].name, "ival");
        assert_eq!(res.messages[0].fields[0].number, 1);
        assert_eq!(res.messages[0].fields[0].typ, FieldType::Int64);
        assert_eq!(res.messages[0].fields[0].options[0].name, "default");
        assert_eq!(
            res.messages[0].fields[0].options[0].value,
            UninterpretedOptionValue::F64(-8e-28)
        );
        assert_eq!(res.fields[0].name, "my_map");
        assert_eq!(res.fields[0].number, 2);
        assert_eq!(
            res.fields[0].typ,
            FieldType::Map(Box::new((FieldType::Int32, FieldType::String)))
        );
    }

    #[test]
    fn test_proto3_message() {
        let mut p = Parser::new(
            r#"
            message Outer {
                option (my_option).a = true;
                message Inner {   // Level 2
                    int64 ival = 1;
                }
                map<int32, string> my_map = 2;
            }
        "#,
        );
        p.syntax = Syntax::Proto3;
        let res = p.next_message_opt().unwrap().unwrap();
        assert_eq!(res.name, "Outer");
        assert_eq!(res.fields[0].name, "my_map");
        assert_eq!(res.fields[0].number, 2);
        assert_eq!(
            res.fields[0].typ,
            FieldType::Map(Box::new((FieldType::Int32, FieldType::String)))
        );
        assert_eq!(res.messages[0].name, "Inner");
        assert_eq!(res.messages[0].fields[0].name, "ival");
        assert_eq!(res.messages[0].fields[0].number, 1);
        assert_eq!(res.messages[0].fields[0].typ, FieldType::Int64);
        assert_eq!(res.options[0].name, "(my_option).a");
        assert_eq!(
            res.options[0].value,
            UninterpretedOptionValue::Ident("true".to_owned())
        );
    }

    #[test]
    fn test_extend() {
        let mut p = Parser::new(
            r#"
            extend Foo {
                optional int32 bar = 126;
            }
        "#,
        );
        let res = p.next_extend_opt().unwrap().unwrap();
        assert_eq!(res[0].extendee, "Foo");
        assert_eq!(res[0].field.name, "bar");
        assert_eq!(res[0].field.number, 126);
        assert_eq!(res[0].field.typ, FieldType::Int32);
    }

    #[test]
    fn test_service() {
        let mut p = Parser::new(
            r#"
            service SearchService {
                rpc Search (SearchRequest) returns (SearchResponse);
            }
        "#,
        );
        let res = p.next_service_opt().unwrap().unwrap();
        assert_eq!(res.name, "SearchService");
        assert_eq!(res.methods[0].name, "Search");
        assert_eq!(res.methods[0].input_type, "SearchRequest");
        assert_eq!(res.methods[0].output_type, "SearchResponse");
    }
}
