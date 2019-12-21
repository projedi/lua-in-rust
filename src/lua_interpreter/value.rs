use crate::lua_lexemes::StringLiteral;

#[derive(Debug, Clone)]
pub struct TableRef<'s, 'g>(
    std::marker::PhantomData<&'s ()>,
    std::marker::PhantomData<&'g ()>,
);

impl<'s, 'g> TableRef<'s, 'g> {
    pub fn make(rows: &[Value<'s, 'g>]) -> TableRef<'s, 'g> {
        // TODO: Implement.
        eprintln!("TableRef::make {:?}", rows);
        TableRef(std::marker::PhantomData, std::marker::PhantomData)
    }

    pub fn get(&self, key: Value<'s, 'g>) -> Value<'s, 'g> {
        // TODO: Implement.
        eprintln!("TableRef::get {:?} {:?}", self, key);
        Value::from_nil()
    }

    pub fn set(&mut self, key: Value<'s, 'g>, value: Value<'s, 'g>) {
        // TODO: Implement.
        eprintln!("TableRef::set {:?} {:?} {:?}", self, key, value);
    }

    pub fn get_metatable(&self) -> Option<TableRef<'s, 'g>> {
        // TODO: Implement
        eprintln!("TableRef::get_metatable {:?}", self);
        None
    }

    pub fn set_metatable(&mut self, mt: Option<TableRef<'s, 'g>>) {
        // TODO: Implement
        eprintln!("TableRef::set_metatable {:?} {:?}", self, mt);
    }

    pub fn len(&self) -> Number {
        // TODO: Implement
        eprintln!("TableRef::len {:?}", self);
        0.0
    }
}

#[derive(Debug, Clone)]
struct StringRef<'g>(std::marker::PhantomData<&'g ()>);

impl<'g> StringRef<'g> {
    fn to_number(&self) -> Option<Number> {
        // TODO: Implement.
        eprintln!("StringRef::to_number {:?}", self);
        None
    }

    fn from_number(n: Number) -> StringRef<'g> {
        // TODO: Implement.
        eprintln!("StringRef::from_number {:?}", n);
        StringRef(std::marker::PhantomData)
    }

    fn len(&self) -> Number {
        // TODO: Implement
        eprintln!("StringRef::len {:?}", self);
        0.0
    }

    fn concat_ss<'s1, 's2>(s1: &'s1 str, s2: &'s2 str) -> StringRef<'g> {
        // TODO: Implement
        eprintln!("StringRef::concat_ss {:?} {:?}", s1, s2);
        StringRef(std::marker::PhantomData)
    }

    fn concat_sd<'s>(s1: &'s str, s2: StringRef<'g>) -> StringRef<'g> {
        // TODO: Implement
        eprintln!("StringRef::concat_sd {:?} {:?}", s1, s2);
        StringRef(std::marker::PhantomData)
    }

    fn concat_ds<'s>(s1: StringRef<'g>, s2: &'s str) -> StringRef<'g> {
        // TODO: Implement
        eprintln!("StringRef::concat_ds {:?} {:?}", s1, s2);
        StringRef(std::marker::PhantomData)
    }

    fn concat_dd(s1: StringRef<'g>, s2: StringRef<'g>) -> StringRef<'g> {
        // TODO: Implement
        eprintln!("StringRef::concat_dd {:?} {:?}", s1, s2);
        StringRef(std::marker::PhantomData)
    }
}

#[derive(Debug, Clone)]
pub struct Varargs<'s, 'g>(
    std::marker::PhantomData<&'s ()>,
    std::marker::PhantomData<&'g ()>,
);

impl<'s, 'g> Varargs<'s, 'g> {
    fn make(vals: &[Value<'s, 'g>]) -> Varargs<'s, 'g> {
        // TODO: Implement
        eprintln!("Varargs::make {:?}", vals);
        Varargs(std::marker::PhantomData, std::marker::PhantomData)
    }

    fn get(&self) -> Vec<Value<'s, 'g>> {
        // TODO: Implement
        eprintln!("Varargs::get {:?}", self);
        vec![]
    }
}

#[derive(Debug, Clone)]
pub struct FunctionRef<'s, 'g>(
    std::marker::PhantomData<&'s ()>,
    std::marker::PhantomData<&'g ()>,
);

#[derive(Debug, Clone)]
pub struct FunctionInput<'s, 'g> {
    pub global_env: TableRef<'s, 'g>,
    pub captured_env: TableRef<'s, 'g>,
    // Starts with function parameters.
    pub local_env: TableRef<'s, 'g>,
    pub varargs: Varargs<'s, 'g>,
}

impl<'s, 'g> FunctionRef<'s, 'g> {
    fn make(
        f: impl Fn(FunctionInput<'s, 'g>) -> Vec<Value<'s, 'g>> + 'g,
        input: FunctionInput<'s, 'g>,
    ) -> FunctionRef<'s, 'g> {
        // TODO: Implement
        eprintln!("FunctionRef::make {:?}", input);
        FunctionRef(std::marker::PhantomData, std::marker::PhantomData)
    }

    fn get_global_env(&self) -> TableRef<'s, 'g> {
        // TODO: Implement
        eprintln!("FunctionRef::get_env {:?}", self);
        TableRef::make(&[])
    }

    fn set_global_env(&mut self, env: TableRef<'s, 'g>) {
        // TODO: Implement
        eprintln!("FunctionRef::set_env {:?} {:?}", self, env);
    }
}

pub type Number = f64;

#[derive(Debug, Clone)]
enum StringEnum<'s, 'g> {
    StaticString(StringLiteral<'s>),
    DynamicString(StringRef<'g>),
}

#[derive(Debug, Clone)]
pub struct String<'s, 'g>(StringEnum<'s, 'g>);

impl<'s, 'g> String<'s, 'g> {
    pub fn from_string_literal(s: StringLiteral<'s>) -> String<'s, 'g> {
        String(StringEnum::StaticString(s))
    }

    pub fn to_number(&self) -> Option<Number> {
        match &self.0 {
            StringEnum::StaticString(s) => match s.get_str().parse::<Number>() {
                Ok(n) => Some(n),
                Err(_) => None,
            },
            StringEnum::DynamicString(s) => s.to_number(),
        }
    }

    pub fn from_number(n: Number) -> String<'s, 'g> {
        String(StringEnum::DynamicString(StringRef::from_number(n)))
    }

    pub fn concat(l: String<'s, 'g>, r: String<'s, 'g>) -> String<'s, 'g> {
        match (l.0, r.0) {
            (StringEnum::StaticString(s1), StringEnum::StaticString(s2)) => String(
                StringEnum::DynamicString(StringRef::concat_ss(s1.get_str(), s2.get_str())),
            ),
            (StringEnum::StaticString(s1), StringEnum::DynamicString(s2)) => String(
                StringEnum::DynamicString(StringRef::concat_sd(s1.get_str(), s2)),
            ),
            (StringEnum::DynamicString(s1), StringEnum::StaticString(s2)) => String(
                StringEnum::DynamicString(StringRef::concat_ds(s1, s2.get_str())),
            ),
            (StringEnum::DynamicString(s1), StringEnum::DynamicString(s2)) => {
                String(StringEnum::DynamicString(StringRef::concat_dd(s1, s2)))
            }
        }
    }

    pub fn len(&self) -> Number {
        match &self.0 {
            StringEnum::StaticString(s) => s.get_str().len() as Number,
            StringEnum::DynamicString(s) => s.len(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ValueEnum<'s, 'g> {
    Nil,
    Bool(bool),
    Number(Number),
    String(String<'s, 'g>),
    Function(FunctionRef<'s, 'g>),
    Table(TableRef<'s, 'g>),
    // TODO: userdata, thread.
}

#[derive(Debug, Clone)]
pub enum Type {
    Nil,
    Bool,
    Number,
    String,
    Function,
    Table,
}

#[derive(Debug, Clone)]
pub struct Value<'s, 'g>(ValueEnum<'s, 'g>);

impl<'s, 'g> Value<'s, 'g> {
    pub fn from_nil() -> Value<'s, 'g> {
        Value(ValueEnum::Nil)
    }

    pub fn from_bool(b: bool) -> Value<'s, 'g> {
        Value(ValueEnum::Bool(b))
    }

    pub fn from_number(n: Number) -> Value<'s, 'g> {
        Value(ValueEnum::Number(n))
    }

    pub fn from_string(s: String<'s, 'g>) -> Value<'s, 'g> {
        Value(ValueEnum::String(s))
    }

    pub fn from_function(f: FunctionRef<'s, 'g>) -> Value<'s, 'g> {
        Value(ValueEnum::Function(f))
    }

    pub fn from_table(t: TableRef<'s, 'g>) -> Value<'s, 'g> {
        Value(ValueEnum::Table(t))
    }

    pub fn get_type(&self) -> Type {
        match self.0 {
            ValueEnum::Nil => Type::Nil,
            ValueEnum::Bool(_) => Type::Bool,
            ValueEnum::Number(_) => Type::Number,
            ValueEnum::String(_) => Type::String,
            ValueEnum::Function(_) => Type::Function,
            ValueEnum::Table(_) => Type::Table,
        }
    }

    pub fn is_nil(&self) -> bool {
        match self.0 {
            ValueEnum::Nil => true,
            _ => false,
        }
    }

    pub fn to_bool(&self) -> bool {
        match self.0 {
            ValueEnum::Nil => false,
            ValueEnum::Bool(b) => b,
            _ => true,
        }
    }

    pub fn to_number(&self) -> Option<Number> {
        match &self.0 {
            ValueEnum::Number(n) => Some(*n),
            ValueEnum::String(s) => s.to_number(),
            _ => None,
        }
    }

    pub fn to_string(&self) -> Option<String<'s, 'g>> {
        match &self.0 {
            ValueEnum::Number(n) => Some(String::from_number(*n)),
            ValueEnum::String(s) => Some(s.clone()),
            _ => None,
        }
    }

    pub fn as_table(&self) -> Option<&TableRef<'s, 'g>> {
        match &self.0 {
            ValueEnum::Table(t) => Some(t),
            _ => None,
        }
    }

    pub fn as_function(&self) -> Option<&FunctionRef<'s, 'g>> {
        match &self.0 {
            ValueEnum::Function(f) => Some(f),
            _ => None,
        }
    }
}

pub fn adjust_values<'s, 'g>(mut vals: Vec<Value<'s, 'g>>, count: usize) -> Vec<Value<'s, 'g>> {
    vals.resize_with(count, Value::from_nil);
    vals
}

pub fn adjust_values_1<'s, 'g>(vals: Vec<Value<'s, 'g>>) -> Value<'s, 'g> {
    match vals.into_iter().next() {
        Some(val) => val,
        None => Value::from_nil(),
    }
}
