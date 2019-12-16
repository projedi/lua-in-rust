#[derive(Debug, Clone)]
pub struct TableRef<'s, 'g>(std::marker::PhantomData<&'s ()>, std::marker::PhantomData<&'g ()>);

impl<'s, 'g> TableRef<'s, 'g> {
    pub fn get(&self, key: Value<'s, 'g>) -> Value<'s, 'g> {
        // TODO: Implement.
        eprintln!("TableRef::get {:?} {:?}", self, key);
        Value::Nil
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

    pub fn len(&self) -> Number {
        // TODO: Implement
        eprintln!("StringRef::len {:?}", self);
        0.0
    }
}

#[derive(Debug, Clone)]
pub struct FunctionRef<'g>(std::marker::PhantomData<&'g ()>);

pub type Number = f64;

#[derive(Debug, Clone)]
pub enum String<'s, 'g> {
    StaticString(&'s str),
    DynamicString(StringRef<'g>),
}

impl<'s, 'g> String<'s, 'g> {
    pub fn from_str(s: &'s str) -> String<'s, 'g> {
        String::StaticString(s)
    }

    pub fn to_number(&self) -> Option<Number> {
        match self {
            String::StaticString(s) => {
                match s.parse::<Number>() {
                    Ok(n) => Some(n),
                    Err(_) => None,
                }
            }
            String::DynamicString(s) => {
                s.to_number()
            }
        }
    }

    pub fn from_number(n: Number) -> String<'s, 'g> {
        String::DynamicString(StringRef::from_number(n))
    }

    pub fn concat(l: String<'s, 'g>, r: String<'s, 'g>) -> String<'s, 'g> {
        // TODO: Implement
        eprintln!("String::concat {:?} {:?}", l, r);
        String::StaticString("")
    }

    pub fn len(&self) -> Number {
        match self {
            String::StaticString(s) => {
                s.len() as Number
            }
            String::DynamicString(s) => {
                s.len()
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum Value<'s, 'g> {
    Nil,
    Bool(bool),
    Number(Number),
    String(String<'s, 'g>),
    Function(FunctionRef<'g>),
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

impl<'s, 'g> Value<'s, 'g> {
    pub fn from_nil() -> Value<'s, 'g> {
        Value::Nil
    }

    pub fn from_bool(b: bool) -> Value<'s, 'g> {
        Value::Bool(b)
    }

    pub fn from_number(n: Number) -> Value<'s, 'g> {
        Value::Number(n)
    }

    pub fn from_string(s: String<'s, 'g>) -> Value<'s, 'g> {
        Value::String(s)
    }

    pub fn from_function(f: FunctionRef<'g>) -> Value<'s, 'g> {
        Value::Function(f)
    }

    pub fn from_table(t: TableRef<'s, 'g>) -> Value<'s, 'g> {
        Value::Table(t)
    }

    pub fn get_type(&self) -> Type {
        match self {
            Value::Nil => Type::Nil,
            Value::Bool(_) => Type::Bool,
            Value::Number(_) => Type::Number,
            Value::String(_) => Type::String,
            Value::Function(_) => Type::Function,
            Value::Table(_) => Type::Table,
        }
    }

    pub fn to_bool(&self) -> bool {
        match self {
            Value::Nil => false,
            Value::Bool(b) => *b,
            _ => true,
        }
    }

    pub fn to_number(&self) -> Option<Number> {
        match self {
            Value::Number(n) => Some(*n),
            Value::String(s) => {
                s.to_number()
            }
            _ => None,
        }
    }

    pub fn to_string(&self) -> Option<String<'s, 'g>> {
        match self {
            Value::Number(n) => Some(String::from_number(*n)),
            Value::String(s) => Some(s.clone()),
            _ => None,
        }
    }

    pub fn as_table(&self) -> Option<&TableRef<'s, 'g>> {
        match self {
            Value::Table(t) => Some(t),
            _ => None,
        }
    }

    pub fn as_function(&self) -> Option<&FunctionRef<'g>> {
        match self {
            Value::Function(f) => Some(f),
            _ => None,
        }
    }
}
