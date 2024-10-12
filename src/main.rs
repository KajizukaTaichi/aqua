use std::collections::{HashMap, HashSet};

fn main() {
    println!("Aqua 0.1");
    let classes = HashMap::from([
        (
            "number".to_string(),
            (Class {
                properties: HashSet::from(["raw".to_string()]),
                methods: HashMap::from([
                    (
                        "+".to_string(),
                        Function::BuiltIn(|args, scope| {
                            let left = scope
                                .get("self")
                                .unwrap()
                                .get_object()
                                .properties
                                .get("raw")
                                .unwrap()
                                .get_data();
                            let right = args[0]
                                .get_object()
                                .properties
                                .get("raw")
                                .unwrap()
                                .get_data();
                            let mut obj = scope.get("self").unwrap().get_object();
                            obj.properties.insert(
                                "raw".to_string(),
                                Type::Data(
                                    (f64::from_le_bytes(left.try_into().unwrap())
                                        + f64::from_le_bytes(right.try_into().unwrap()))
                                    .to_le_bytes()
                                    .to_vec(),
                                ),
                            );
                            obj
                        }),
                    ),
                    (
                        "-".to_string(),
                        Function::BuiltIn(|args, scope| {
                            let left = scope
                                .get("self")
                                .unwrap()
                                .get_object()
                                .properties
                                .get("raw")
                                .unwrap()
                                .get_data();
                            let right = args[0]
                                .get_object()
                                .properties
                                .get("raw")
                                .unwrap()
                                .get_data();
                            let mut obj = scope.get("self").unwrap().get_object();
                            obj.properties.insert(
                                "raw".to_string(),
                                Type::Data(
                                    (f64::from_le_bytes(left.try_into().unwrap())
                                        - f64::from_le_bytes(right.try_into().unwrap()))
                                    .to_le_bytes()
                                    .to_vec(),
                                ),
                            );
                            obj
                        }),
                    ),
                    (
                        "*".to_string(),
                        Function::BuiltIn(|args, scope| {
                            let left = scope
                                .get("self")
                                .unwrap()
                                .get_object()
                                .properties
                                .get("raw")
                                .unwrap()
                                .get_data();
                            let right = args[0]
                                .get_object()
                                .properties
                                .get("raw")
                                .unwrap()
                                .get_data();
                            let mut obj = scope.get("self").unwrap().get_object();
                            obj.properties.insert(
                                "raw".to_string(),
                                Type::Data(
                                    (f64::from_le_bytes(left.try_into().unwrap())
                                        * f64::from_le_bytes(right.try_into().unwrap()))
                                    .to_le_bytes()
                                    .to_vec(),
                                ),
                            );
                            obj
                        }),
                    ),
                    (
                        "/".to_string(),
                        Function::BuiltIn(|args, scope| {
                            let left = scope
                                .get("self")
                                .unwrap()
                                .get_object()
                                .properties
                                .get("raw")
                                .unwrap()
                                .get_data();
                            let right = args[0]
                                .get_object()
                                .properties
                                .get("raw")
                                .unwrap()
                                .get_data();
                            let mut obj = scope.get("self").unwrap().get_object();
                            obj.properties.insert(
                                "raw".to_string(),
                                Type::Data(
                                    (f64::from_le_bytes(left.try_into().unwrap())
                                        / f64::from_le_bytes(right.try_into().unwrap()))
                                    .to_le_bytes()
                                    .to_vec(),
                                ),
                            );
                            obj
                        }),
                    ),
                    (
                        "__display__".to_string(),
                        Function::BuiltIn(|_, scope| {
                            let left = scope
                                .get("self")
                                .unwrap()
                                .get_object()
                                .properties
                                .get("raw")
                                .unwrap()
                                .get_data();
                            let class = scope.get("string").unwrap();
                            Object {
                                class: class.to_owned().get_class(),
                                properties: HashMap::from([(
                                    "raw".to_string(),
                                    Type::Data(
                                        f64::from_le_bytes(left.try_into().unwrap())
                                            .to_string()
                                            .as_bytes()
                                            .to_vec(),
                                    ),
                                )]),
                            }
                        }),
                    ),
                ]),
            }),
        ),
        (
            "string".to_string(),
            (Class {
                properties: HashSet::from(["raw".to_string()]),
                methods: HashMap::from([
                    (
                        "+".to_string(),
                        Function::BuiltIn(|args, scope| {
                            let left = scope
                                .get("self")
                                .unwrap()
                                .get_object()
                                .properties
                                .get("raw")
                                .unwrap()
                                .get_data();
                            let right = args[0]
                                .get_object()
                                .properties
                                .get("raw")
                                .unwrap()
                                .get_data();
                            let mut obj = scope.get("self").unwrap().get_object();
                            obj.properties.insert(
                                "raw".to_string(),
                                Type::Data(
                                    (String::from_utf8(left).unwrap()
                                        + &String::from_utf8(right).unwrap())
                                        .as_bytes()
                                        .to_vec(),
                                ),
                            );
                            obj
                        }),
                    ),
                    (
                        "__display__".to_string(),
                        Function::BuiltIn(|_, scope| scope.get("string").unwrap().get_object()),
                    ),
                ]),
            }),
        ),
    ]);
    let scope = {
        let mut new = HashMap::new();
        for (k, v) in classes {
            new.insert(k, Type::Class(v));
        }
        new
    };

    let code = "x = 1; y = x + 2; 1 + y";
    dbg!(run_program(code.to_string(), &mut scope.clone())
        .get_object()
        .display(scope));
}

fn run_program(source: String, scope: &mut Scope) -> Type {
    let source = tokenize_program(source);
    let mut result = None;

    // Execute each line
    for lines in source {
        if lines.len() == 2 {
            // Define variable
            result = Some(parse_expr(lines[1].clone(), scope.clone()).eval(scope.clone()));
            scope.insert(lines[0].trim().to_string(), result.clone().unwrap());
        } else {
            // Evaluate the expression
            result = Some(parse_expr(lines[0].to_string(), scope.clone()).eval(scope.clone()));
        }
    }
    result.unwrap()
}

fn tokenize_program(input: String) -> Vec<Vec<String>> {
    let mut tokens: Vec<Vec<String>> = Vec::new();
    let mut current_token = String::new();
    let mut after_equal = String::new();
    let mut is_equal = false;
    let mut in_parentheses: usize = 0;
    let mut in_quote = false;

    for c in input.chars() {
        match c {
            '{' if !in_quote => {
                if is_equal {
                    after_equal.push(c);
                } else {
                    current_token.push(c);
                }
                in_parentheses += 1;
            }
            '}' if !in_quote => {
                if is_equal {
                    after_equal.push(c);
                } else {
                    current_token.push(c);
                }
                in_parentheses -= 1;
            }
            ';' if !in_quote => {
                if in_parentheses != 0 {
                    if is_equal {
                        after_equal.push(c);
                    } else {
                        current_token.push(c);
                    }
                } else if !current_token.is_empty() {
                    if is_equal {
                        is_equal = false;
                        tokens.push(vec![current_token.clone(), after_equal.clone()]);
                        current_token.clear();
                        after_equal.clear();
                    } else {
                        tokens.push(vec![current_token.clone()]);
                        current_token.clear();
                    }
                }
            }
            '=' if !in_quote => {
                if in_parentheses != 0 {
                    if is_equal {
                        after_equal.push(c);
                    } else {
                        current_token.push(c);
                    }
                } else {
                    is_equal = true;
                }
            }
            '"' => {
                in_quote = !in_quote;
                if is_equal {
                    after_equal.push(c);
                } else {
                    current_token.push(c);
                }
            }
            _ => {
                if is_equal {
                    after_equal.push(c);
                } else {
                    current_token.push(c);
                }
            }
        }
    }

    if in_parentheses == 0 && !current_token.is_empty() {
        if is_equal {
            tokens.push(vec![current_token.clone(), after_equal]);
            current_token.clear();
        } else {
            tokens.push(vec![current_token.clone()]);
            current_token.clear();
        }
    }
    tokens
}

fn parse_object(source: String, classes: Scope) -> Type {
    let mut source = source.trim().to_string();
    if let Ok(n) = source.parse::<f64>() {
        let class = classes.get("number").unwrap();
        Type::Object(Object {
            class: class.to_owned().get_class(),
            properties: HashMap::from([("raw".to_string(), Type::Data(n.to_le_bytes().to_vec()))]),
        })
    } else if source.starts_with("(") && source.ends_with(")") {
        source.remove(source.find("(").unwrap());
        source.remove(source.rfind(")").unwrap());
        parse_expr(source, classes)
    } else if source.starts_with("\"") && source.ends_with("\"") {
        source.remove(source.find("\"").unwrap());
        source.remove(source.rfind("\"").unwrap());
        let class = classes.get("string").unwrap();
        Type::Object(Object {
            class: class.to_owned().get_class(),
            properties: HashMap::from([(
                "raw".to_string(),
                Type::Data(source.as_bytes().to_vec()),
            )]),
        })
    } else {
        Type::Variable(source)
    }
}

fn parse_expr(source: String, classes: Scope) -> Type {
    let tokens = tokenize_expr(source);
    if tokens.len() == 1 {
        parse_object(tokens[0].clone(), classes)
    } else {
        Type::Expr(Expr {
            object: Box::new(parse_object(tokens[0].clone(), classes.clone())),
            methods: tokens[1].clone(),
            args: tokens
                .get(2..)
                .unwrap_or_default()
                .iter()
                .map(|s| parse_object(s.clone(), classes.clone()))
                .collect(),
        })
    }
}

fn tokenize_expr(input: String) -> Vec<String> {
    let mut tokens = Vec::new();
    let mut current_token = String::new();
    let mut in_parentheses: usize = 0;
    let mut in_quote = false;

    for c in input.chars() {
        match c {
            '(' if !in_quote => {
                in_parentheses += 1;
                current_token.push(c);
            }
            ')' if !in_quote => {
                if in_parentheses != 0 {
                    current_token.push(c);
                    in_parentheses -= 1;
                    if in_parentheses == 0 {
                        tokens.push(current_token.clone());
                        current_token.clear();
                    }
                }
            }
            '"' => {
                if in_parentheses == 0 {
                    if in_quote {
                        current_token.push(c);
                        in_quote = false;
                        tokens.push(current_token.clone());
                        current_token.clear();
                    } else {
                        in_quote = true;
                        current_token.push(c);
                    }
                } else {
                    current_token.push(c);
                }
            }
            ' ' | '\n' | '\t' | '\r' | '　' => {
                if in_parentheses != 0 || in_quote {
                    current_token.push(c);
                } else if !current_token.is_empty() {
                    tokens.push(current_token.clone());
                    current_token.clear();
                }
            }
            _ => {
                current_token.push(c);
            }
        }
    }

    if !(in_parentheses != 0 || in_quote || current_token.is_empty()) {
        tokens.push(current_token);
    }
    tokens
}

type Args = Vec<Type>;
type Scope = HashMap<String, Type>;

#[derive(Clone, Debug)]
struct Class {
    properties: HashSet<String>,
    methods: HashMap<String, Function>,
}

#[derive(Clone, Debug)]
struct Object {
    class: Class,
    properties: Scope,
}
impl Object {
    fn call_method(&mut self, name: String, args: Args, mut scope: Scope) -> Object {
        scope.insert("self".to_string(), Type::Object(self.clone()));
        scope.extend(self.properties.clone());

        let method = self.class.methods.get(&name).unwrap();
        let result = method.call(args, scope.clone());

        *self = scope.get("self").unwrap().get_object();
        for i in &self.class.properties {
            self.properties
                .insert(i.to_string(), scope.get(i).unwrap().to_owned());
        }

        result
    }

    fn display(&mut self, scope: Scope) -> String {
        String::from_utf8(
            self.call_method("__display__".to_string(), vec![], scope)
                .properties
                .get("raw")
                .unwrap()
                .get_data(),
        )
        .unwrap()
    }
}

#[derive(Clone, Debug)]
enum Type {
    Class(Class),
    Object(Object),
    Variable(String),
    Data(Vec<u8>),
    Expr(Expr),
}
impl Type {
    fn get_object(&self) -> Object {
        match self {
            Type::Object(obj) => obj.to_owned(),
            _ => panic!("らんらんるー"),
        }
    }

    fn get_data(&self) -> Vec<u8> {
        match self {
            Type::Data(data) => data.to_owned(),
            _ => panic!("らんらんるー"),
        }
    }

    fn get_expr(&self) -> Expr {
        match self {
            Type::Expr(expr) => expr.to_owned(),
            _ => panic!("らんらんるー"),
        }
    }

    fn get_class(&self) -> Class {
        match self {
            Type::Class(class) => class.to_owned(),
            _ => panic!("らんらんるー"),
        }
    }

    fn eval(&self, scope: Scope) -> Type {
        match self {
            Type::Expr(expr) => Type::Object(expr.eval(scope)),
            Type::Variable(variable) => scope.get(variable).unwrap().to_owned(),
            other => other.clone(),
        }
    }
}

#[derive(Clone, Debug)]
enum Function {
    BuiltIn(fn(Args, Scope) -> Object),
    UserDefined(String),
}
impl Function {
    fn call(&self, args: Args, properties: Scope) -> Object {
        let args = {
            let mut new = vec![];
            for i in args {
                if let Type::Variable(v) = i {
                    new.push(properties.get(&v).unwrap().to_owned());
                } else {
                    new.push(i);
                }
            }
            new
        };

        if let Function::BuiltIn(func) = self {
            func(args, properties)
        } else if let Function::UserDefined(code) = self {
            run_program(code.to_string(), &mut properties.clone()).get_object()
        } else {
            todo!()
        }
    }
}

#[derive(Clone, Debug)]
struct Expr {
    object: Box<Type>,
    methods: String,
    args: Args,
}
impl Expr {
    fn eval(&self, scope: Scope) -> Object {
        let args = {
            let mut new = vec![];
            for i in self.args.clone() {
                if let Type::Expr(expr) = i {
                    new.push(Type::Object(expr.eval(scope.clone())));
                } else if let Type::Variable(v) = i {
                    new.push(scope.get(&v).unwrap().to_owned());
                } else {
                    new.push(i);
                }
            }
            new
        };

        (*self.object).eval(scope.clone()).get_object().call_method(
            self.methods.clone(),
            args,
            scope,
        )
    }
}
