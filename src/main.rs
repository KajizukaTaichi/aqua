use clap::Parser;
use rustyline::DefaultEditor;
use std::{
    collections::{HashMap, HashSet},
    fs::read_to_string,
    process::exit,
};

const VERSION: &str = "0.1.0";

#[derive(Parser, Debug)]
#[command(
    name = "Aqua",
    version = VERSION,
    author = "梶塚太智 <kajizukataichi@outlook.jp>",
    about = "A class based object oriented programming language",
)]
struct Cli {
    /// Run the script file
    #[arg(index = 1)]
    file: Option<String>,
}

fn main() {
    let mut scope = builtin_classes();
    let cli = Cli::parse();

    if let Some(path) = cli.file {
        let code = read_to_string(path).unwrap();
        run_program(code.to_string(), &mut scope);
    } else {
        println!("Aqua {VERSION}");
        let mut rl = DefaultEditor::new().unwrap();

        // REPL
        loop {
            let mut code = String::new();
            loop {
                let enter = rl.readline("> ").unwrap_or_default().trim().to_string();
                if enter.is_empty() {
                    break;
                }
                code += &format!("{enter} ");
                rl.add_history_entry(&enter).unwrap_or_default();
            }

            if !code.is_empty() {
                if code.trim() == ":q" {
                    exit(0);
                } else {
                    if let Type::Object(mut obj) = run_program(code, &mut scope) {
                        println!("{}", obj.display(&mut scope));
                    }
                }
            }
        }
    }
}

fn builtin_classes() -> Scope {
    let classes = HashMap::from([
        (
            "console".to_string(),
            (Class {
                properties: HashSet::new(),
                methods: HashMap::from([
                    (
                        "writeln".to_string(),
                        Function::BuiltIn(|args, scope| {
                            let text = args[0].get_object().display(scope);
                            println!("{text}");

                            scope.get("console").unwrap().get_object()
                        }),
                    ),
                    (
                        "write".to_string(),
                        Function::BuiltIn(|args, scope| {
                            let text = args[0].get_object().display(scope);
                            print!("{text}");

                            scope.get("console").unwrap().get_object()
                        }),
                    ),
                    (
                        "read".to_string(),
                        Function::BuiltIn(|args, scope| {
                            let text = DefaultEditor::new()
                                .unwrap()
                                .readline(&args[0].get_object().display(scope))
                                .unwrap_or_default();

                            let class = scope.get("string").unwrap();
                            Object {
                                class: class.to_owned().get_class(),
                                properties: HashMap::from([(
                                    "raw".to_string(),
                                    Type::Data(text.to_string().as_bytes().to_vec()),
                                )]),
                            }
                        }),
                    ),
                    (
                        "__display__".to_string(),
                        Function::BuiltIn(|_, scope| {
                            let class = scope.get("string").unwrap();
                            Object {
                                class: class.to_owned().get_class(),
                                properties: HashMap::from([(
                                    "raw".to_string(),
                                    Type::Data("console".to_string().as_bytes().to_vec()),
                                )]),
                            }
                        }),
                    ),
                ]),
            }),
        ),
        (
            "system".to_string(),
            (Class {
                properties: HashSet::new(),
                methods: HashMap::from([
                    ("exit".to_string(), Function::BuiltIn(|_, _| exit(0))),
                    (
                        "__display__".to_string(),
                        Function::BuiltIn(|_, scope| {
                            let class = scope.get("string").unwrap();
                            Object {
                                class: class.to_owned().get_class(),
                                properties: HashMap::from([(
                                    "raw".to_string(),
                                    Type::Data("system".to_string().as_bytes().to_vec()),
                                )]),
                            }
                        }),
                    ),
                ]),
            }),
        ),
        (
            "null".to_string(),
            (Class {
                properties: HashSet::new(),
                methods: HashMap::from([(
                    "__display__".to_string(),
                    Function::BuiltIn(|_, scope| {
                        let class = scope.get("string").unwrap();
                        Object {
                            class: class.to_owned().get_class(),
                            properties: HashMap::from([(
                                "raw".to_string(),
                                Type::Data("null".as_bytes().to_vec()),
                            )]),
                        }
                    }),
                )]),
            }),
        ),
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
                    (
                        "to-string".to_string(),
                        Function::BuiltIn(|_, scope| {
                            let mut value = scope.get("self").unwrap().get_object();
                            value.call_method("__display__".to_string(), vec![], scope)
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
                        "to-number".to_string(),
                        Function::BuiltIn(|_, scope| {
                            let left = scope
                                .get("self")
                                .unwrap()
                                .get_object()
                                .properties
                                .get("raw")
                                .unwrap()
                                .get_data();
                            let class = scope.get("number").unwrap();
                            Object {
                                class: class.to_owned().get_class(),
                                properties: HashMap::from([(
                                    "raw".to_string(),
                                    Type::Data(
                                        String::from_utf8(left.try_into().unwrap())
                                            .unwrap()
                                            .parse::<f64>()
                                            .unwrap_or_default()
                                            .to_le_bytes()
                                            .to_vec(),
                                    ),
                                )]),
                            }
                        }),
                    ),
                    (
                        "__display__".to_string(),
                        Function::BuiltIn(|_, scope| scope.get("self").unwrap().get_object()),
                    ),
                ]),
            }),
        ),
    ]);

    let mut new = HashMap::new();
    for (k, v) in classes.clone() {
        new.insert(k, Type::Class(v));
    }

    for i in ["console", "system"] {
        let class = classes.get(i).unwrap();
        new.insert(
            i.to_string(),
            Type::Object(Object {
                class: class.to_owned(),
                properties: HashMap::new(),
            }),
        );
    }
    new
}

fn run_program(source: String, scope: &mut Scope) -> Type {
    let source = tokenize_program(source);
    let mut result = parse_object("null".to_string(), scope);

    // Execute each line
    for lines in source {
        if lines.len() == 2 {
            let define: Vec<&str> = lines[0].split_whitespace().collect();

            if define.len() == 2 {
                // Setting property of the object
                let mut object = scope.get(define[0]).unwrap().get_object();
                object.properties.insert(
                    define[1].to_string(),
                    parse_expr(lines[1].clone(), scope).eval(scope),
                );
                result = Type::Object(object);
                scope.insert(define[0].to_string(), result.clone());
            } else if define.len() == 1 {
                // Define variable
                result = parse_expr(lines[1].clone(), scope).eval(scope);
                scope.insert(lines[0].trim().to_string(), result.clone());
            }
        } else {
            // Evaluate the expression
            result = parse_expr(lines[0].to_string(), scope).eval(scope);
        }
    }
    result
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

fn parse_object(source: String, classes: &mut Scope) -> Type {
    let mut source = source.trim().to_string();
    if let Ok(n) = source.parse::<f64>() {
        let class = classes.get("number").unwrap();
        Type::Object(Object {
            class: class.to_owned().get_class(),
            properties: HashMap::from([("raw".to_string(), Type::Data(n.to_le_bytes().to_vec()))]),
        })
    } else if source == "null" {
        let class = classes.get("null").unwrap();
        Type::Object(Object {
            class: class.to_owned().get_class(),
            properties: HashMap::new(),
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
    } else if source.contains("class{") && source.ends_with("}") {
        source.remove(source.rfind("}").unwrap());
        source = source.replacen("class{", "", 1);

        let tokens = tokenize_program(source);
        let mut properties = HashSet::new();
        let mut methods = HashMap::new();

        for token in tokens {
            if token.len() == 2 {
                methods.insert(token[0].trim().to_string().clone(), {
                    let tokens = tokenize_expr(token[1].clone());
                    Function::UserDefined(
                        {
                            let mut temp = tokens[0].clone();
                            temp = temp.replacen("method(", "", 1);
                            temp.remove(temp.rfind(")").unwrap());
                            tokenize_expr(temp)
                        },
                        tokens[1].clone(),
                    )
                });
            } else {
                properties.insert(token[0].trim().to_string().clone());
            }
        }

        Type::Class(Class {
            properties,
            methods,
        })
    } else if source.contains("{") && source.ends_with("}") {
        source.remove(source.rfind("}").unwrap());
        let (class, body) = source.split_once("{").unwrap();
        let class = classes.get(class.trim()).unwrap();
        Type::Object(Object {
            class: class.to_owned().get_class(),
            properties: {
                let mut result = HashMap::new();
                let tokens = tokenize_program(body.to_string());
                for token in tokens {
                    if token.len() == 2 {
                        result.insert(
                            token[0].trim().to_string().clone(),
                            parse_expr(token[1].trim().to_string().clone(), classes).eval(classes),
                        );
                    }
                }
                result
            },
        })
    } else {
        Type::Variable(source)
    }
}

fn parse_expr(source: String, classes: &mut Scope) -> Type {
    let tokens = tokenize_expr(source);
    if tokens.len() == 1 {
        parse_object(tokens[0].clone(), classes)
    } else {
        Type::Expr(Expr {
            object: Box::new(parse_object(tokens[0].clone(), classes)),
            methods: tokens[1].clone(),
            args: tokens
                .get(2..)
                .unwrap_or_default()
                .iter()
                .map(|s| parse_object(s.clone(), classes))
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
            '(' | '{' if !in_quote => {
                in_parentheses += 1;
                current_token.push(c);
            }
            ')' | '}' if !in_quote => {
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
    fn call_method(&mut self, name: String, args: Args, scope: &mut Scope) -> Object {
        scope.insert("self".to_string(), Type::Object(self.clone()));
        scope.extend(self.properties.clone());

        let method = self.class.methods.get(&name).unwrap();
        let result = method.call(args, scope);

        *self = scope.get("self").unwrap().get_object();
        for i in &self.class.properties {
            self.properties
                .insert(i.to_string(), scope.get(i).unwrap().to_owned());
        }

        // dbg!(&scope);
        result
    }

    fn display(&mut self, scope: &mut Scope) -> String {
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

    fn get_class(&self) -> Class {
        match self {
            Type::Class(class) => class.to_owned(),
            _ => panic!("らんらんるー"),
        }
    }

    fn eval(&self, scope: &mut Scope) -> Type {
        match self {
            Type::Expr(expr) => Type::Object(expr.eval(scope)),
            Type::Variable(variable) => scope.get(variable).unwrap().to_owned(),
            other => other.clone(),
        }
    }
}

#[derive(Clone, Debug)]
enum Function {
    BuiltIn(fn(Args, &mut Scope) -> Object),
    UserDefined(Vec<String>, String),
}
impl Function {
    fn call(&self, args: Args, properties: &mut Scope) -> Object {
        if let Function::BuiltIn(func) = self {
            func(args, properties)
        } else if let Function::UserDefined(params, code) = self {
            for (param, arg) in params.iter().zip(args) {
                properties.insert(param.to_string(), arg);
            }

            let mut code = code.replacen("{", "", 1);
            code.remove(code.rfind("}").unwrap());
            run_program(code.to_string(), properties).get_object()
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
    fn eval(&self, scope: &mut Scope) -> Object {
        let args = {
            let mut new = vec![];
            for i in self.args.clone() {
                if let Type::Expr(expr) = i {
                    new.push(Type::Object(expr.eval(scope)));
                } else if let Type::Variable(v) = i {
                    new.push(scope.get(&v).unwrap().to_owned());
                } else {
                    new.push(i);
                }
            }
            new
        };

        (*self.object)
            .eval(scope)
            .get_object()
            .call_method(self.methods.clone(), args, scope)
    }
}
