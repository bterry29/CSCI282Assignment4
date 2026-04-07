// Assignment 4: Diamondback Compiler - Functions and Calling Conventions
// TODO: Complete this compiler implementation
//
// Extend compiler to support Diamondback, adding function definitions and calls
// Introduces stack frames, calling conventions, and proper function compilation


use im::HashMap;
use sexp::Atom::*;
use sexp::*;
use std::env;
use std::fs::File;
use std::io::prelude::*;

// ============= Abstract Syntax Tree =============

/// Unary operators
#[derive(Debug)]
enum Op1 {
    Add1,
    Sub1,
    Negate,
    IsNum,
    IsBool,
    Print,
}

/// Binary operators
#[derive(Debug)]
enum Op2 {
    Plus,
    Minus,
    Times,
    Less,
    Greater,
    LessEq,
    GreaterEq,
    Equal,
}

/// The Boa expression AST
///
/// Grammar:
///   <expr> := <number>
///           | <identifier>
///           | (let (<binding>+) <expr>)
///           | (add1 <expr>) | (sub1 <expr>)
///           | (+ <expr> <expr>) | (- <expr> <expr>) | (* <expr> <expr>)
///   <binding> := (<identifier> <expr>)

// Add Program struct
struct Program {
    defns: Vec<Definition>, 
    main: Expr,
}

// Add Definition struct
struct Definition {
    name: String,
    params: Vec<String>,
    body: Expr,
}


#[derive(Debug)]
enum Expr {
    Number(i32),
    Bool(bool),
    Input,
    Id(String),
    Let(Vec<(String, Expr)>, Box<Expr>),
    UnOp(Op1, Box<Expr>),
    BinOp(Op2, Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Block(Vec<Expr>),
    Loop(Box<Expr>),
    Break(Box<Expr>),
    Set(String, Box<Expr>),
    Call(String, Vec<Expr>),
}

// ============= Assembly Representation =============

/// Values that can appear in assembly instructions
#[derive(Debug)]
enum Val {
    Reg(Reg),
    Imm(i32),
    RegOffset(Reg, i32), // e.g., [rsp - 8]
}

/// Registers we use
#[derive(Debug)]
enum Reg {
    RAX,
    RSP,
    RBX,
    RDI,
    RBP,
}

/// Assembly instructions we generate
#[derive(Debug)]
enum Instr {
    IMov(Val, Val),
    IAdd(Val, Val),
    ISub(Val, Val),
    IMul(Val, Val),
    IAnd(Val, Val),
    Cmp(Val,Val),
    Jmp(String),
    Neg(Val),
    Je(String),
    Jne(String),
    Label(String),
    Jl(String),
    Jg(String),
    Jle(String),
    Jge(String),
    Sar(Val, Val),
    Jo(String),
    Push(Val),
    Pop(Val),
    Call(String),
    Ret,


}

// ============= Parsing =============

/// Parse an S-expression into our Expr AST
///
/// Examples of valid Boa expressions:
///   42                          -> Number(42)
///   x                           -> Id("x")
///   (add1 5)                    -> UnOp(Add1, Number(5))
///   (+ 1 2)                     -> BinOp(Plus, Number(1), Number(2))
///   (let ((x 5)) x)             -> Let([("x", Number(5))], Id("x"))
///   (let ((x 5) (y 6)) (+ x y)) -> Let([("x", Number(5)), ("y", Number(6))], BinOp(...))
///
/// Error handling:
///   - Invalid syntax: panic!("Invalid")
///   - Number out of i32 range: panic!("Invalid")

//Implement parse_program helper
fn parse_program(s: &Sexp) -> Program {
    match s {
        Sexp::List(items) => {
            let mut defns = Vec::new();
            let mut non_defns = Vec::new();

            for item in items {
                if let Some(defn) = try_parse_defn(item) {
                    defns.push(defn);
                } else {
                    non_defns.push(item);
                }
            }

            if defns.is_empty() {
                Program {
                    defns,
                    main: parse_expr(s),
                }
            } else if non_defns.len() == 1 {
                Program {
                    defns,
                    main: parse_expr(non_defns[0]),
                }
            } else {
                panic!("Invalid program");
            }
        }
        _ => Program {
            defns: Vec::new(),
            main: parse_expr(s),
        },
    }
}

//Add try_parse_defn helper
fn try_parse_defn(s: &Sexp) -> Option<Definition> {
    match s{
        Sexp::List(vec) => match&vec[..] {
            [Sexp::Atom(S(fun)), Sexp::List(signature), body] if fun == "fun" => {
                match &signature[..]{
                    [Sexp::Atom(S(name)), params @ ..] => {
                        let param_names: Vec<String> = params.iter().map(|p| {
                            match p{
                                Sexp::Atom(S(name)) => name.clone(),
                                _=> panic!("Invalid parameter"),
                            }
                        }).collect();

                        Some(Definition {
                            name: name.clone(),
                            params: param_names,
                            body: parse_expr(body),
                        })
                    }
                    _=> None,
                }
            }
            _=> None,
        },
        _=> None,
    }
    }


fn parse_expr(s: &Sexp) -> Expr {
    match s {
        // TODO: Handle number atoms
        // Hint: Sexp::Atom(I(n)) => ...
        //       Use i32::try_from(*n).unwrap_or_else(|_| panic!("Invalid"))
        Sexp::Atom(I(n)) => {
            let n_i32 = i32::try_from(*n).unwrap_or_else(|_| panic!("Invalid"));
            Expr::Number(n_i32)
        }
        // TODO: Handle identifier atoms
        // Hint: Sexp::Atom(S(name)) => ...
        //       Make sure to check it's not a reserved keyword
        Sexp::Atom(S(name)) => {
            if name == "true" {
                Expr::Bool(true)
            } else if name == "false" {
                Expr::Bool(false)
            } else if name == "input" {
                Expr::Input
            }
            else if name == "add1" || name == "sub1" || name == "+" || name == "-" || name == "*" || name == "let" || name == "if" || name == "loop" || name == "break" || name == "set!" || name == "block" || name  == "negate"|| name == "isnum" || name == "isbool" 
            || name == "<" || name == ">" || name == "<=" || name == ">=" || name == "=" || name == "fun" || name == "print" {
                panic!("Invalid");
            }else{
            Expr::Id(name.clone())
            }
        }
        // TODO: Handle list expressions
        // Add cases for negate, isnum, isbool, and comparison operators
        Sexp::List(vect) => match &vect[..] {
             [Sexp::Atom(S(op)), e] if op == "add1" => Expr::UnOp(Op1::Add1, Box::new(parse_expr(e))),
             [Sexp::Atom(S(op)), e] if op == "sub1" => Expr::UnOp(Op1::Sub1, Box::new(parse_expr(e))),
             [Sexp::Atom(S(op)), e] if op == "negate" => Expr::UnOp(Op1::Negate, Box::new(parse_expr(e))),
             [Sexp::Atom(S(op)), e] if op == "isnum" => Expr::UnOp(Op1::IsNum, Box::new(parse_expr(e))),
             [Sexp::Atom(S(op)), e] if op == "isbool" => Expr::UnOp(Op1::IsBool, Box::new(parse_expr(e))),
             [Sexp::Atom(S(op)), e] if op == "print" => Expr:: UnOp(Op1::Print, Box::new(parse_expr(e))),
             [Sexp::Atom(S(op)), e1, e2] if op == "+" => Expr::BinOp(Op2::Plus, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
             [Sexp::Atom(S(op)), e1, e2] if op == "-" => Expr::BinOp(Op2::Minus, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
             [Sexp::Atom(S(op)), e1, e2] if op == "*" => Expr::BinOp(Op2::Times, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
             [Sexp::Atom(S(op)), Sexp::List(bindings), body] if op == "let" => {
                 let mut binds = Vec::new();
                 for b in bindings {
                    binds.push(parse_bind(b));
                 }
                 Expr::Let(binds, Box::new(parse_expr(body)))
             },
             // Add cases for comparison operators
             [Sexp::Atom(S(op)), e1, e2] if op == "<" => Expr::BinOp(Op2::Less, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
             [Sexp::Atom(S(op)), e1, e2] if op == ">" => Expr::BinOp(Op2::Greater, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
             [Sexp::Atom(S(op)), e1, e2] if op == "<=" => Expr::BinOp(Op2::LessEq, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
             [Sexp::Atom(S(op)), e1, e2] if op == ">=" => Expr::BinOp(Op2::GreaterEq, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
             [Sexp::Atom(S(op)), e1, e2] if op == "=" => Expr::BinOp(Op2::Equal, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
             // Add cases for if, loop, break, set!, and block
             [Sexp::Atom(S(op)), e1, e2, e3] if op == "if" => Expr::If(Box::new(parse_expr(e1)), Box::new(parse_expr(e2)), Box::new(parse_expr(e3))),
             [Sexp::Atom(S(op)), e, rest @ ..] if op == "block" => {
                let mut exprs = vec![parse_expr(e)];
                for i in rest
                {
                    exprs.push(parse_expr(i));
                }
                Expr::Block(exprs)
             },
             [Sexp::Atom(S(op)),e] if op == "loop" => Expr::Loop(Box::new(parse_expr(e))),
             [Sexp::Atom(S(op)),e] if op == "break" => Expr::Break(Box::new(parse_expr(e))),
             [Sexp::Atom(S(op)), Sexp::Atom(S(name)), e] if op == "set!" => Expr::Set(name.clone(), Box::new(parse_expr(e))),
             [Sexp::Atom(S(name)), args @ ..] => {
                let parsed_args = args.iter().map(parse_expr).collect();
                Expr::Call(name.clone(), parsed_args)
            },
             _ => panic!("Invalid"),
        },

        _ => panic!("Invalid"),
    }
}

/// Parse a single binding from a let expression
///
/// A binding looks like: (x 5) or (my-var (+ 1 2))
/// Returns a tuple of (variable_name, expression)
///
/// Error handling:
///   - Invalid binding syntax: panic!("Invalid")
fn parse_bind(s: &Sexp) -> (String, Expr) {
    // TODO: Parse a binding of the form (identifier expr)
    // Hint: match s {
    //     Sexp::List(vec) => match &vec[..] {
    //         [Sexp::Atom(S(name)), e] => (name.clone(), parse_expr(e)),
    //         _ => panic!("Invalid"),
    //     }
    //     _ => panic!("Invalid"),
    // }
    match s{
        Sexp::List(vec) => match &vec[..]{
            [Sexp::Atom(S(name)), e] => (name.clone(), parse_expr(e)),
            _=> panic!("Invalid"),
        }
        _ => panic!("Invalid"),
    }
}

// ============= Compilation =============

/// Compile an expression to a list of assembly instructions
///
/// Parameters:
///   - e: the expression to compile
///   - si: stack index - the next available stack slot (starts at 2)
///         Stack slots are at [rsp - 8*si], e.g., si=2 means [rsp - 16]
///   - env: environment mapping variable names to stack offsets
///
/// The compiled code should leave its result in RAX.
///
/// Stack layout:
///   [rsp - 8]  : reserved (return address area)
///   [rsp - 16] : first variable (si=2)
///   [rsp - 24] : second variable (si=3)
///   ...
///
/// Examples:
///   Number(5) -> [IMov(Reg(RAX), Imm(5))]
///
///   UnOp(Add1, Number(5)) ->
///     [IMov(Reg(RAX), Imm(5)), IAdd(Reg(RAX), Imm(1))]
///
///   BinOp(Plus, Number(1), Number(2)) ->
///     1. Compile left operand (result in RAX)
///     2. Save RAX to stack at [rsp - 8*si]
///     3. Compile right operand (result in RAX)
///     4. Add stack value to RAX
///
///   Let([(x, 5)], Id(x)) ->
///     1. Compile binding expression (5)
///     2. Store result at stack slot
///     3. Add x -> stack_offset to environment
///     4. Compile body with updated environment

// Add value tagging and untagging
const NUM_TAG: i64 = 0;
const NUM_TAG_MASK: i64 = 1;
const BOOL_TAG: i64 = 1;
const BOOL_TAG_MASK: i64 = 1;
const TRUE_VAL: i64 = 3;
const FALSE_VAL: i64 = 1;

fn encode_num(n: i32) -> i64 {
    (n as i64) << 1 
}

fn decode_num(tagged: i64) -> i32 {
    (tagged >> 1) as i32
}

// Add control flow labels

fn new_label(label_counter: &mut i32, name: &str) -> String {
    *label_counter += 1;
    format!("{}_{}", name, label_counter)
}

fn compile_to_instrs(e: &Expr, si: i32, env: &HashMap<String, i32>, label_counter: &mut i32, break_target: Option<String>) -> Vec<Instr> {
    match e {
        // TODO: Number - move immediate value to RAX
        Expr::Number(n) => vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(encode_num(*n) as i32))],
        // TODO: Id - look up variable in environment, load from stack
        Expr:: Id(name) => {
            let offset = env.get(name).unwrap_or_else(|| panic!("Unbound variable identifier {}", name));
            vec![Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RBP, *offset))]
        }
        //TODO: Add cases for Bool
        Expr::Bool(b) => {
            if *b {
                vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(TRUE_VAL as i32))]
            }else{
                vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(FALSE_VAL as i32))]
            }
        }
        //TODO: Add case for Input
        Expr::Input => vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Reg(Reg::RDI))],
        // TODO: UnOp - compile subexpression, then apply operation
        Expr::UnOp(op, e) => {
            let mut instrs = compile_to_instrs(e, si, env, label_counter, break_target.clone());
            match op {
                Op1::Add1 => {
                    instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
                    instrs.push(Instr::IAnd(Val::Reg(Reg::RBX), Val::Imm(NUM_TAG_MASK as i32)));
                    instrs.push(Instr::Cmp(Val::Reg(Reg::RBX), Val::Imm(NUM_TAG as i32)));
                    instrs.push(Instr::Jne("throw_error".to_string()));
                    instrs.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(encode_num(1) as i32)));
                    instrs.push(Instr::Jo("throw_overflow".to_string()));
                },
                Op1::Sub1 => {
                    instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
                    instrs.push(Instr::IAnd(Val::Reg(Reg::RBX), Val::Imm(NUM_TAG_MASK as i32)));
                    instrs.push(Instr::Cmp(Val::Reg(Reg::RBX), Val::Imm(NUM_TAG as i32)));
                    instrs.push(Instr::Jne("throw_error".to_string()));
                    instrs.push(Instr::ISub(Val::Reg(Reg::RAX), Val::Imm(encode_num(1) as i32)));
                    instrs.push(Instr::Jo("throw_overflow".to_string()));
                },
                Op1::Negate => {
                instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
                instrs.push(Instr::IAnd(Val::Reg(Reg::RBX), Val::Imm(NUM_TAG_MASK as i32)));
                instrs.push(Instr::Cmp(Val::Reg(Reg::RBX), Val::Imm(NUM_TAG as i32)));
                instrs.push(Instr::Jne("throw_error".to_string()));
                instrs.push(Instr::Neg(Val::Reg(Reg::RAX)));
                instrs.push(Instr::Jo("throw_overflow".to_string()));
            },
                Op1::IsNum => {
                    let true_label = new_label(label_counter, "isnum_true");
                    let end_label = new_label(label_counter, "isnum_end");

                    instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));

                    instrs.push(Instr::IAnd(Val::Reg(Reg::RBX), Val::Imm(NUM_TAG_MASK as i32)));
                    instrs.push(Instr::Cmp(Val::Reg(Reg::RBX), Val::Imm(NUM_TAG as i32)));
                    instrs.push(Instr::Je(true_label.clone()));

                    instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
                    instrs.push(Instr::Jmp(end_label.clone()));
                    instrs.push(Instr::Label(true_label));

                    instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));
                    instrs.push(Instr::Label(end_label));

                }
                Op1::IsBool => {
                    let true_label = new_label(label_counter, "isbool_true");
                    let end_label = new_label(label_counter, "isbool_end");

                    instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));


                    instrs.push(Instr::IAnd(Val::Reg(Reg::RBX), Val::Imm(BOOL_TAG_MASK as i32)));
                    instrs.push(Instr::Cmp(Val::Reg(Reg::RBX), Val::Imm(BOOL_TAG as i32)));
                    instrs.push(Instr::Je(true_label.clone()));

                    instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
                    instrs.push(Instr::Jmp(end_label.clone()));
                    instrs.push(Instr::Label(true_label));

                    instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));
                    instrs.push(Instr::Label(end_label));
                }
                Op1::Print => {
                    instrs.push(Instr::IMov(Val::Reg(Reg::RDI), Val::Reg(Reg::RAX)));
                    instrs.push(Instr::Call("_snek_print".to_string()));
                }
            }
            instrs
        }
        // TODO: BinOp - compile both operands using the stack
        Expr::BinOp(op,e1,e2) => {
            match op {
                Op2::Plus => {
                    let mut instrs = compile_to_instrs(e1, si, env, label_counter, break_target.clone());

                    instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
                    instrs.push(Instr::IAnd(Val::Reg(Reg::RBX), Val::Imm(NUM_TAG_MASK as i32)));
                    instrs.push(Instr::Cmp(Val::Reg(Reg::RBX), Val::Imm(NUM_TAG as i32)));
                    instrs.push(Instr::Jne("throw_error".to_string()));

                    instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, -8*si), Val::Reg(Reg::RAX)));

                    let mut right_instrs = compile_to_instrs(e2, si+1, env, label_counter, break_target.clone());
                    instrs.append(&mut right_instrs);

                    instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
                    instrs.push(Instr::IAnd(Val::Reg(Reg::RBX), Val::Imm(NUM_TAG_MASK as i32)));
                    instrs.push(Instr::Cmp(Val::Reg(Reg::RBX), Val::Imm(NUM_TAG as i32)));
                    instrs.push(Instr::Jne("throw_error".to_string()));

                    instrs.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, -8*si)));
                    instrs.push(Instr::Jo("throw_overflow".to_string()));
                    instrs
                },
                Op2::Minus => {
                    let mut instrs = compile_to_instrs(e1, si, env, label_counter, break_target.clone());

                    instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
                    instrs.push(Instr::IAnd(Val::Reg(Reg::RBX), Val::Imm(NUM_TAG_MASK as i32)));
                    instrs.push(Instr::Cmp(Val::Reg(Reg::RBX), Val::Imm(NUM_TAG as i32)));
                    instrs.push(Instr::Jne("throw_error".to_string()));

                    instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, -8*si), Val::Reg(Reg::RAX)));

                    let mut right_instrs = compile_to_instrs(e2, si+1, env, label_counter, break_target.clone());
                    instrs.append(&mut right_instrs);

                    instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
                    instrs.push(Instr::IAnd(Val::Reg(Reg::RBX), Val::Imm(NUM_TAG_MASK as i32)));
                    instrs.push(Instr::Cmp(Val::Reg(Reg::RBX), Val::Imm(NUM_TAG as i32)));
                    instrs.push(Instr::Jne("throw_error".to_string()));

                   instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
                   instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, -8*si)));

                   instrs.push(Instr::ISub(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
                   instrs.push(Instr::Jo("throw_overflow".to_string()));
                   instrs
                },
                Op2::Times => {
                    let mut instrs = compile_to_instrs(e1, si, env, label_counter, break_target.clone());

                    instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, -8*si), Val::Reg(Reg::RAX)));

                    instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
                    instrs.push(Instr::IAnd(Val::Reg(Reg::RBX), Val::Imm(NUM_TAG_MASK as i32)));
                    instrs.push(Instr::Cmp(Val::Reg(Reg::RBX), Val::Imm(NUM_TAG as i32)));
                    instrs.push(Instr::Jne("throw_error".to_string()));

                    let mut right_instrs = compile_to_instrs(e2, si+1, env, label_counter, break_target.clone());
                    instrs.append(&mut right_instrs);

                    instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
                    instrs.push(Instr::IAnd(Val::Reg(Reg::RBX), Val::Imm(NUM_TAG_MASK as i32)));
                    instrs.push(Instr::Cmp(Val::Reg(Reg::RBX), Val::Imm(NUM_TAG as i32)));
                    instrs.push(Instr::Jne("throw_error".to_string()));

                    instrs.push(Instr::Sar(Val::Reg(Reg::RAX), Val::Imm(1)));
                    instrs.push(Instr::IMul(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, -8*si)));
                    instrs.push(Instr::Jo("throw_overflow".to_string()));
                    instrs
                },
                Op2::Less => {
                    let true_label = new_label(label_counter, "less_true");
                    let end_label = new_label(label_counter, "less_end");

                    let mut instrs = vec![];
                    let mut left_instrs = compile_to_instrs(e1, si, env, label_counter, break_target.clone());
                    instrs.append(&mut left_instrs);

                    instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
                    instrs.push(Instr::IAnd(Val::Reg(Reg::RBX), Val::Imm(NUM_TAG_MASK as i32)));
                    instrs.push(Instr::Cmp(Val::Reg(Reg::RBX), Val::Imm(NUM_TAG as i32)));
                    instrs.push(Instr::Jne("throw_error".to_string()));

                    instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, -8*si), Val::Reg(Reg::RAX)));

                    let mut right_instrs = compile_to_instrs(e2, si+1, env, label_counter, break_target.clone());
                    instrs.append(&mut right_instrs);

                    instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
                    instrs.push(Instr::IAnd(Val::Reg(Reg::RBX), Val::Imm(NUM_TAG_MASK as i32)));
                    instrs.push(Instr::Cmp(Val::Reg(Reg::RBX), Val::Imm(NUM_TAG as i32)));
                    instrs.push(Instr::Jne("throw_error".to_string()));

                    instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, -8*si)));

                    instrs.push(Instr::Cmp(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
                    instrs.push(Instr::Jl(true_label.clone()));

                    instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
                    instrs.push(Instr::Jmp(end_label.clone()));

                    instrs.push(Instr::Label(true_label));
                    instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));
                    instrs.push(Instr::Label(end_label));
                    instrs },
                Op2::Greater => {
                        let true_label = new_label(label_counter, "less_true");
                        let end_label = new_label(label_counter, "less_end");
    
                        let mut instrs = vec![];
                        let mut left_instrs = compile_to_instrs(e1, si, env, label_counter, break_target.clone());
                        instrs.append(&mut left_instrs);

                        instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
                        instrs.push(Instr::IAnd(Val::Reg(Reg::RBX), Val::Imm(NUM_TAG_MASK as i32)));
                        instrs.push(Instr::Cmp(Val::Reg(Reg::RBX), Val::Imm(NUM_TAG as i32)));
                        instrs.push(Instr::Jne("throw_error".to_string()));
    
                        instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, -8*si), Val::Reg(Reg::RAX)));
    
                        let mut right_instrs = compile_to_instrs(e2, si+1, env, label_counter, break_target.clone());
                        instrs.append(&mut right_instrs);

                        instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
                        instrs.push(Instr::IAnd(Val::Reg(Reg::RBX), Val::Imm(NUM_TAG_MASK as i32)));
                        instrs.push(Instr::Cmp(Val::Reg(Reg::RBX), Val::Imm(NUM_TAG as i32)));
                        instrs.push(Instr::Jne("throw_error".to_string()));
    
                        instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
                        instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, -8*si)));
    
                        instrs.push(Instr::Cmp(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
                        instrs.push(Instr::Jg(true_label.clone()));
    
                        instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
                        instrs.push(Instr::Jmp(end_label.clone()));
    
                        instrs.push(Instr::Label(true_label));
                        instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));
                        instrs.push(Instr::Label(end_label));
                        instrs
                    },
                Op2::LessEq => {
                        let true_label = new_label(label_counter, "less_true");
                        let end_label = new_label(label_counter, "less_end");
    
                        let mut instrs = vec![];
                        let mut left_instrs = compile_to_instrs(e1, si, env, label_counter, break_target.clone());
                        instrs.append(&mut left_instrs);

                        instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
                        instrs.push(Instr::IAnd(Val::Reg(Reg::RBX), Val::Imm(NUM_TAG_MASK as i32)));
                        instrs.push(Instr::Cmp(Val::Reg(Reg::RBX), Val::Imm(NUM_TAG as i32)));
                        instrs.push(Instr::Jne("throw_error".to_string()));
    
                        instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, -8*si), Val::Reg(Reg::RAX)));
    
                        let mut right_instrs = compile_to_instrs(e2, si+1, env, label_counter, break_target.clone());
                        instrs.append(&mut right_instrs);

                        instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
                        instrs.push(Instr::IAnd(Val::Reg(Reg::RBX), Val::Imm(NUM_TAG_MASK as i32)));
                        instrs.push(Instr::Cmp(Val::Reg(Reg::RBX), Val::Imm(NUM_TAG as i32)));
                        instrs.push(Instr::Jne("throw_error".to_string()));
    
                        instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
                        instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, -8*si)));
    
                        instrs.push(Instr::Cmp(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
                        instrs.push(Instr::Jle(true_label.clone()));
    
                        instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
                        instrs.push(Instr::Jmp(end_label.clone()));
    
                        instrs.push(Instr::Label(true_label));
                        instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));
                        instrs.push(Instr::Label(end_label));
                        instrs
                },
                Op2::GreaterEq => {
                        let true_label = new_label(label_counter, "less_true");
                        let end_label = new_label(label_counter, "less_end");
    
                        let mut instrs = vec![];
                        let mut left_instrs = compile_to_instrs(e1, si, env, label_counter, break_target.clone());
                        instrs.append(&mut left_instrs);

                        instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
                        instrs.push(Instr::IAnd(Val::Reg(Reg::RBX), Val::Imm(NUM_TAG_MASK as i32)));
                        instrs.push(Instr::Cmp(Val::Reg(Reg::RBX), Val::Imm(NUM_TAG as i32)));
                        instrs.push(Instr::Jne("throw_error".to_string()));
    
                        instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, -8*si), Val::Reg(Reg::RAX)));
    
                        let mut right_instrs = compile_to_instrs(e2, si+1, env, label_counter, break_target.clone());
                        instrs.append(&mut right_instrs);

                        instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
                        instrs.push(Instr::IAnd(Val::Reg(Reg::RBX), Val::Imm(NUM_TAG_MASK as i32)));
                        instrs.push(Instr::Cmp(Val::Reg(Reg::RBX), Val::Imm(NUM_TAG as i32)));
                        instrs.push(Instr::Jne("throw_error".to_string()));
    
                        instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
                        instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, -8*si)));
    
                        instrs.push(Instr::Cmp(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
                        instrs.push(Instr::Jge(true_label.clone()));
    
                        instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(1)));
                        instrs.push(Instr::Jmp(end_label.clone()));
    
                        instrs.push(Instr::Label(true_label));
                        instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(3)));
                        instrs.push(Instr::Label(end_label));
                        instrs
            },
            Op2::Equal => {
                let true_label = new_label(label_counter, "equal_true");
                let end_label = new_label(label_counter, "equal_end");

                let mut instrs = vec![];

                let mut left_instrs = compile_to_instrs(e1, si, env, label_counter, break_target.clone());
                instrs.append(&mut left_instrs);

                instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, -8*si), Val::Reg(Reg::RAX)));  

                let mut right_instrs = compile_to_instrs(e2, si+1, env, label_counter, break_target.clone());
                instrs.append(&mut right_instrs);

                instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, -8*si)));

                instrs.push(Instr::IAnd(Val::Reg(Reg::RBX), Val::Imm(1)));
                instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, -8*(si+1)), Val::Reg(Reg::RAX)));
                instrs.push(Instr::IAnd(Val::Reg(Reg::RAX), Val::Imm(1)));
                instrs.push(Instr::Cmp(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
                
                instrs.push(Instr::Jne("throw_error".to_string()));

                // Restore full right value
                instrs.push(Instr::IMov(Val::Reg(Reg::RBX),Val::RegOffset(Reg::RSP, -8 * (si + 1))));

                // Restore full left value
                instrs.push(Instr::IMov(Val::Reg(Reg::RAX),Val::RegOffset(Reg::RSP, -8 * si)));

                instrs.push(Instr::Cmp(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
                instrs.push(Instr::Je(true_label.clone()));

                instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(FALSE_VAL as i32)));
                instrs.push(Instr::Jmp(end_label.clone()));

                instrs.push(Instr::Label(true_label));
                instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(TRUE_VAL as i32)));
                instrs.push(Instr::Label(end_label));
                instrs
            }
        
        }
    }
        //TODO: Add case for If
        Expr::If(cond,then_expr, else_expr) => {
            let else_label = new_label(label_counter, "if_else");
            let end_label = new_label(label_counter, "if_end");

            let mut instrs = vec![];
            // Compile condition
            let mut cond_instrs = compile_to_instrs(cond, si, env, label_counter, break_target.clone());
            instrs.append(&mut cond_instrs);
            // Check if false
            instrs.push(Instr::Cmp(Val::Reg(Reg::RAX), Val::Imm(FALSE_VAL as i32)));
            instrs.push(Instr::Je(else_label.clone()));
            // Compile then branch
            let mut then_instrs = compile_to_instrs(then_expr, si, env, label_counter, break_target.clone());
            instrs.append(&mut then_instrs);
            // Jump to end
            instrs.push(Instr::Jmp(end_label.clone()));
            instrs.push(Instr::Label(else_label));
            // Compile else branch
            let mut else_instrs = compile_to_instrs(else_expr, si, env, label_counter, break_target.clone());
            instrs.append(&mut else_instrs);

            instrs.push(Instr::Label(end_label));
            instrs
        } 
        //TODO: Add case for Block
        Expr::Block(exprs) => {
            let mut instrs = vec![];
            for e in exprs {
                let mut e_instrs = compile_to_instrs(e, si, env, label_counter, break_target.clone());
                instrs.append(&mut e_instrs);
            }
            instrs
        }
        // TODO: Add case for Loop
        Expr::Loop(body) => {
            let loop_start = new_label(label_counter, "loop_start");
            let loop_end = new_label(label_counter, "loop_end");

            let mut instrs = vec![];

            instrs.push(Instr::Label(loop_start.clone()));

            let mut body_instrs = compile_to_instrs(body, si, env, label_counter, Some(loop_end.clone()));
            instrs.append(&mut body_instrs);

            instrs.push(Instr::Jmp(loop_start));
            instrs.push(Instr::Label(loop_end));

            instrs
        }
        // TODO: Add case for Break
        Expr::Break(e) => {
            match break_target {
                Some(label) => {
                    let mut instrs = compile_to_instrs(e, si, env, label_counter, Some(label.clone()));
                    instrs.push(Instr::Jmp(label));
                    instrs
                }
                None => panic!("break outside of loop"),
            }
        }
        //TODO: Add case for Set
        Expr::Set(name,e) => {
            let offset = env.get(name).unwrap_or_else(|| panic!("Unbound variable identifier {}", name));


            let mut instrs = compile_to_instrs(e, si, env, label_counter, break_target.clone());

            instrs.push(Instr::IMov(Val::RegOffset(Reg::RBP, *offset), Val::Reg(Reg::RAX)));
            instrs
        }
        // TODO: Let - bind variables and compile body
        Expr::Let(bindings, body) => {
            let mut instrs = Vec::new();
            let mut seen = HashMap::new();
            let mut current_env = env.clone();
            let mut current_si = si;
            for (name, expr) in bindings {
                if seen.contains_key(name) {
                    panic!("Duplicate binding");
                }
                seen.insert(name.clone(), true);
                let mut bind_instrs = compile_to_instrs(expr, current_si, &current_env, label_counter, break_target.clone());
                instrs.append(&mut bind_instrs);
                instrs.push(Instr::IMov(Val::RegOffset(Reg::RBP, -8*current_si), Val::Reg(Reg::RAX)));
                current_env.insert(name.clone(), -8*current_si);
                current_si += 1;
            }
            let mut body_instrs = compile_to_instrs(body, current_si, &current_env, label_counter, break_target.clone());
            instrs.append(&mut body_instrs);
            instrs
        }


        // TODO: Call - compile function calls
        Expr::Call(name, args) => {
            let mut instrs = vec![];

            for arg in args.iter().rev(){
                let mut args_instrs = compile_to_instrs(arg, si, env, label_counter, break_target.clone());
                instrs.append(&mut args_instrs);
                instrs.push(Instr::Push(Val::Reg(Reg::RAX)));
            }

            instrs.push(Instr::Call(format!("fun_{}", name)));

            if !args.is_empty() {
                instrs.push(Instr::IAdd(Val::Reg(Reg::RSP), Val::Imm(8 * args.len() as i32)));
            }

            instrs
        }
        _ => panic!("Invalid {:?}", e),
    }
}

// ============= Code Generation =============

/// Convert a Val to its assembly string representation
fn val_to_str(v: &Val) -> String {
    match v {
        Val::Reg(Reg::RAX) => String::from("rax"),
        Val::Reg(Reg::RSP) => String::from("rsp"),
        Val::Reg(Reg::RBX) => String::from("rbx"),
        Val::Reg(Reg::RDI) => String::from("rdi"),
        Val::Reg(Reg::RBP) => String::from("rbp"),
        Val::Imm(n) => format!("{}", n),
        Val::RegOffset(Reg::RSP, offset) => format!("[rsp + {}]", offset),
        Val::RegOffset(Reg::RAX, offset) => format!("[rax + {}]", offset),
        Val::RegOffset(Reg::RBX, offset) => format!("[rbx + {}]", offset),
        Val::RegOffset(Reg::RDI, offset) => format!("[rdi + {}]", offset),
        Val::RegOffset(Reg::RBP, offset) => format!("[rbp + {}]", offset),
    
    
    }
}

/// Convert an Instr to its assembly string representation
fn instr_to_str(i: &Instr) -> String {
    match i {
        Instr::IMov(dst, src) => format!("mov {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::IAdd(dst, src) => format!("add {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::ISub(dst, src) => format!("sub {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::IMul(dst, src) => format!("imul {}, {}", val_to_str(dst), val_to_str(src)),
        // Add cases for IAnd, Cmp, Jmp, Neg, Je, Jne, and Label
        Instr::IAnd(dst, src) => format!("and {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::Cmp(dst, src) => format!("cmp {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::Jmp(label) => format!("jmp {}", label),
        Instr::Neg(dst) => format!("neg {}", val_to_str(dst)),
        Instr::Je(label) => format!("je {}", label),
        Instr::Jne(label) => format!("jne {}", label),
        Instr::Label(label) => format!("{}:", label),
        Instr::Jl(label) => format!("jl {}", label),
        Instr::Jg(label) => format!("jg {}", label),
        Instr::Jle(label) => format!("jle {}", label),
        Instr::Jge(label) => format!("jge {}", label),
        Instr::Sar(dst, src) => format!("sar {}, {}", val_to_str(dst), val_to_str(src)),
        Instr::Jo(label) => format!("jo {}", label),
        Instr::Push(v) => format!("push {}", val_to_str(v)),
        Instr::Pop(v) => format!("pop {}", val_to_str(v)),
        Instr::Call(label) => format!("call {}", label),
        Instr::Ret => String::from("ret"),
        

    }
}

// Add compile_defn helper
fn compile_defn(defn: &Definition, label_counter: &mut i32) -> String {
    let mut env = HashMap::new();

    for (i, param) in defn.params.iter().enumerate(){
        env.insert(param.clone(), 16 + (i as i32) * 8);
    }

    let body_instrs = compile_to_instrs(&defn.body, 1, &env, label_counter, None);
    let body_asm = body_instrs.iter().map(instr_to_str).collect::<Vec<String>>().join("\n   ");

    format!("fun_{}:
    push rbp
    mov rbp, rsp
    {}
    pop rbp
    ret",
        defn.name, body_asm)
}

// Add compile_program
fn compile_program(prog: &Program) -> String {
    let mut label_counter = 0;

    let defns_asm= prog.defns.iter().map(|p| compile_defn(p, &mut label_counter))
    .collect::<Vec<String>>().join("\n\n");

    let env: HashMap<String, i32> = HashMap::new();
    let main_instrs = compile_to_instrs(&prog.main, 2, &env, &mut label_counter, None);
    let main_asm = main_instrs.iter().map(instr_to_str).collect::<Vec<String>>().join("\n   ");

    if defns_asm.is_empty() {
        format!("our_code_starts_here:
  push rbp
  mov rbp, rsp
  {}
  pop rbp
  ret
",
            main_asm)
    } else {
        format!(
            "{}

our_code_starts_here:
  push rbp
  mov rbp, rsp
  {}
  pop rbp
  ret
",
            defns_asm, main_asm
        )
    }
}

/// Compile an expression to a complete assembly string
// fn compile(e: &Expr) -> String {
//     let env: HashMap<String, i32> = HashMap::new();
//     let mut label_counter = 0;
//     let instrs = compile_to_instrs(e, 2, &env, &mut label_counter, None);
//     instrs
//         .iter()
//         .map(|i| instr_to_str(i))
//         .collect::<Vec<String>>()
//         .join("\n  ")
// }

// ============= Main =============

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    if args.len() != 3 {
        eprintln!("Usage: {} <input.snek> <output.s>", args[0]);
        std::process::exit(1);
    }

    let in_name = &args[1];
    let out_name = &args[2];

    // Read input file
    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;

    // Parse S-expression from text
    let sexp = parse(&in_contents).unwrap_or_else(|_| panic!("Invalid"));

    // Convert S-expression to our AST
    let prog = parse_program(&sexp);

    // Generate assembly instructions
    let instrs = compile_program(&prog);

    // Wrap instructions in assembly program template
    let asm_program = format!(
        "section .text
extern _snek_error
extern _snek_print
global our_code_starts_here
{}
throw_error:
    mov rdi, 1
    call _snek_error
throw_overflow:
    mov rdi, 2
    call _snek_error
",
        instrs
    );

    // Write output assembly file
    let mut out_file = File::create(out_name)?;
    out_file.write_all(asm_program.as_bytes())?;

    Ok(())
}

// ============= TESTS =============
//
// Run with: cargo test
//
// These tests help verify your implementation. Uncomment and add more!

#[cfg(test)]
mod tests {
    use super::*;

    // Helper to parse a string directly
    fn parse_str(s: &str) -> Expr {
        parse_expr(&parse(s).unwrap())
    }

    // ===== Parsing Tests =====

    #[test]
    fn test_parse_number() {
        let expr = parse_str("42");
        match expr {
            Expr::Number(42) => (),
            _ => panic!("Expected Number(42), got {:?}", expr),
        }
    }

    #[test]
    fn test_parse_identifier() {
        let expr = parse_str("x");
        match expr {
            Expr::Id(name) => assert_eq!(name, "x"),
            _ => panic!("Expected Id(\"x\"), got {:?}", expr),
        }
    }

    #[test]
    fn test_parse_add1() {
        let expr = parse_str("(add1 5)");
        match expr {
            Expr::UnOp(Op1::Add1, _) => (),
            _ => panic!("Expected UnOp(Add1, ...), got {:?}", expr),
        }
    }

    #[test]
    fn test_parse_binary_plus() {
        let expr = parse_str("(+ 1 2)");
        match expr {
            Expr::BinOp(Op2::Plus, _, _) => (),
            _ => panic!("Expected BinOp(Plus, ...), got {:?}", expr),
        }
    }

    #[test]
    fn test_parse_let_simple() {
        let expr = parse_str("(let ((x 5)) x)");
        match expr {
            Expr::Let(bindings, _) => {
                assert_eq!(bindings.len(), 1);
                assert_eq!(bindings[0].0, "x");
            }
            _ => panic!("Expected Let, got {:?}", expr),
        }
    }

    #[test]
    fn test_parse_let_multiple_bindings() {
        let expr = parse_str("(let ((x 5) (y 6)) (+ x y))");
        match expr {
            Expr::Let(bindings, _) => {
                assert_eq!(bindings.len(), 2);
            }
            _ => panic!("Expected Let with 2 bindings, got {:?}", expr),
        }
    }

    // ===== Error Tests =====

    #[test]
    #[should_panic(expected = "Duplicate binding")]
    fn test_duplicate_binding() {
        let expr = parse_str("(let ((x 1) (x 2)) x)");
        let env: HashMap<String, i32> = HashMap::new();
        let mut label_counter = 0;
        compile_to_instrs(&expr, 2, &env, &mut label_counter, None);
    }

    #[test]
    #[should_panic(expected = "Unbound variable identifier y")]
    fn test_unbound_variable() {
        let expr = parse_str("y");
        let env: HashMap<String, i32> = HashMap::new();
        let mut label_counter = 0;
        compile_to_instrs(&expr, 2, &env, &mut label_counter, None);
    }

    // ===== Compilation Tests =====

    #[test]
    fn test_compile_number() {
        let expr = Expr::Number(42);
        let env: HashMap<String, i32> = HashMap::new();
        let mut label_counter = 0;
        let instrs = compile_to_instrs(&expr, 2, &env, &mut label_counter, None);
        assert_eq!(instrs.len(), 1);
    }

    // Add more tests as you implement features!
}
