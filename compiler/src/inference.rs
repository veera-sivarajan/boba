use crate::error::BobaError;
use crate::expr::Expr;
use crate::lexer::{Token, TokenType};
use crate::stmt::Stmt;
use std::collections::HashMap;

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub enum Term {
    Expr(Expr),  // variable
    Var(String), // variable
    Num,         // constant
    Bool,
    Arrow(ArrowType), // function application
}

impl Term {
    fn is_ident(&self) -> bool {
        matches!(self, Term::Expr(_) | Term::Var(_))
    }

    pub fn make_arrow(domain: Term, range: Term) -> Self {
        Term::Arrow(ArrowType {
            domain: Box::new(domain),
            range: Box::new(range),
        })
    }
}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub struct ArrowType {
    domain: Box<Term>,
    range: Box<Term>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Substitution {
    var: Term,
    is: Term,
}

impl Substitution {
    pub fn new(var: Term, is: Term) -> Self {
        Self { var, is }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Constraint {
    lhs: Term,
    rhs: Term,
}

impl Constraint {
    fn new(lhs: Term, rhs: Term) -> Self {
        Constraint { lhs, rhs }
    }
}

// pub fn infer_types(ast: &[Stmt]) -> Vec<Substitution> {
//     let mut cons = vec![];
//     for stmt in ast {
//         generate_constraints(stmt, &mut cons);
//     }
//     unify(&mut cons, &mut vec![])
// }

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub struct Symbol {
    name: String,
    is_mutable: bool,
}

pub enum Type {
    Number,
    Bool,
    String,
    Char,
    Array(Box<Type>),
    Unknown,
}

pub struct StaticAnalysis {
    context: Vec<HashMap<Symbol, Type>>,
    constraints: Vec<Constraint>,
}

impl StaticAnalysis {
    pub fn new() -> Self {
        Self {
            context: vec![HashMap::new()],
            constraints: vec![],
        }
    }

    pub fn check(&mut self, ast: &[Stmt]) -> Result<(), BobaError> {
        for stmt in ast {
            self.check_stmt(stmt)?;
        }
        Ok(())
    }

    fn check_stmt(&mut self, stmt: &Stmt) -> Result<(), BobaError> {
        match stmt {
            Stmt::If { condition, .. } => {
                self.generate_constraints(condition);
                self.constraints.push(Constraint::new(
                    Term::Expr(condition.clone()),
                    Term::Bool,
                ));
                Ok(())
            }
            Stmt::Let {
                name,
                init,
                is_mutable,
            } => {
                let var_name = name.to_string();
                if self.is_global_scope() && *is_mutable {
                    Err(BobaError::TypeError {
                        msg: format!("Global variable '{var_name}' cannot be mutable")
                            .into(),
                        span: name.span,
                    })
                } else if self
                    .variable_is_declared(&var_name)
                    .is_some_and(|scope| scope == self.scope())
                {
                    Err(BobaError::TypeError {
                        msg: format!("Cannot redeclare variable '{var_name}'")
                            .into(),
                        span: name.span,
                    })
                } else if !is_mutable && init.is_none() {
                    Err(BobaError::TypeError {
                        msg: format!("Immutable variable '{var_name}' cannot be declared without initializing.").into(),
                        span: name.span,
                    })
                } else {
                    self.add_variable(&var_name, *is_mutable);
                    if let Some(expr) = init {
                        self.generate_constraints(expr);
                        self.constraints.push(Constraint::new(
                            Term::Var(var_name),
                            Term::Expr(expr.clone()),
                        ));
                    }
                    Ok(())
                }
            }
            Stmt::Block(stmts) => {
                // self.context.push(HashMap::new());
                // self.check(stmts)?;
                // Ok(())
                todo!()
            }
            _ => todo!(),
        }
    }

    fn scope(&self) -> u8 {
        self.context.len() as u8 - 1
    }

    fn is_global_scope(&self) -> bool {
        self.scope() == 0
    }

    fn variable_is_declared(&self, name: &str) -> Option<u8> {
        for (index, scope) in self.context.iter().rev().enumerate() {
            for (key, _value) in scope {
                if key.name == name {
                    return Some(index as u8);
                }
            }
        }
        None
    }

    fn add_variable(&mut self, name: &str, is_mutable: bool) {
        if let Some(map) = self.context.last_mut() {
            let name = name.to_string();
            map.insert(Symbol { name, is_mutable }, Type::Unknown);
        }
    }

    fn generate_constraints(&mut self, expr: &Expr) {
        match expr {
            Expr::Number(_) => {
                self.constraints.push(Constraint {
                    lhs: Term::Expr(expr.clone()),
                    rhs: Term::Num,
                });
            }
            Expr::Boolean(_) => {
                self.constraints.push(Constraint {
                    lhs: Term::Expr(expr.clone()),
                    rhs: Term::Bool,
                });
            }
            Expr::Binary { left, oper, right } => {
                self.generate_constraints(left);
                self.generate_constraints(right);
                self.constraints.extend(vec![
                    Constraint::new(Term::Expr(*left.clone()), Term::Num),
                    Constraint::new(Term::Expr(*right.clone()), Term::Num),
                ]);
                match oper.kind {
                    TokenType::Plus
                    | TokenType::Minus
                    | TokenType::Slash
                    | TokenType::Star => {
                        self.constraints.push(Constraint::new(
                            Term::Expr(expr.clone()),
                            Term::Num,
                        ));
                    }
                    TokenType::Less
                    | TokenType::LessEqual
                    | TokenType::GreaterEqual
                    | TokenType::Greater => {
                        self.constraints.push(Constraint::new(
                            Term::Expr(expr.clone()),
                            Term::Bool,
                        ));
                    }
                    _ => unreachable!(),
                }
            }
            Expr::Variable(token) => {
                let name = token.to_string();
                self.constraints.push(Constraint {
                    lhs: Term::Expr(expr.clone()),
                    rhs: Term::Var(name),
                });
            }
            _ => {
                eprintln!("{expr:?}");
                todo!()
            }
        }
    }
}

fn occurs_check(left: &Term, right: &Term) -> bool {
    match left {
        Term::Arrow(ArrowType { domain, range }) => {
            occurs_check(left, domain) || occurs_check(left, range)
        }
        _ => left == right,
    }
}

fn replace(left: &Term, term: &Term, right: &Term) -> Term {
    match term {
        Term::Arrow(ArrowType { domain, range }) => Term::Arrow(ArrowType {
            domain: Box::new(replace(left, domain, right)),
            range: Box::new(replace(left, range, right)),
        }),
        _ => {
            if left == term {
                right.clone()
            } else {
                term.clone()
            }
        }
    }
}

fn replace_all(
    left: &Term,
    right: &Term,
    consts: &mut [Constraint],
    subst: &mut [Substitution],
) {
    if !occurs_check(left, right) {
        for c in consts.iter_mut() {
            c.lhs = replace(left, &c.lhs, right);
            c.rhs = replace(left, &c.rhs, right);
        }

        for s in subst.iter_mut() {
            s.var = replace(left, &s.var, right);
            s.is = replace(left, &s.is, right);
        }
    } else {
        panic!("Occurs check failed.");
    }
}

fn unify(
    consts: &mut Vec<Constraint>,
    subs: &mut Vec<Substitution>,
) -> Vec<Substitution> {
    if consts.is_empty() {
        subs.to_vec()
    } else {
        let (first, rest) = consts.split_at_mut(1);
        let first = first.first().unwrap();

        let left = first.lhs.clone();
        let right = first.rhs.clone();

        if left == right {
            unify(&mut rest.to_vec(), subs)
        } else if left.is_ident() {
            replace_all(&left, &right, rest, subs);
            subs.push(Substitution::new(left, right));
            unify(&mut rest.to_vec(), subs)
        } else if right.is_ident() {
            replace_all(&right, &left, rest, subs);
            subs.push(Substitution::new(right, left));
            unify(&mut rest.to_vec(), subs)
        } else {
            match (&left, &right) {
                (
                    Term::Arrow(ArrowType {
                        domain: d_one,
                        range: r_one,
                    }),
                    Term::Arrow(ArrowType {
                        domain: d_two,
                        range: r_two,
                    }),
                ) => {
                    let mut new_rest = rest.to_vec();
                    new_rest.extend(vec![
                        Constraint::new(*d_one.clone(), *d_two.clone()),
                        Constraint::new(*r_one.clone(), *r_two.clone()),
                    ]);
                    unify(&mut new_rest, subs)
                }
                _ => {
                    for sub in subs {
                        println!("Found: {sub}");
                    }
                    let msg = format!("{left} and {right} do not unify.");
                    panic!("{msg}");
                }
            }
        }
    }
}

use std::fmt;
impl fmt::Display for Constraint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} = {}", self.lhs, self.rhs)
    }
}

impl fmt::Display for Substitution {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} :: {}", self.var, self.is)
    }
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Term::Var(c) => write!(f, "{c}"),
            Term::Num => write!(f, "Number"),
            Term::Bool => write!(f, "Bool"),
            Term::Arrow(a_type) => {
                write!(f, "{} -> {}", a_type.domain, a_type.range)
            }
            Term::Expr(e) => write!(f, "{e}"),
        }
    }
}
