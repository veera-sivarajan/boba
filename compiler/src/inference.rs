use crate::error::BobaError;
use crate::expr::Expr;
use crate::lexer::{Token, TokenType};
use crate::stmt::Stmt;

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub enum Term {
    Expr(Expr), // variable
    Var(String),  // variable
    Num,        // constant
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

pub fn infer_types(ast: &[Stmt]) -> Vec<Substitution> {
    let mut cons = vec![];
    for stmt in ast {
        generate_constraints(stmt, &mut cons);
    }
    unify(&mut cons, &mut vec![])
}

struct Symbol {
    name: Token,
    is_mutable: bool,
    is_global: bool,
    kind: Term,
}

fn generate_constraints(stmt: &Stmt, constraints: &mut Vec<Constraint>) {
    match stmt {
        Stmt::If { condition, .. } => {
            generate_constraints_expr(condition, constraints);
            let result = vec![Constraint::new(
                Term::Expr(condition.clone()),
                Term::Bool,
            )];
            constraints.extend(result);
        }
        Stmt::Let { name, init, .. } => {
            let name = name.to_string();
            if let Some(expr) = init {
                generate_constraints_expr(expr, constraints);
                constraints.push(Constraint::new(
                    Term::Var(name),
                    Term::Expr(expr.clone())
                ));
            }
        }
        _ => todo!(),
    }
}

fn generate_constraints_expr(expr: &Expr, constraints: &mut Vec<Constraint>) {
    match expr {
        Expr::Number(_) => {
            constraints.push(Constraint {
                lhs: Term::Expr(expr.clone()),
                rhs: Term::Num,
            });
        }
        Expr::Boolean(_) => {
            constraints.push(Constraint {
                lhs: Term::Expr(expr.clone()),
                rhs: Term::Bool,
            });
        }
        Expr::Binary { left, oper, right } => {
            generate_constraints_expr(left, constraints);
            generate_constraints_expr(right, constraints);
            let mut consequent = vec![
                Constraint::new(Term::Expr(*left.clone()), Term::Num),
                Constraint::new(Term::Expr(*right.clone()), Term::Num),
            ];
            match oper.kind {
                TokenType::Plus
                | TokenType::Minus
                | TokenType::Slash
                | TokenType::Star => {
                    consequent.push(Constraint::new(
                        Term::Expr(expr.clone()),
                        Term::Num,
                    ));
                    constraints.extend(consequent);
                }
                TokenType::Less
                | TokenType::LessEqual
                | TokenType::GreaterEqual
                | TokenType::Greater => {
                    consequent.push(Constraint::new(
                        Term::Expr(expr.clone()),
                        Term::Bool,
                    ));
                    constraints.extend(consequent);
                }
                _ => unreachable!(),
            }
        }
        Expr::Variable(token) => {
            let name = token.to_string();
            constraints.push(Constraint {
                lhs: Term::Expr(expr.clone()),
                rhs: Term::Var(name),
            });
        }
        _ => {
            eprintln!("{expr:?}");
            todo!()
        },
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
