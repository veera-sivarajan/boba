use crate::stmt::Stmt;
use crate::expr::Expr;
use crate::error::BobaError;

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub enum Term {
    Expr(Expr), // variable
    Var(char),  // variable
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

pub fn infer_types(ast: &[Stmt]) -> Result<Vec<Substitution>, BobaError> {
    todo!()
}

fn generate_constraints(stmt: &Stmt, constraints: &mut Vec<Constraint>) {
    todo!()
}
    
fn occurs_check(left: &Term, right: &Term) -> bool {
    todo!()
}

fn replace(left: &Term, term: &Term, right: &Term) -> Term {
    todo!()
}

fn replace_all(
    left: &Term,
    right: &Term,
    consts: &mut [Constraint],
    subst: &mut [Substitution],
) {
    todo!()
}

fn unify(
    consts: &mut Vec<Constraint>,
    subs: &mut Vec<Substitution>,
) -> Result<Vec<Substitution>, BobaError> {
    todo!()
}
