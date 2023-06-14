use crate::expr::Expr;
use crate::lexer::Token;
use std::rc::Rc;

struct Register {
    name: Rc<str>,
    inuse: bool,
}

impl Register {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.into(),
            inuse: false,
        }
    }
}

struct ScratchRegisters {
    table: [Register; 7],
}

struct RegisterIndex(u8);

impl ScratchRegisters {
    pub fn new() -> Self {
        Self {
            table: [
                Register::new("%rbx"),
                Register::new("%r10"),
                Register::new("%r11"),
                Register::new("%r12"),
                Register::new("%r13"),
                Register::new("%r14"),
                Register::new("%r15"),
            ],
        }
    }

    pub fn allocate(&mut self) -> RegisterIndex {
        for (index, register) in self.table.iter_mut().enumerate() {
            if !register.inuse {
                register.inuse = true;
                return RegisterIndex(index as u8);
            }
        }
        unreachable!()
    }

    pub fn deallocate(&mut self, index: RegisterIndex) {
        let mut register = self.table.get_mut(index.0 as usize).unwrap();
        register.inuse = false;
    }

    pub fn name(&self, index: &RegisterIndex) -> Rc<str> {
        let register = self.table.get(index.0 as usize).unwrap();
        register.name.clone()
    }
}

struct LabelIndex(u32);

struct Labels {
    count: u32,
}

impl Labels {
    fn new() -> Self {
        Self {
            count: 0,
        }
    }

    fn create(&mut self) -> LabelIndex {
        let index = LabelIndex(self.count);
        self.count += 1;
        index
    }

    fn name(&self, index: LabelIndex) -> Box<str> {
        format!(".L{}", index.0).into()
    }
}

pub struct CodeGen {
    registers: ScratchRegisters,
    labels: Labels,
}

impl CodeGen {
    pub fn new() -> Self {
        Self {
            registers: ScratchRegisters::new(),
            labels: Labels::new(),
        }
    }

    pub fn generate_assembly(&mut self, ast: &[Expr]) {
        for ele in ast {
            match ele {
                Expr::Binary { left, oper, right} => {
                    self.binary(left, oper, right);
                }
                Expr::Number(num) => self.number(*num),
            }
        }
    }

    fn binary(&mut self, left: &Expr, oper: &Token, right: &Expr) {


    }

    fn number(&mut self, value: f64) {


    }
}
