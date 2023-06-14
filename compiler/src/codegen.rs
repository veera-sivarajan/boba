use crate::expr::Expr;
use crate::lexer::{Token, TokenType};
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

use std::fmt;
impl fmt::Display for RegisterIndex {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

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
        Self { count: 0 }
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
            self.codegen(ele);
        }
    }

    fn codegen(&mut self, expr: &Expr) -> RegisterIndex {
        match expr {
            Expr::Binary { left, oper, right } => {
                self.binary(left, oper, right)
            }
            Expr::Number(num) => self.number(*num),
        }
    }

    fn binary(&mut self, left: &Expr, oper: &Token, right: &Expr ) -> RegisterIndex {
        let left_register = self.codegen(left);
        let right_register = self.codegen(right);
        match oper.kind {
            TokenType::Plus => {
                println!("ADDQ {}, {}", self.registers.name(&left_register), self.registers.name(&right_register));
                self.registers.deallocate(left_register);
                right_register
            }
            TokenType::Minus => {
                println!("SUBQ {}, {}", self.registers.name(&left_register), self.registers.name(&right_register));
                self.registers.deallocate(left_register);
                right_register
            }
            TokenType::Star => {
                let result_register = self.registers.allocate();
                println!("MOV  {}, %rax", self.registers.name(&right_register));
                println!("IMUL {}", self.registers.name(&left_register));
                println!("MOV  %rax, {}", self.registers.name(&result_register));
                self.registers.deallocate(left_register);
                self.registers.deallocate(right_register);
                result_register
            }
            _ => todo!(),

        }
    }

    fn number(&mut self, value: f64) -> RegisterIndex {
        let register = self.registers.allocate();
        println!("MOV  ${}, {}", value as u64,  self.registers.name(&register));
        register
    }
}
