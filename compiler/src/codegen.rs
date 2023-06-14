use crate::expr::Expr;
use crate::lexer::{Token, TokenType};
use std::fmt::Write;
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

#[derive(Default, Clone)]
pub struct Assembly {
    pub header: String,
    pub code: String,
    pub runtime: String,
}

pub struct CodeGen {
    registers: ScratchRegisters,
    assembly: Assembly,
}

impl CodeGen {
    pub fn new() -> Self {
        Self {
            registers: ScratchRegisters::new(),
            assembly: Assembly::default(),
        }
    }

    pub fn generate_assembly(&mut self, ast: &[Expr]) -> Assembly {
        for ele in ast {
            self.assembly_helper(ele);
        }
        self.assembly.clone()
    }

    fn generate_header(&mut self) -> fmt::Result {
        writeln!(&mut self.assembly.header, "# header")?;
        writeln!(&mut self.assembly.header, ".globl main")?;
        writeln!(&mut self.assembly.header, ".LC0:")?;
        writeln!(&mut self.assembly.header, r#".string "%d\n""#)?;
        writeln!(&mut self.assembly.header, "main: ")
    }

    fn generate_runtime(&mut self, result: &RegisterIndex) -> fmt::Result {
        writeln!(&mut self.assembly.runtime, "# code for printf()")?;
        writeln!(&mut self.assembly.runtime, "PUSHQ %rbp")?;
        writeln!(&mut self.assembly.runtime, "MOVQ %rsp, %rbp")?;
        writeln!(
            &mut self.assembly.runtime,
            "MOVQ  {}, %rsi",
            self.registers.name(result)
        )?;
        writeln!(&mut self.assembly.runtime, "LEAQ .LC0(%rip), %rax")?;
        writeln!(&mut self.assembly.runtime, "MOVQ %rax, %rdi")?;
        writeln!(&mut self.assembly.runtime, "MOVL $0, %eax")?;
        writeln!(&mut self.assembly.runtime, "CALL printf@PLT")?;
        writeln!(&mut self.assembly.runtime, "MOVL $0, %eax")?;
        writeln!(&mut self.assembly.runtime, "POPQ %rbp")?;
        writeln!(&mut self.assembly.runtime, "RET")
    }

    fn assembly_helper(&mut self, expr: &Expr) {
        self.generate_header().expect("Unable to write header.");
        let result = self.generate_code(expr).expect("Unable to write code.");
        self.generate_runtime(&result)
            .expect("Unable to write runtime.");
    }

    fn generate_code(&mut self, expr: &Expr) -> Result<RegisterIndex, fmt::Error> {
        writeln!(&mut self.assembly.code, "# generated assembly")?;
        self.codegen(expr)
    }
    
    fn codegen(&mut self, expr: &Expr) -> Result<RegisterIndex, fmt::Error> {
        match expr {
            Expr::Binary { left, oper, right } => {
                self.binary(left, oper, right)
            }
            Expr::Number(num) => self.number(*num),
        }
    }

    fn binary(
        &mut self,
        left: &Expr,
        oper: &Token,
        right: &Expr,
    ) -> Result<RegisterIndex, fmt::Error> {
        let left_register = self.codegen(left)?;
        let right_register = self.codegen(right)?;
        match oper.kind {
            TokenType::Plus => {
                writeln!(
                    &mut self.assembly.code,
                    "ADDQ {}, {}",
                    self.registers.name(&left_register),
                    self.registers.name(&right_register)
                )?;
                self.registers.deallocate(left_register);
                Ok(right_register)
            }
            TokenType::Minus => {
                writeln!(
                    &mut self.assembly.code,
                    "SUBQ {}, {}",
                    self.registers.name(&left_register),
                    self.registers.name(&right_register)
                )?;
                self.registers.deallocate(left_register);
                Ok(right_register)
            }
            TokenType::Star => {
                let result_register = self.registers.allocate();
                writeln!(
                    &mut self.assembly.code,
                    "MOVQ {}, %rax",
                    self.registers.name(&right_register)
                )?;
                writeln!(
                    &mut self.assembly.code,
                    "IMUL {}",
                    self.registers.name(&left_register)
                )?;
                writeln!(
                    &mut self.assembly.code,
                    "MOVQ %rax, {}",
                    self.registers.name(&result_register)
                )?;
                self.registers.deallocate(left_register);
                self.registers.deallocate(right_register);
                Ok(result_register)
            }
            TokenType::Slash => {
                let result_register = self.registers.allocate();
                writeln!(
                    &mut self.assembly.code,
                    "MOVQ {}, %rax",
                    self.registers.name(&left_register)
                )?;
                writeln!(&mut self.assembly.code, "CQO")?;
                writeln!(
                    &mut self.assembly.code,
                    "IDIV {}",
                    self.registers.name(&right_register)
                )?;
                writeln!(
                    &mut self.assembly.code,
                    "MOVQ %rax, {}",
                    self.registers.name(&result_register)
                )?;
                self.registers.deallocate(left_register);
                self.registers.deallocate(right_register);
                Ok(result_register)
            }
            _ => todo!(),
        }
    }

    fn number(&mut self, value: f64) -> Result<RegisterIndex, fmt::Error> {
        let register = self.registers.allocate();
        writeln!(
            &mut self.assembly.code,
            "MOVQ ${}, {}",
            value as u64,
            self.registers.name(&register)
        )?;
        Ok(register)
    }
}
