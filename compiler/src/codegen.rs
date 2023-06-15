use crate::error::BobaError;
use crate::expr::Expr;
use crate::lexer::{Token, TokenType};
use crate::stmt::Stmt;
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
    pub data: String,
}

pub struct CodeGen {
    globals: Vec<Token>,
    registers: ScratchRegisters,
    assembly: Assembly,
}

impl CodeGen {
    pub fn new() -> Self {
        Self {
            globals: Vec::new(),
            registers: ScratchRegisters::new(),
            assembly: Assembly {
                code: "# generated assembly\n".to_string(),
                data: ".data\n".to_string(),
                ..Assembly::default()
            },
        }
    }

    pub fn compile(&mut self, ast: &[Stmt]) -> Result<Assembly, BobaError> {
        self.generate_header()?;
        for ele in ast {
            self.codegen(ele)?;
        }
        Ok(self.assembly.clone())
    }

    fn generate_header(&mut self) -> fmt::Result {
        writeln!(
            &mut self.assembly.header,
            r#"
# header
        .globl main 

        .LC0:
        .string "%d\n"
main: 
"#
        )
    }

    fn codegen(&mut self, stmt: &Stmt) -> Result<(), BobaError> {
        match stmt {
            Stmt::Let {
                name,
                is_mutable: _,
                init,
            } => self.let_stmt(name, init),
            Stmt::Expression(expr) => todo!(),
            Stmt::Print(expr) => self.print_stmt(expr),
        }
    }

    fn print_stmt(&mut self, expr: &Expr) -> Result<(), BobaError> {
        let register = self.expression(expr)?;
        self.emit_code("# code for printf()")?;
        self.emit_code("PUSHQ %rbp")?;
        self.emit_code("MOVQ %rsp, %rbp")?;
        self.emit_code(format!("MOVQ  {}, %rsi", self.registers.name(&register)).as_str())?;
        self.emit_code("LEAQ .LC0(%rip), %rax")?;
        self.emit_code("MOVQ %rax, %rdi")?;
        self.emit_code("MOVL $0, %eax")?;
        self.emit_code("CALL printf@PLT")?;
        self.emit_code("MOVL $0, %eax")?;
        self.emit_code("POPQ %rbp")?;
        self.emit_code("RET")
    }

    fn emit_data(&mut self, value: &str) -> Result<(), BobaError> {
        writeln!(&mut self.assembly.data, "{value}").map_err(|e| e.into())
    }

    fn emit_code(&mut self, value: &str) -> Result<(), BobaError> {
        writeln!(&mut self.assembly.code, "{value}").map_err(|e| e.into())
    }

    fn let_stmt(
        &mut self,
        name: &Token,
        init: &Option<Expr>,
    ) -> Result<(), BobaError> {
        if let TokenType::Identifier(variable_name) = &name.kind {
            if let Some(Expr::Number(value)) = init {
                self.add_global(name);
                self.emit_data(
                    format!("{variable_name}:  .quad {value}").as_str(),
                )
            } else {
                Err(BobaError::Compiler {
                    msg: "Global variables should be initated with constants"
                        .into(),
                    span: name.span,
                })
            }
        } else {
            unreachable!()
        }
    }

    fn expression(&mut self, expr: &Expr) -> Result<RegisterIndex, BobaError> {
        match expr {
            Expr::Binary { left, oper, right } => {
                self.binary(left, oper, right)
            }
            Expr::Number(num) => self.number(*num),
            Expr::Variable(token) => self.variable(token),
        }
    }

    fn symbol(&self, token: &Token) -> Result<String, BobaError> {
        let symbol_name = token.identifier_name();
        if self.globals.contains(token) {
            Ok(format!("{symbol_name}(%rip)"))
        } else {
            Err(BobaError::Compiler {
                msg: format!("Undeclared variable {symbol_name}").into(),
                span: token.span,
            })
        }
    }

    fn add_global(&mut self, token: &Token) {
        self.globals.push(token.clone());
    }

    fn variable(&mut self, token: &Token) -> Result<RegisterIndex, BobaError> {
        let register = self.registers.allocate();
        self.emit_code(format!("MOVQ {}, {}", self.symbol(token)?, self.registers.name(&register)).as_str())?;
        Ok(register)
    }

    fn binary(
        &mut self,
        left: &Expr,
        oper: &Token,
        right: &Expr,
    ) -> Result<RegisterIndex, BobaError> {
        let left_register = self.expression(left)?;
        let right_register = self.expression(right)?;
        match oper.kind {
            TokenType::Plus => {
                self.emit_code(
                    format!(
                        "ADDQ {}, {}",
                        self.registers.name(&left_register),
                        self.registers.name(&right_register)
                    )
                    .as_str(),
                )?;
                self.registers.deallocate(left_register);
                Ok(right_register)
            }
            TokenType::Minus => {
                self.emit_code(
                    format!(
                        "SUBQ {}, {}",
                        self.registers.name(&left_register),
                        self.registers.name(&right_register)
                    )
                    .as_str(),
                )?;
                self.registers.deallocate(left_register);
                Ok(right_register)
            }
            TokenType::Star => {
                let result_register = self.registers.allocate();
                self.emit_code(
                    format!(
                        "MOVQ {}, %rax",
                        self.registers.name(&right_register)
                    )
                    .as_str(),
                )?;
                self.emit_code(
                    format!("IMUL {}", self.registers.name(&left_register))
                        .as_str(),
                )?;
                self.emit_code(
                    format!(
                        "MOVQ %rax, {}",
                        self.registers.name(&result_register)
                    )
                    .as_str(),
                )?;
                self.registers.deallocate(left_register);
                self.registers.deallocate(right_register);
                Ok(result_register)
            }
            TokenType::Slash => {
                let result_register = self.registers.allocate();
                self.emit_code(
                    format!(
                        "MOVQ {}, %rax",
                        self.registers.name(&left_register)
                    )
                    .as_str(),
                )?;
                self.emit_code(format!("CQO").as_str())?;
                self.emit_code(
                    format!("IDIV {}", self.registers.name(&right_register))
                        .as_str(),
                )?;
                self.emit_code(
                    format!(
                        "MOVQ %rax, {}",
                        self.registers.name(&result_register)
                    )
                    .as_str(),
                )?;
                self.registers.deallocate(left_register);
                self.registers.deallocate(right_register);
                Ok(result_register)
            }
            _ => todo!(),
        }
    }

    fn number(&mut self, value: f64) -> Result<RegisterIndex, BobaError> {
        let register = self.registers.allocate();
        self.emit_code(
            format!(
                "MOVQ ${}, {}",
                value as u64,
                self.registers.name(&register)
            )
            .as_str(),
        )?;
        Ok(register)
    }
}
