use crate::error::BobaError;
use crate::expr::Expr;
use crate::lexer::{Token, TokenType};
use crate::stmt::Stmt;
use std::fmt::Write;

#[derive(Default)]
struct Labels {
    count: u16,
}

impl Labels {
    fn new() -> Self {
        Labels::default()
    }

    fn create(&mut self) -> Box<str> {
        let label = format!(".L{}", self.count);
        self.count += 1;
        label.into()
    }
}

struct RegisterIndex(u8);

use std::fmt;
impl fmt::Display for RegisterIndex {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self.0 {
                0 => "%rbx",
                1 => "%r10",
                2 => "%r11",
                3 => "%r12",
                4 => "%r13",
                5 => "%r14",
                6 => "%r15",
                _ => unreachable!(),
            }
        )
    }
}

struct ScratchRegisters {
    table: [bool; 7],
}

impl ScratchRegisters {
    pub fn new() -> Self {
        Self { table: [false; 7] }
    }

    pub fn allocate(&mut self) -> RegisterIndex {
        for (index, in_use) in self.table.iter_mut().enumerate() {
            if !*in_use {
                *in_use = true;
                return RegisterIndex(index as u8);
            }
        }
        unreachable!()
    }

    pub fn deallocate(&mut self, index: RegisterIndex) {
        let register = self.table.get_mut(index.0 as usize).unwrap();
        *register = false;
    }
}

#[derive(Default, Clone)]
pub struct Assembly {
    pub header: String,
    pub code: String,
    pub data: String,
}

pub struct CodeGen {
    globals: Vec<Token>,
    registers: ScratchRegisters,
    assembly: Assembly,
    labels: Labels,
}

impl CodeGen {
    pub fn new() -> Self {
        Self {
            globals: Vec::new(),
            registers: ScratchRegisters::new(),
            assembly: Assembly {
                header: r#".globl main
.LC0:
        .string "%d\n"
"#
                .to_string(),
                code: "main:\n".to_string(),
                data: ".data\n".to_string(),
            },
            labels: Labels::new(),
        }
    }

    pub fn compile(&mut self, ast: &[Stmt]) -> Result<Assembly, BobaError> {
        for ele in ast {
            self.codegen(ele)?;
        }
        self.emit_code("ret", "", "")?;
        Ok(self.assembly.clone())
    }

    fn codegen(&mut self, stmt: &Stmt) -> Result<(), BobaError> {
        match stmt {
            Stmt::Let {
                name,
                init,
                ..
            } => self.let_stmt(name, init),
            Stmt::Expression(_expr) => todo!(),
            Stmt::Print(expr) => self.print_stmt(expr),
            Stmt::If {
                condition,
                then,
                elze,
            } => self.if_stmt(condition, then, elze),
            Stmt::Block(stmts) => self.block_stmt(stmts),
        }
    }

    fn if_stmt(
        &mut self,
        condition: &Expr,
        then: &[Stmt],
        elze: &Option<Vec<Stmt>>,
    ) -> Result<(), BobaError> {
        let false_label = self.labels.create();
        let done_label = self.labels.create();
        self.boolean_expression(condition, &false_label)?;
        self.block_stmt(then)?;
        self.emit_code("jmp", &done_label, "")?;
        self.emit_label(false_label)?;
        if let Some(else_body) = elze {
            self.block_stmt(else_body)?;
        }
        self.emit_label(done_label)?;
        Ok(())
    }

    fn boolean_expression(
        &mut self,
        condition: &Expr,
        false_label: &str,
    ) -> Result<(), BobaError> {
        let Expr::Binary { left, oper, right } = condition else {
            panic!("Expect a boolean expression.");
        };
        let left = self.expression(left)?;
        let right = self.expression(right)?;
        self.emit_code("cmp", right, left)?;
        match oper.kind {
            TokenType::Less => self.emit_code("jnl", false_label, "")?,
            TokenType::LessEqual => self.emit_code("jnle", false_label, "")?,
            TokenType::Greater => self.emit_code("jng", false_label, "")?,
            TokenType::GreaterEqual => {
                self.emit_code("jnge", false_label, "")?
            }
            _ => unreachable!(),
        }
        Ok(())
    }

    fn block_stmt(&mut self, stmts: &[Stmt]) -> Result<(), BobaError> {
        for stmt in stmts {
            self.codegen(stmt)?;
        }
        Ok(())
    }

    fn print_stmt(&mut self, expr: &Expr) -> Result<(), BobaError> {
        let register = self.expression(expr)?;
        self.emit_code("pushq", "%rbp", "")?;
        self.emit_code("movq", "%rsp", "%rbp")?;
        self.emit_code("movq", &register, "%rsi")?;
        self.emit_code("leaq", ".LC0(%rip)", "%rax")?;
        self.emit_code("movq", "%rax", "%rdi")?;
        self.emit_code("movl", "$0", "%eax")?;
        self.emit_code("call", "printf@PLT", "")?;
        self.emit_code("movl", "$0", "%eax")?;
        self.emit_code("popq", "%rbp", "")
    }

    fn emit_data<S: Into<String>>(
        &mut self,
        symbol_name: &str,
        size: &str,
        value: S,
    ) -> Result<(), BobaError> {
        self.data_writer(symbol_name, size, value)
            .map_err(|e| e.into())
    }

    fn data_writer<S: Into<String>>(
        &mut self,
        symbol_name: &str,
        size: &str,
        value: S,
    ) -> fmt::Result {
        writeln!(&mut self.assembly.data, "{symbol_name}:")?;
        writeln!(&mut self.assembly.data, "{:8}.{size} {}", " ", value.into())
    }

    fn code_writer(
        &mut self,
        instruction: &str,
        first_operand: impl fmt::Display,
        second_operand: impl fmt::Display,
    ) -> Result<(), std::fmt::Error> {
        write!(&mut self.assembly.code, "{:8}{instruction:6}", " ")?;
        let first_operand = first_operand.to_string();
        let second_operand = second_operand.to_string();
        if !first_operand.is_empty() && !second_operand.is_empty() {
            write!(
                &mut self.assembly.code,
                "{first_operand}, {second_operand}"
            )?;
        } else if !first_operand.is_empty() {
            write!(&mut self.assembly.code, "{first_operand}")?;
        }
        writeln!(&mut self.assembly.code)
    }

    fn emit_code(
        &mut self,
        instruction: &str,
        first_operand: impl fmt::Display,
        second_operand: impl fmt::Display,
    ) -> Result<(), BobaError> {
        self.code_writer(instruction, first_operand, second_operand)
            .map_err(|e| e.into())
    }

    fn emit_label<S: Into<String>>(
        &mut self,
        label: S,
    ) -> Result<(), BobaError> {
        writeln!(&mut self.assembly.code, "{}:", label.into())
            .map_err(|e| e.into())
    }

    fn let_stmt(
        &mut self,
        name: &Token,
        init: &Option<Expr>,
    ) -> Result<(), BobaError> {
        let symbol_name = name.to_string();
        if let Some(expr) = init {
            if let Some(size) = expr.get_size() {
                self.add_global(name);
                self.emit_data(&symbol_name, size.as_ref(), expr)
            } else {
                Err(BobaError::Compiler {
                    msg: "Global variables can be initialized with constants only"
                        .into(),
                    span: name.span,
                })
            }
        } else {
            Err(BobaError::Compiler {
                msg: "Global variables should be initated with constants"
                    .into(),
                span: name.span,
            })
        }
    }

    fn expression(&mut self, expr: &Expr) -> Result<RegisterIndex, BobaError> {
        match expr {
            Expr::Binary { left, oper, right } => {
                self.binary(left, oper, right)
            }
            Expr::Number(num) => self.number(*num),
            Expr::Variable(token) => self.variable(token),
            Expr::Boolean(value) => self.boolean(value),
            _ => todo!(),
        }
    }

    fn boolean(&mut self, value: &bool) -> Result<RegisterIndex, BobaError> {
        let number = u8::from(*value);
        let register = self.registers.allocate();
        self.emit_code("movq", format!("${number}").as_str(), &register)?;
        Ok(register)
    }

    fn symbol(&self, name: &Token) -> Result<String, BobaError> {
        if self.globals.contains(name) {
            Ok(format!("{name}(%rip)"))
        } else {
            Err(BobaError::Compiler {
                msg: format!("Undeclared variable {name}").into(),
                span: name.span,
            })
        }
    }

    fn add_global(&mut self, token: &Token) {
        self.globals.push(token.clone());
    }

    fn variable(&mut self, token: &Token) -> Result<RegisterIndex, BobaError> {
        let register = self.registers.allocate();
        self.emit_code("movq", &self.symbol(token)?, &register)?;
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
        match &oper.kind {
            TokenType::Plus => {
                self.emit_code("addq", &left_register, &right_register)?;
                self.registers.deallocate(left_register);
                Ok(right_register)
            }
            TokenType::Minus => {
                self.emit_code("subq", &right_register, &left_register)?;
                self.registers.deallocate(right_register);
                Ok(left_register)
            }
            TokenType::Star => {
                let result_register = self.registers.allocate();
                self.emit_code("movq", &right_register, "%rax")?;
                self.emit_code("imul", &left_register, "")?;
                self.emit_code("movq", "%rax", &result_register)?;
                self.registers.deallocate(left_register);
                self.registers.deallocate(right_register);
                Ok(result_register)
            }
            TokenType::Slash => {
                let result_register = self.registers.allocate();
                self.emit_code("movq", &left_register, "%rax")?;
                self.emit_code("cqo", "", "")?;
                self.emit_code("idiv", &right_register, "")?;
                self.emit_code("movq", "%rax", &result_register)?;
                self.registers.deallocate(left_register);
                self.registers.deallocate(right_register);
                Ok(result_register)
            }
            comparison_token => {
                let result = self.registers.allocate();
                let true_label = self.labels.create();
                let done_label = self.labels.create();
                self.emit_code("cmp", &right_register, &left_register)?;
                match comparison_token {
                    TokenType::Less => self.emit_code("jl", &true_label, "")?,
                    TokenType::LessEqual => {
                        self.emit_code("jle", &true_label, "")?
                    }
                    TokenType::Greater => {
                        self.emit_code("jg", &true_label, "")?
                    }
                    TokenType::GreaterEqual => {
                        self.emit_code("jge", &true_label, "")?
                    }
                    _ => unreachable!(),
                };
                self.emit_code("mov", "$0", &result)?;
                self.emit_code("jmp", &done_label, "")?;
                self.emit_label(true_label)?;
                self.emit_code("mov", "$1", &result)?;
                self.emit_label(done_label)?;
                Ok(result)
            }
        }
    }

    fn number(&mut self, value: i64) -> Result<RegisterIndex, BobaError> {
        let register = self.registers.allocate();
        self.emit_code(
            "movq",
            format!("${}", value as u64).as_str(),
            &register,
        )?;
        Ok(register)
    }
}
