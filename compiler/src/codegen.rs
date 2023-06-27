use crate::error::BobaError;
use crate::expr::Expr;
use crate::lexer::{Token, TokenType};
use crate::stmt::{Stmt, Parameter};
use std::fmt::Write;
use crate::analyzer::{Kind, Type};

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
    pub global: String,
}

impl Assembly {
    fn build(&self) -> String {
        format!("{}{}{}{}", self.global, self.header, self.code, self.data)
    }
}

pub struct CodeGen {
    data_buffer: Vec<Token>,
    global_buffer: Vec<Token>,
    registers: ScratchRegisters,
    assembly: Assembly,
    labels: Labels,
}

impl CodeGen {
    pub fn new() -> Self {
        Self {
            data_buffer: Vec::new(),
            global_buffer: Vec::new(),
            registers: ScratchRegisters::new(),
            assembly: Assembly {
                global: String::new(),
                header: r#".LC0:
        .string "%d\n"
"#
                .to_string(),
                code: String::new(),
                data: ".data\n".to_string(),
            },
            labels: Labels::new(),
        }
    }

    pub fn compile(&mut self, ast: &[Stmt]) -> Result<String, BobaError> {
        for ele in ast {
            self.codegen(ele)?;
        }
        self.build_assembly()
    }

    fn build_assembly(&mut self) -> Result<String, BobaError> {
        self.build_globals()?;
        Ok(self.assembly.build())
    }

    fn build_globals(&mut self) -> Result<(), BobaError> {
        let mut globals = self.global_buffer.iter();
        if let Some(name) = globals.next() {
            write!(&mut self.assembly.global, ".globl {name}")?;
        }
        for other_names in globals {
            write!(&mut self.assembly.global, ", {other_names}")?;
        }
        writeln!(&mut self.assembly.global)?;
        Ok(())
    }

    fn codegen(&mut self, stmt: &Stmt) -> Result<(), BobaError> {
        match stmt {
            Stmt::LocalVariable { name, init, ty_pe, kind, .. } => self.local_variable_decl(name, init, ty_pe, kind),
            Stmt::GlobalVariable { name, init } => self.global_variable_decl(name, init),
            Stmt::Expression(expr) => {
                let register = self.expression(expr)?;
                self.registers.deallocate(register);
                Ok(())
            }
            Stmt::Print(expr) => self.print_stmt(expr),
            Stmt::If {
                condition,
                then,
                elze,
            } => self.if_stmt(condition, then, elze),
            Stmt::Block(stmts) => self.block_stmt(stmts),
            Stmt::Function {
                name,
                params,
                return_type,
                body,
            } => {
                self.function_decl(name, params, return_type, body)
            }
        }
    }

    fn local_variable_decl(&mut self, name: &Token, init: &Expr, ty_pe: &Option<Type>, kind: &Option<Kind>) -> Result<(), BobaError> {
        let register = self.expression(init)?;
        if let Some(Kind::LocalVariable(index)) = kind {
            let index = (index + 1) * 8;
            self.emit_code("movq", &register, format!("-{index}(%rbp)"))?;
        };
        self.registers.deallocate(register);
        Ok(())
    }

    fn add_global_name(&mut self, name: &Token) {
        self.global_buffer.push(name.clone());
    }

    fn function_decl(
        &mut self,
        name: &Token,
        params: &[Parameter],
        _return_type: &Token,
        body: &Stmt,
    ) -> Result<(), BobaError> {
        self.add_global_name(name);
        self.emit_label(name.to_string())?;
        self.emit_code("pushq", "%rbp", "")?;
        self.emit_code("movq", "%rsp", "%rbp")?;
        let argument_registers = ["%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"];
        for (index, _) in params.iter().enumerate() {
            if index < 6 {
                self.emit_code("pushq", argument_registers[index], "")?;
            } else {
                todo!("Can't handle functions with more than six parameters.");
            }
        }
        let space_for_locals = 16;
        self.emit_code("subq", format!("${space_for_locals}"), "%rsp")?;
        let callee_saved_registers = ["%rbx", "%r12", "%r13", "%r14", "%r15"];
        for register in callee_saved_registers {
            self.emit_code("pushq", register, "")?;
        }
        self.codegen(body)?;
        for register in callee_saved_registers.iter().rev() {
            self.emit_code("popq", register, "")?;
        }
        self.emit_code("movq", "%rbp", "%rsp")?;
        self.emit_code("popq", "%rbp", "")?;
        self.emit_code("ret", "", "")?;
        Ok(())
    }

    fn if_stmt(
        &mut self,
        condition: &Expr,
        then: &Stmt,
        elze: &Option<Box<Stmt>>,
    ) -> Result<(), BobaError> {
        let false_label = self.labels.create();
        let done_label = self.labels.create();
        self.boolean_expression(condition, &false_label)?;
        self.codegen(then)?;
        self.emit_code("jmp", &done_label, "")?;
        self.emit_label(false_label)?;
        if let Some(else_body) = elze {
            self.codegen(else_body)?;
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
        self.emit_code("cmp", &right, &left)?;
        match oper.kind {
            TokenType::Less => self.emit_code("jnl", false_label, "")?,
            TokenType::LessEqual => self.emit_code("jnle", false_label, "")?,
            TokenType::Greater => self.emit_code("jng", false_label, "")?,
            TokenType::GreaterEqual => {
                self.emit_code("jnge", false_label, "")?
            }
            _ => unreachable!(),
        }
        self.registers.deallocate(left);
        self.registers.deallocate(right);
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
        let dummy = self.registers.allocate();
        self.emit_code("pushq", &dummy, "")?;
        self.emit_code("movq", &register, "%rsi")?;
        self.emit_code("leaq", ".LC0(%rip)", "%rax")?;
        self.emit_code("movq", "%rax", "%rdi")?;
        self.emit_code("xor", "%eax", "%eax")?;
        self.emit_code("call", "printf@PLT", "")?;
        self.emit_code("popq", &dummy, "")?;
        self.registers.deallocate(dummy);
        self.registers.deallocate(register);
        Ok(())
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

    fn global_variable_decl(
        &mut self,
        name: &Token,
        init: &Expr,
    ) -> Result<(), BobaError> {
        self.add_data(name); // add_data() and emit_data() should be combined into one function
        self.emit_data(&name.to_string(), "quad", init)
    }

    fn expression(&mut self, expr: &Expr) -> Result<RegisterIndex, BobaError> {
        match expr {
            Expr::Binary { left, oper, right } => {
                self.binary(left, oper, right)
            }
            Expr::Number(num) => self.number(*num),
            Expr::Variable{ name, ty_pe, kind  } => self.variable(name, ty_pe, kind),
            Expr::Boolean(value) => self.boolean(value),
            Expr::Call {
                callee,
                args,
            } => self.function_call(callee, args),
            _ => todo!("{expr}"),
        }
    }

    fn function_call(
        &mut self,
        callee: &Token,
        args: &[Expr],
    ) -> Result<RegisterIndex, BobaError> {
        let dummy = self.registers.allocate();
        self.emit_code("pushq", &dummy, "")?;
        let mut arguments = vec![];
        for arg in args {
            arguments.push(self.expression(arg)?);
        }
        let registers = ["%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"];
        for (value, register) in arguments.iter().zip(registers.iter()) {
            self.emit_code("movq", value, register)?;
        }
        self.emit_code("pushq", "%r10", "")?;
        self.emit_code("pushq", "%r11", "")?;
        self.emit_code("call", callee, "")?;
        self.emit_code("popq", "%r11", "")?;
        self.emit_code("popq", "%r10", "")?;
        let result = self.registers.allocate();
        self.emit_code("movq", "%rax", &result)?;
        self.emit_code("popq", &dummy, "")?;
        self.registers.deallocate(dummy);
        Ok(result)
    }

    fn boolean(&mut self, value: &bool) -> Result<RegisterIndex, BobaError> {
        let number = u8::from(*value);
        let register = self.registers.allocate();
        self.emit_code("movq", format!("${number}").as_str(), &register)?;
        Ok(register)
    }

    fn symbol(&self, name: &Token) -> Box<str> {
        format!("{name}(%rip)").into()
    }

    fn add_data(&mut self, token: &Token) {
        self.data_buffer.push(token.clone());
    }

    fn variable(&mut self, name: &Token, ty_pe: &Option<Type>, kind: &Option<Kind>) -> Result<RegisterIndex, BobaError> {
        let register = self.registers.allocate();
        if let Some(Kind::Parameter(index)) | Some(Kind::LocalVariable(index)) = kind {
            let index = (index + 1) * 8;
            self.emit_code("movq", format!("-{index}(%rbp)"), &register)?;
        } else {
            self.emit_code("movq", self.symbol(name), &register)?;
        }
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
