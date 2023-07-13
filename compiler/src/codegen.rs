use crate::error::BobaError;
use crate::expr::{BinaryOperand, Comparison, LLExpr, UnaryOperand};
use crate::stmt::LLStmt;
use crate::typecheck::{Kind, Type};
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
    pub code: String,   // code
    pub data: String,   // global variables
    pub global: String, // function names
}

impl Assembly {
    fn build(&self) -> String {
        format!("{}{}{}{}", self.global, self.header, self.code, self.data)
    }
}

pub struct CodeGen {
    global_buffer: Vec<Box<str>>, // function name
    registers: ScratchRegisters,
    assembly: Assembly,
    labels: Labels,
}

impl CodeGen {
    pub fn new() -> Self {
        Self {
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

    pub fn compile(&mut self, ast: &[LLStmt]) -> Result<String, BobaError> {
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

    fn codegen(&mut self, stmt: &LLStmt) -> Result<(), BobaError> {
        match stmt {
            LLStmt::LocalVariable {
                init,
                ty_pe,
                variable_index,
            } => self.local_variable_decl(init, ty_pe, variable_index),
            LLStmt::GlobalVariable { name, init } => {
                self.global_variable_decl(name, init)
            }
            LLStmt::Expression(expr) => {
                let register = self.expression(expr)?;
                self.registers.deallocate(register);
                Ok(())
            }
            LLStmt::Print { value, ty_pe } => self.print_stmt(value, ty_pe),
            LLStmt::If {
                condition,
                then,
                elze,
            } => self.if_stmt(condition, then, elze),
            LLStmt::Block(stmts) => self.block_stmt(stmts),
            LLStmt::Function {
                name,
                param_sizes,
                space_for_locals,
                body,
            } => self.function_decl(name, param_sizes, space_for_locals, body),
            LLStmt::Return { name, value } => self.return_stmt(name, value),
            LLStmt::While { condition, body } => {
                self.while_stmt(condition, body)
            }
        }
    }

    fn while_stmt(
        &mut self,
        condition: &LLExpr,
        body: &LLStmt,
    ) -> Result<(), BobaError> {
        let loop_begin = self.labels.create();
        let loop_end = self.labels.create();
        self.emit_label(loop_begin.clone())?;
        self.boolean_expression(condition, &loop_end)?;
        self.codegen(body)?;
        self.emit_code("jmp", &loop_begin, "")?;
        self.emit_label(loop_end)?;
        Ok(())
    }

    fn return_stmt(
        &mut self,
        name: &str,
        expr: &LLExpr,
    ) -> Result<(), BobaError> {
        let register = self.expression(expr)?;
        self.emit_code("movq", &register, "%rax")?;
        self.emit_code("jmp", format!(".{name}_epilogue"), "")?;
        self.registers.deallocate(register);
        Ok(())
    }

    fn local_variable_decl(
        &mut self,
        init: &LLExpr,
        ty_pe: &Type,
        index: &u16,
    ) -> Result<(), BobaError> {
        let register = self.expression(init)?;
        let size: u8 = ty_pe.into();
        let index = (index + 1) * size;
        self.emit_code("movq", &register, format!("-{index}(%rbp)"))?;
        self.registers.deallocate(register);
        Ok(())
    }

    fn add_global_name(&mut self, name: &str) {
        self.global_buffer.push(name.into());
    }

    fn function_decl(
        &mut self,
        name: &str,
        params_sizes: &[u16],
        space_for_locals: &u16,
        body: &LLStmt,
    ) -> Result<(), BobaError> {
        self.add_global_name(name);
        self.emit_label(name)?;
        self.emit_code("pushq", "%rbp", "")?;
        self.emit_code("movq", "%rsp", "%rbp")?;
        let argument_registers = ["%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"];
        for index in 0..params_count {
            if index < 6 {
                self.emit_code(
                    "pushq",
                    argument_registers[index as usize],
                    "",
                )?;
            } else {
                todo!("Can't handle functions with more than six parameters.");
            }
        }
        self.emit_code("subq", format!("${space_for_locals}"), "%rsp")?;
        let callee_saved_registers = ["%rbx", "%r12", "%r13", "%r14", "%r15"];
        for register in callee_saved_registers {
            self.emit_code("pushq", register, "")?;
        }
        self.codegen(body)?;
        self.emit_label(format!(".{name}_epilogue"))?;
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
        condition: &LLExpr,
        then: &LLStmt,
        elze: &Option<Box<LLStmt>>,
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
        condition: &LLExpr,
        false_label: &str,
    ) -> Result<(), BobaError> {
        if let LLExpr::Binary {
            left,
            oper: BinaryOperand::Compare(operator),
            right,
            ..
        } = condition
        {
            let left = self.expression(left)?;
            let right = self.expression(right)?;
            self.emit_code("cmp", &right, &left)?;
            self.comparison_operation(operator, false_label)?;
            self.registers.deallocate(left);
            self.registers.deallocate(right);
        } else {
            let result = self.expression(condition)?;
            self.emit_code("cmp", "$1", &result)?; // condition code = result - 1
            self.emit_code("js", false_label, "")?;
            self.registers.deallocate(result);
        }
        Ok(())
    }

    fn comparison_operation(
        &mut self,
        operation: &Comparison,
        false_label: &str,
    ) -> Result<(), BobaError> {
        match operation {
            Comparison::Less => self.emit_code("jnl", false_label, "")?,
            Comparison::LessEqual => self.emit_code("jnle", false_label, "")?,
            Comparison::Greater => self.emit_code("jng", false_label, "")?,
            Comparison::GreaterEqual => {
                self.emit_code("jnge", false_label, "")?
            }
            Comparison::EqualEqual => {
                self.emit_code("jne", false_label, "")?;
            }
        };
        Ok(())
    }

    fn block_stmt(&mut self, stmts: &[LLStmt]) -> Result<(), BobaError> {
        for stmt in stmts {
            self.codegen(stmt)?;
        }
        Ok(())
    }

    fn print_stmt(
        &mut self,
        value: &LLExpr,
        _ty_pe: &Type,
    ) -> Result<(), BobaError> {
        let register = self.expression(value)?;
        self.emit_code("andq", "$-16", "%rsp")?;
        self.emit_code("movq", &register, "%rsi")?;
        self.emit_code("leaq", ".LC0(%rip)", "%rax")?;
        self.emit_code("movq", "%rax", "%rdi")?;
        self.emit_code("xor", "%eax", "%eax")?;
        self.emit_code("call", "printf@PLT", "")?;
        self.registers.deallocate(register);
        Ok(())
    }

    fn emit_data(
        &mut self,
        symbol_name: &str,
        size: &str,
        value: impl std::fmt::Debug,
    ) -> Result<(), BobaError> {
        self.data_writer(symbol_name, size, value)
            .map_err(|e| e.into())
    }

    fn data_writer(
        &mut self,
        symbol_name: &str,
        size: &str,
        value: impl std::fmt::Debug,
    ) -> fmt::Result {
        writeln!(&mut self.assembly.data, "{symbol_name}:")?;
        writeln!(&mut self.assembly.data, "{:8}.{size} {value:?}", " ")
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
        name: &str,
        init: &LLExpr,
    ) -> Result<(), BobaError> {
        self.emit_data(name, "quad", init)
    }

    fn expression(
        &mut self,
        expr: &LLExpr,
    ) -> Result<RegisterIndex, BobaError> {
        match expr {
            LLExpr::Binary {
                left, oper, right, ..
            } => self.binary(left, oper, right),
            LLExpr::Number(num) => self.number(*num),
            LLExpr::Variable {
                name, ty_pe, kind, ..
            } => self.variable(name, ty_pe, kind),
            LLExpr::Boolean(value) => self.boolean(value),
            LLExpr::Call { callee, args, .. } => {
                self.function_call(callee, args)
            }
            LLExpr::Assign { value, index, .. } => {
                self.assignment(value, *index)
            }
            LLExpr::Unary { oper, right, .. } => self.unary(oper, right),
            LLExpr::Group { value, .. } => self.expression(expr),
            LLExpr::String(literal) => self.string(literal),
        }
    }

    fn emit_string(
        &mut self,
        label: &str,
        literal: &str,
    ) -> Result<(), BobaError> {
        writeln!(&mut self.assembly.header, "{label}:")?;
        writeln!(&mut self.assembly.header, "{:8}.string \"{literal}\"", " ")?;
        writeln!(&mut self.assembly.header, "{:8}.text", " ")?;
        Ok(())
    }

    fn string(&mut self, literal: &str) -> Result<RegisterIndex, BobaError> {
        let label = self.labels.create();
        self.emit_string(&label, literal)?;
        let register = self.registers.allocate();
        self.emit_code("leaq", format!("{label}(%rip)"), &register)?;
        Ok(register)
    }

    fn unary(
        &mut self,
        oper: &UnaryOperand,
        right: &LLExpr,
    ) -> Result<RegisterIndex, BobaError> {
        let operand = self.expression(right)?;
        match oper {
            UnaryOperand::LogicalNot => {
                // FIXME: Find a better way to implement logical not
                // using test and sete
                self.emit_code("not", &operand, "")?;
                self.emit_code("inc", &operand, "")?;
                self.emit_code("inc", &operand, "")?;
            }
            UnaryOperand::Negate => self.emit_code("neg", &operand, "")?,
        }
        Ok(operand)
    }

    fn assignment(
        &mut self,
        value: &LLExpr,
        index: u16,
    ) -> Result<RegisterIndex, BobaError> {
        let value = self.expression(value)?;
        // TODO: Fix size of variable
        let index = (index + 1) * 8;
        self.emit_code("movq", &value, format!("-{index}(%rbp)"))?;
        Ok(value)
    }

    fn function_call(
        &mut self,
        callee: &str,
        args: &[LLExpr],
    ) -> Result<RegisterIndex, BobaError> {
        self.emit_code("andq", "$-16", "%rsp")?;
        let arguments = args
            .iter()
            .map(|arg| self.expression(arg))
            .collect::<Result<Vec<RegisterIndex>, BobaError>>()?;
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
        Ok(result)
    }

    fn boolean(&mut self, value: &bool) -> Result<RegisterIndex, BobaError> {
        let number = u8::from(*value);
        let register = self.registers.allocate();
        self.emit_code("movq", format!("${number}"), &register)?;
        Ok(register)
    }

    fn variable(
        &mut self,
        name: &str,
        ty_pe: &Type,
        kind: &Kind,
    ) -> Result<RegisterIndex, BobaError> {
        let register = self.registers.allocate();
        if let Kind::Parameter(index) | Kind::LocalVariable(index) = kind {
            let size: u8 = ty_pe.into();
            let index = (index + 1) * size;
            self.emit_code("movq", format!("-{index}(%rbp)"), &register)?;
        } else {
            let symbol = format!("{name}(%rip)");
            self.emit_code("movq", symbol, &register)?;
        }
        Ok(register)
    }

    fn binary(
        &mut self,
        left: &LLExpr,
        oper: &BinaryOperand,
        right: &LLExpr,
    ) -> Result<RegisterIndex, BobaError> {
        let left_register = self.expression(left)?;
        let right_register = self.expression(right)?;
        match &oper {
            BinaryOperand::Add => {
                self.emit_code("addq", &left_register, &right_register)?;
                self.registers.deallocate(left_register);
                Ok(right_register)
            }
            BinaryOperand::Subtract => {
                self.emit_code("subq", &right_register, &left_register)?;
                self.registers.deallocate(right_register);
                Ok(left_register)
            }
            BinaryOperand::Multiply => {
                let result_register = self.registers.allocate();
                self.emit_code("movq", &right_register, "%rax")?;
                self.emit_code("imul", &left_register, "")?;
                self.emit_code("movq", "%rax", &result_register)?;
                self.registers.deallocate(left_register);
                self.registers.deallocate(right_register);
                Ok(result_register)
            }
            BinaryOperand::Divide => {
                let result_register = self.registers.allocate();
                self.emit_code("movq", &left_register, "%rax")?;
                self.emit_code("cqo", "", "")?;
                self.emit_code("idiv", &right_register, "")?;
                self.emit_code("movq", "%rax", &result_register)?;
                self.registers.deallocate(left_register);
                self.registers.deallocate(right_register);
                Ok(result_register)
            }
            BinaryOperand::Modulus => {
                let result_register = self.registers.allocate();
                self.emit_code("movq", &left_register, "%rax")?;
                self.emit_code("cqo", "", "")?;
                self.emit_code("idiv", &right_register, "")?;
                self.emit_code("movq", "%rdx", &result_register)?;
                self.registers.deallocate(left_register);
                self.registers.deallocate(right_register);
                Ok(result_register)
            }
            BinaryOperand::Compare(comparison_operand) => {
                let result = self.registers.allocate();
                let false_label = self.labels.create();
                let done_label = self.labels.create();
                self.emit_code("cmp", &right_register, &left_register)?;
                self.comparison_operation(comparison_operand, &false_label)?;
                self.emit_code("mov", "$1", &result)?;
                self.emit_code("jmp", &done_label, "")?;
                self.emit_label(false_label)?;
                self.emit_code("mov", "$0", &result)?;
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
