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

#[derive(Copy, Clone)]
enum RegisterSize {
    Byte,
    DWord,
    QWord,
}

impl From<u16> for RegisterSize {
    fn from(value: u16) -> RegisterSize {
        match value {
            1 => RegisterSize::Byte,
            4 => RegisterSize::DWord,
            8 => RegisterSize::QWord,
            _ => unreachable!(),
        }
    }
}

#[derive(Copy, Clone)]
struct RegisterIndex {
    value: u8,
    size: RegisterSize,
}

// Byte:  [bl,  r10b, r11b, r12b, r13b, r14b, r15b]
// DWord: [ebx, r10d, r11d, r12d, r13d, r14d, r15d]
// QWord: [rbx, r10,  r11,  r12,  r13,  r14,  r15]
const REGISTERS: [[&str; 7]; 3] = [
    ["%bl", "%r10b", "%r11b", "%r12b", "%r13b", "%r14b", "%r15b"],
    ["%ebx", "%r10d", "%r11d", "%r12d", "%r13d", "%r14d", "%r15d"],
    ["%rbx", "%r10", "%r11", "%r12", "%r13", "%r14", "%r15"],
];

const ARGUMENTS: [[&str; 6]; 3] = [
    ["%di", "%sil", "%dl", "%cl", "%r8b", "%r9b"],
    ["%edi", "%esi", "%edx", "%ecx", "%r8d", "%r9d"],
    ["%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"],
];

use std::fmt;
impl fmt::Display for RegisterIndex {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", REGISTERS[self.size as usize][self.value as usize])
    }
}

struct ScratchRegisters {
    table: [bool; 7],
}

impl ScratchRegisters {
    pub fn new() -> Self {
        Self { table: [false; 7] }
    }

    pub fn allocate(&mut self, ty_pe: &Type) -> RegisterIndex {
        for (index, in_use) in self.table.iter_mut().enumerate() {
            if !*in_use {
                *in_use = true;
                return RegisterIndex {
                    value: index as u8,
                    size: RegisterSize::from(ty_pe.as_size()),
                };
            }
        }
        unreachable!()
    }

    pub fn deallocate(&mut self, index: RegisterIndex) {
        let register = self.table.get_mut(index.value as usize).unwrap();
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
                header: r#".format_string:
        .string "%s\n"
.format_number:
        .string "%d\n"
.format_true:
        .string "true\n"
.format_false:
        .string "false\n"
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
                param_types,
                space_for_locals,
                body,
            } => self.function_decl(name, param_types, space_for_locals, body),
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
        let return_type = expr.to_type();
        let register = self.expression(expr)?;
        let mov = self.move_for(&return_type);
        let rax = self.rax_for(&return_type).to_string();
        self.emit_code(mov, &register, rax)?;
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
        let mov= self.move_for(ty_pe);
        self.emit_code(mov, &register, format!("-{index}(%rbp)"))?;
        self.registers.deallocate(register);
        Ok(())
    }

    fn add_global_name(&mut self, name: &str) {
        self.global_buffer.push(name.into());
    }

    fn function_decl(
        &mut self,
        name: &str,
        param_types: &[Type],
        space_for_locals: &u16,
        body: &LLStmt,
    ) -> Result<(), BobaError> {
        self.add_global_name(name);
        self.emit_label(name)?;
        self.emit_code("pushq", "%rbp", "")?;
        self.emit_code("movq", "%rsp", "%rbp")?;
        let mut size_sum = 0;
        for index in 0..param_types.len() {
            let param_type = param_types[index];
            let register_size = RegisterSize::from(param_type.as_size());
            if index < 6 {
                let mov = self.move_for(&param_type);
                size_sum += param_type.as_size();
                self.emit_code(
                    mov,
                    ARGUMENTS[register_size as usize][index],
                    format!("-{}(%rbp)", size_sum),
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
        ty_pe: &Type,
    ) -> Result<(), BobaError> {
        let register = self.expression(value)?;
        self.emit_code("andq", "$-16", "%rsp")?;
        // self.emit_code("movq", &register, "%rsi")?;
        self.emit_code(self.move_for(ty_pe), &register, self.rsi_for(ty_pe))?;
        self.emit_code("leaq", self.format_string(ty_pe), "%rax")?;
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
        instruction: impl fmt::Display,
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
        instruction: impl fmt::Display,
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
                left,
                oper,
                right,
                ty_pe,
            } => self.binary(left, oper, right, ty_pe),
            LLExpr::Number(num) => self.number(*num),
            LLExpr::Variable {
                name, ty_pe, kind, ..
            } => self.variable(name, ty_pe, kind),
            LLExpr::Boolean(value) => self.boolean(value),
            LLExpr::Call { callee, args, ty_pe } => {
                self.function_call(callee, args, ty_pe)
            }
            LLExpr::Assign { value, index, ty_pe } => {
                self.assignment(value, *index, ty_pe)
            }
            LLExpr::Unary { oper, right, .. } => self.unary(oper, right),
            LLExpr::Group { value, .. } => self.expression(value),
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
        let register = self.registers.allocate(&Type::String);
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
        ty_pe: &Type,
    ) -> Result<RegisterIndex, BobaError> {
        let value = self.expression(value)?;
        let mov = self.move_for(ty_pe);
        self.emit_code(mov, &value, format!("-{index}(%rbp)"))?;
        Ok(value)
    }

    fn function_call(
        &mut self,
        callee: &str,
        args: &[LLExpr],
        ty_pe: &Type,
    ) -> Result<RegisterIndex, BobaError> {
        self.emit_code("andq", "$-16", "%rsp")?;
        let arguments = args
            .iter()
            .map(|arg| self.expression(arg))
            .collect::<Result<Vec<RegisterIndex>, BobaError>>()?;
        let arg_types: Vec<Type> = args.iter().map(|arg| arg.to_type()).collect();
        for index in 0..arg_types.len() {
            let ty = arg_types[index];
            let register_index = RegisterSize::from(ty.as_size()) as usize;
            let mov = self.move_for(&ty);
            self.emit_code(mov, arguments[index], ARGUMENTS[register_index][index])?;
        }
        self.emit_code("pushq", "%r10", "")?;
        self.emit_code("pushq", "%r11", "")?;
        self.emit_code("call", callee, "")?;
        self.emit_code("popq", "%r11", "")?;
        self.emit_code("popq", "%r10", "")?;
        let result = self.registers.allocate(ty_pe);
        let mov = self.move_for(ty_pe);
        let rax = self.rax_for(ty_pe);
        self.emit_code(mov, rax, result)?;
        Ok(result)
    }

    fn boolean(&mut self, value: &bool) -> Result<RegisterIndex, BobaError> {
        let number = u8::from(*value);
        let register = self.registers.allocate(&Type::Bool);
        self.emit_code("movb", format!("${number}"), &register)?;
        Ok(register)
    }

    fn move_for(&self, ty_pe: &Type) -> String {
        match ty_pe.as_size() {
            1 => String::from("movb"),
            4 => String::from("movl"),
            8 => String::from("movq"),
            _ => unreachable!(),
        }
    }

    fn rax_for(&self, ty_pe: &Type) -> String {
        match ty_pe.as_size() {
            1 => String::from("%al"),
            4 => String::from("%eax"),
            8 => String::from("%rax"),
            _ => unreachable!(),
        }
    }

    fn rsi_for(&self, ty_pe: &Type) -> String {
        let lexeme = match ty_pe.as_size() {
            1 => ARGUMENTS[RegisterSize::Byte as usize][1],
            4 => ARGUMENTS[RegisterSize::DWord as usize][1],
            8 => ARGUMENTS[RegisterSize::QWord as usize][1],
            _ => unreachable!(),
        };
        lexeme.to_string()
    }

    fn format_string(&self, ty_pe: &Type) -> String {
        match ty_pe {
            Type::String => String::from(".format_string(%rip)"),
            Type::Number => String::from(".format_number(%rip)"),
            Type::Bool => todo!(),
            _ => unreachable!(),
        }
    }

    fn variable(
        &mut self,
        name: &str,
        ty_pe: &Type,
        kind: &Kind,
    ) -> Result<RegisterIndex, BobaError> {
        let register = self.registers.allocate(ty_pe);
        if let Kind::Parameter(index) | Kind::LocalVariable(index) = kind {
            self.emit_code(
                self.move_for(ty_pe),
                format!("-{index}(%rbp)"),
                &register,
            )?;
        } else {
            let symbol = format!("{name}(%rip)");
            self.emit_code(self.move_for(ty_pe), symbol, &register)?;
        }
        Ok(register)
    }

    fn binary(
        &mut self,
        left: &LLExpr,
        oper: &BinaryOperand,
        right: &LLExpr,
        ty_pe: &Type,
    ) -> Result<RegisterIndex, BobaError> {
        let left_register = self.expression(left)?;
        let right_register = self.expression(right)?;
        match &oper {
            BinaryOperand::Add => {
                self.emit_code("addl", &left_register, &right_register)?;
                self.registers.deallocate(left_register);
                Ok(right_register)
            }
            BinaryOperand::Subtract => {
                self.emit_code("subl", &right_register, &left_register)?;
                self.registers.deallocate(right_register);
                Ok(left_register)
            }
            BinaryOperand::Multiply => {
                let result_register = self.registers.allocate(ty_pe);
                self.emit_code("movl", &right_register, "%eax")?;
                self.emit_code("imull", &left_register, "")?;
                self.emit_code("movl", "%eax", &result_register)?;
                self.registers.deallocate(left_register);
                self.registers.deallocate(right_register);
                Ok(result_register)
            }
            BinaryOperand::Divide => {
                let result_register = self.registers.allocate(ty_pe);
                self.emit_code("movl", &left_register, "%eax")?;
                self.emit_code("cqo", "", "")?;
                self.emit_code("idivl", &right_register, "")?;
                self.emit_code("movl", "%eax", &result_register)?;
                self.registers.deallocate(left_register);
                self.registers.deallocate(right_register);
                Ok(result_register)
            }
            BinaryOperand::Modulus => {
                let result_register = self.registers.allocate(ty_pe);
                self.emit_code("movl", &left_register, "%eax")?;
                self.emit_code("cqo", "", "")?;
                self.emit_code("idivl", &right_register, "")?;
                self.emit_code("movl", "%edx", &result_register)?;
                self.registers.deallocate(left_register);
                self.registers.deallocate(right_register);
                Ok(result_register)
            }
            BinaryOperand::Compare(comparison_operand) => {
                let result = self.registers.allocate(ty_pe);
                let false_label = self.labels.create();
                let done_label = self.labels.create();
                self.emit_code("cmpl", &right_register, &left_register)?;
                self.comparison_operation(comparison_operand, &false_label)?;
                self.emit_code("movl", "$1", &result)?;
                self.emit_code("jmp", &done_label, "")?;
                self.emit_label(false_label)?;
                self.emit_code("movl", "$0", &result)?;
                self.emit_label(done_label)?;
                Ok(result)
            }
        }
    }

    fn number(&mut self, value: i64) -> Result<RegisterIndex, BobaError> {
        let register = self.registers.allocate(&Type::Number);
        self.emit_code(
            "movl",
            format!("${}", value as u64).as_str(),
            &register,
        )?;
        Ok(register)
    }
}
