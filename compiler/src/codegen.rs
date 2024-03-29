use crate::expr::{BinaryOperand, Comparison, LLExpr, Logical, UnaryOperand};
use crate::stmt::LLStmt;
use crate::typecheck::{Kind, Type};
use std::fmt::Write;

#[derive(Default)]
struct Labels {
    count: u16,
}

#[derive(Clone)]
struct Label(Box<str>);

impl From<String> for Label {
    fn from(value: String) -> Label {
        Label(value.into())
    }
}

impl From<&str> for Label {
    fn from(value: &str) -> Label {
        Label(value.into())
    }
}

impl From<&Label> for Label {
    fn from(value: &Label) -> Label {
        value.clone()
    }
}

impl fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Labels {
    fn new() -> Self {
        Labels::default()
    }

    fn create(&mut self) -> Label {
        let label = format!(".L{}", self.count);
        self.count += 1;
        Label(label.into())
    }
}

#[derive(Copy, Clone)]
enum RegisterSize {
    Byte,
    DWord,
    QWord,
}

impl From<&Type> for RegisterSize {
    fn from(value: &Type) -> RegisterSize {
        match value {
            Type::Bool | Type::Char => RegisterSize::Byte,
            Type::Number => RegisterSize::DWord,
            Type::String | Type::Array { .. } => RegisterSize::QWord,
            Type::Unit => unreachable!(),
        }
    }
}

struct RegisterIndex {
    value: u8,
    size: RegisterSize,
}

impl RegisterIndex {
    fn size(mut self, size: RegisterSize) -> Self {
        self.size = size;
        self
    }
}

fn move_for(ty_pe: &Type) -> String {
    match RegisterSize::from(ty_pe) {
        RegisterSize::Byte => String::from("movb"),
        RegisterSize::DWord => String::from("movl"),
        RegisterSize::QWord => String::from("movq"),
    }
}

fn rax_for(ty_pe: &Type) -> String {
    match RegisterSize::from(ty_pe) {
        RegisterSize::Byte => String::from("%al"),
        RegisterSize::DWord => String::from("%eax"),
        RegisterSize::QWord => String::from("%rax"),
    }
}

fn cmp_for(ty_pe: &Type) -> String {
    match RegisterSize::from(ty_pe) {
        RegisterSize::Byte => String::from("cmpb"),
        RegisterSize::DWord => String::from("cmpl"),
        RegisterSize::QWord => unreachable!(
            "Cannot compare these types. Typechecker should reject it."
        ),
    }
}

use std::fmt;
impl fmt::Display for RegisterIndex {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // Byte:  [bl,  r10b, r11b, r12b, r13b, r14b, r15b]
        // DWord: [ebx, r10d, r11d, r12d, r13d, r14d, r15d]
        // QWord: [rbx, r10,  r11,  r12,  r13,  r14,  r15]
        static REGISTERS: [[&str; 7]; 3] = [
            ["%bl", "%r10b", "%r11b", "%r12b", "%r13b", "%r14b", "%r15b"],
            ["%ebx", "%r10d", "%r11d", "%r12d", "%r13d", "%r14d", "%r15d"],
            ["%rbx", "%r10", "%r11", "%r12", "%r13", "%r14", "%r15"],
        ];
        write!(f, "{}", REGISTERS[self.size as usize][self.value as usize])
    }
}

struct ScratchRegisters {
    table: [bool; 7],
}

impl ScratchRegisters {
    fn new() -> Self {
        Self { table: [false; 7] }
    }

    fn allocate(&mut self, ty_pe: &Type) -> RegisterIndex {
        for (index, in_use) in self.table.iter_mut().enumerate() {
            if !*in_use {
                *in_use = true;
                return RegisterIndex {
                    value: index as u8,
                    size: RegisterSize::from(ty_pe),
                };
            }
        }
        unreachable!("Cannot allocate scratch register.")
    }

    fn deallocate(&mut self, index: RegisterIndex) {
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

impl fmt::Display for Assembly {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}{}{}{}", self.global, self.header, self.code, self.data)
    }
}

pub struct CodeGen {
    global_buffer: Vec<Box<str>>, // function name
    registers: ScratchRegisters,
    assembly: Assembly,
    labels: Labels,
    argument_registers: [[&'static str; 6]; 3],
}

impl CodeGen {
    pub fn new() -> Self {
        Self {
            global_buffer: Vec::new(),
            registers: ScratchRegisters::new(),
            assembly: Assembly {
                global: String::new(),
                header: r#".format_true:
        .string "true"
.format_false:
        .string "false"
"#
                .to_string(),
                code: String::new(),
                data: ".data\n".to_string(),
            },
            labels: Labels::new(),
            argument_registers: [
                ["%dil", "%sil", "%dl", "%cl", "%r8b", "%r9b"],
                ["%edi", "%esi", "%edx", "%ecx", "%r8d", "%r9d"],
                ["%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"],
            ],
        }
    }

    pub fn compile(&mut self, ast: &[LLStmt]) -> Assembly {
        for ele in ast {
            self.codegen(ele);
        }
        self.build_assembly()
    }

    fn build_assembly(&mut self) -> Assembly {
        self.build_globals();
        self.assembly.clone()
    }

    fn build_globals(&mut self) {
        let mut globals = self.global_buffer.iter();
        if let Some(name) = globals.next() {
            write!(&mut self.assembly.global, ".globl {name}")
                .expect("Unable to write global name.");
        }
        for other_names in globals {
            write!(&mut self.assembly.global, ", {other_names}")
                .expect("Unable to write global name.");
        }
        writeln!(&mut self.assembly.global)
            .expect("Unable to write into global.");
    }

    fn codegen(&mut self, stmt: &LLStmt) {
        match stmt {
            LLStmt::LocalVariable {
                init,
                ty_pe,
                variable_index,
            } => self.local_variable_decl(init, ty_pe, *variable_index),
            LLStmt::GlobalVariable { name, init } => {
                self.global_variable_decl(name, init)
            }
            LLStmt::Expression(expr) => {
                let register = self.expression(expr);
                self.registers.deallocate(register);
            }
            LLStmt::Print {
                format,
                args,
                arg_count,
            } => self.print_stmt(format, args, *arg_count),
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
                return_type,
            } => self.function_decl(
                name,
                param_types,
                *space_for_locals,
                body,
                return_type,
            ),
            LLStmt::Return { name, value } => self.return_stmt(name, value),
            LLStmt::While { condition, body } => {
                self.while_stmt(condition, body)
            }
            LLStmt::Error => unreachable!(),
        }
    }

    fn while_stmt(&mut self, condition: &LLExpr, body: &LLStmt) {
        let loop_begin = self.labels.create();
        let loop_end = self.labels.create();
        let true_branch = self.labels.create();
        self.emit_label(&loop_begin);
        self.boolean_expression(condition, &true_branch, &loop_end);
        self.emit_label(true_branch);
        self.codegen(body);
        self.emit_code("jmp", loop_begin, "");
        self.emit_label(loop_end);
    }

    fn return_stmt(&mut self, name: &str, expr: &LLExpr) {
        let return_type = expr.to_type();
        let register = self.expression(expr);
        self.emit_code(
            move_for(&return_type),
            &register,
            rax_for(&return_type),
        );
        self.emit_code("jmp", format!(".{name}_epilogue"), "");
        self.registers.deallocate(register);
    }

    fn local_variable_decl(
        &mut self,
        init: &LLExpr,
        ty_pe: &Type,
        index: usize,
    ) {
        let register = self.expression(init);
        self.emit_code(move_for(ty_pe), &register, format!("-{index}(%rbp)"));
        self.registers.deallocate(register);
    }

    fn add_global_name(&mut self, name: &str) {
        self.global_buffer.push(name.into());
    }

    fn function_prologue(
        &mut self,
        name: &str,
        callee_saved_registers: &[&str; 5],
        space_for_locals: usize,
        param_types: &[Type],
    ) {
        self.add_global_name(name);
        self.emit_label(name);
        self.emit_code("pushq", "%rbp", "");
        self.emit_code("movq", "%rsp", "%rbp");
        let locals_space = format!("${}", space_for_locals);
        self.emit_code("subq", &locals_space, "%rsp");
        let mut size_sum = 0;
        for (index, param_type) in param_types.iter().enumerate() {
            let register_size = RegisterSize::from(param_type);
            // This is a temporary hack. Fix it once pointers are implemented
            size_sum += if param_type.is_array() {
                8
            } else {
                param_type.as_size()
            };
            self.emit_code(
                move_for(param_type),
                self.argument_registers[register_size as usize][index],
                format!("-{size_sum}(%rbp)"),
            );
        }
        for register in callee_saved_registers {
            self.emit_code("pushq", register, "");
        }
    }

    fn function_decl(
        &mut self,
        name: &str,
        param_types: &[Type],
        space_for_locals: usize,
        body: &LLStmt,
        return_type: &Type,
    ) {
        let callee_saved_registers = ["%rbx", "%r12", "%r13", "%r14", "%r15"];
        self.function_prologue(
            name,
            &callee_saved_registers,
            space_for_locals,
            param_types,
        );
        self.codegen(body);
        if name == "main" && return_type == &Type::Unit {
            self.emit_code("movl", "$0", "%eax");
        }
        self.function_epilogue(name, &callee_saved_registers);
    }

    fn function_epilogue(
        &mut self,
        name: &str,
        callee_saved_registers: &[&str; 5],
    ) {
        self.emit_label(format!(".{name}_epilogue"));
        for register in callee_saved_registers.iter().rev() {
            self.emit_code("popq", register, "");
        }
        self.emit_code("movq", "%rbp", "%rsp");
        self.emit_code("popq", "%rbp", "");
        self.emit_code("ret", "", "");
    }

    fn if_stmt(
        &mut self,
        condition: &LLExpr,
        then: &LLStmt,
        elze: &Option<Box<LLStmt>>,
    ) {
        let true_label = self.labels.create();
        let false_label = self.labels.create();
        let done_label = self.labels.create();
        self.boolean_expression(condition, &true_label, &false_label);
        self.emit_label(true_label);
        self.codegen(then);
        self.emit_code("jmp", &done_label, "");
        self.emit_label(false_label);
        if let Some(else_body) = elze {
            self.codegen(else_body);
        }
        self.emit_label(done_label);
    }

    fn boolean_expression(
        &mut self,
        condition: &LLExpr,
        true_label: &Label,
        false_label: &Label,
    ) {
        match condition {
            LLExpr::Binary {
                left,
                right,
                oper: BinaryOperand::Compare(operator),
                ..
            } => {
                let lhs = self.expression(left);
                let rhs = self.expression(right);
                self.emit_code(cmp_for(&left.to_type()), &rhs, &lhs);
                self.comparison_operation(operator, false_label);
                self.registers.deallocate(lhs);
                self.registers.deallocate(rhs);
            }
            LLExpr::Binary {
                left,
                right,
                oper: BinaryOperand::Logic(operator),
                ..
            } => {
                let lhs = self.expression(left);
                let rhs = self.expression(right);
                match operator {
                    Logical::Or => {
                        self.emit_code("cmpb", "$0", &lhs);
                        self.emit_code("jne", true_label, "");
                        self.emit_code("cmpb", "$0", &rhs);
                        self.emit_code("je", false_label, "");
                    }
                    Logical::And => {
                        self.emit_code("cmpb", "$0", &lhs);
                        self.emit_code("je", false_label, "");
                        self.emit_code("cmpb", "$0", &rhs);
                        self.emit_code("je", false_label, "");
                    }
                }
                self.registers.deallocate(lhs);
                self.registers.deallocate(rhs);
            }
            _ => {
                let result = self.expression(condition);
                self.emit_code(cmp_for(&condition.to_type()), "$1", &result); // condition code = result - 1
                self.emit_code("js", false_label, "");
                self.registers.deallocate(result);
            }
        }
    }

    fn comparison_operation(
        &mut self,
        operation: &Comparison,
        false_label: &Label,
    ) {
        match operation {
            Comparison::Less => self.emit_code("jnl", false_label, ""),
            Comparison::LessEqual => self.emit_code("jnle", false_label, ""),
            Comparison::Greater => self.emit_code("jng", false_label, ""),
            Comparison::GreaterEqual => self.emit_code("jnge", false_label, ""),
            Comparison::EqualEqual => self.emit_code("jne", false_label, ""),
            Comparison::BangEqual => self.emit_code("je", false_label, ""),
        };
    }

    fn block_stmt(&mut self, stmts: &[LLStmt]) {
        for stmt in stmts {
            self.codegen(stmt);
        }
    }

    fn print_boolean(&mut self, register: &str, result: &str) {
        let false_label = self.labels.create();
        let done_label = self.labels.create();
        self.emit_code("cmpb", "$1", register);
        self.emit_code("jne", &false_label, "");
        self.emit_code("leaq", ".format_true(%rip)", result);
        self.emit_code("jmp", &done_label, "");
        self.emit_label(false_label);
        self.emit_code("leaq", ".format_false(%rip)", result);
        self.emit_label(done_label);
    }

    fn push_arg_to_stack(&mut self, kind: &Type, register: &str) {
        match kind {
            Type::Char => {
                let result = self.registers.allocate(&Type::Char);
                self.emit_code("movb", register, &result);
                let result = result.size(RegisterSize::QWord);
                self.emit_code("pushq", &result, "");
                self.registers.deallocate(result);
            }
            Type::Number => {
                let result = self.registers.allocate(&Type::Number);
                self.emit_code("movl", register, &result);
                let result = result.size(RegisterSize::QWord);
                self.emit_code("pushq", &result, "");
                self.registers.deallocate(result);
            }
            Type::String | Type::Array { .. } => {
                self.emit_code("pushq", register, "");
            }
            Type::Bool => {
                let result = self.registers.allocate(&Type::String);
                self.print_boolean(register, &result.to_string());
                self.emit_code("pushq", &result, "");
                self.registers.deallocate(result);
            }
            Type::Unit => unreachable!(),
        }
    }

    fn place_arg_at_register(
        &mut self,
        kind: &Type,
        register_index: usize,
        register: &str,
    ) {
        let size_index = if kind == &Type::Bool {
            RegisterSize::from(&Type::String)
        } else {
            RegisterSize::from(kind)
        } as usize;
        let result = self.argument_registers[size_index][register_index];
        match kind {
            Type::Char | Type::Number | Type::String | Type::Array { .. } => {
                self.emit_code(move_for(kind), register, result);
            }
            Type::Bool => {
                self.print_boolean(register, result);
            }
            Type::Unit => unreachable!(),
        }
    }

    fn store_argument(
        &mut self,
        arg_index: usize,
        register: &str,
        kind: &Type,
    ) {
        if arg_index < 6 {
            self.place_arg_at_register(kind, arg_index, register);
        } else {
            self.push_arg_to_stack(kind, register);
        }
    }

    fn flatten_array(
        &mut self,
        base_register: RegisterIndex,
        array_len: usize,
        element_type: &Type,
        mut arg_index: usize,
    ) {
        if let Type::Array { len, ty_pe } = element_type {
            let element_size = 8; // arrays decay into pointers and pointers are 8 bytes
            for index in (0..array_len).rev() {
                let first_ele = self.registers.allocate(&Type::String);
                let offset = if index == 0 {
                    0_isize
                } else {
                    -(index as isize * element_size as isize)
                };
                self.emit_code(
                    "movq",
                    &format!("{offset}({base_register})"),
                    &first_ele,
                );
                self.flatten_array(first_ele, *len, ty_pe, arg_index);
                arg_index -= *len;
            }
            self.registers.deallocate(base_register);
        } else {
            let element_size = element_type.as_size();
            for (count, index) in (0..array_len).rev().enumerate() {
                let offset = if index == 0 {
                    0_isize
                } else {
                    -(index as isize * element_size as isize)
                };
                self.store_argument(
                    arg_index - count,
                    &format!("{offset}({base_register})"),
                    element_type,
                );
            }
            self.registers.deallocate(base_register);
        }
    }

    fn format_print_argument(
        &mut self,
        arg_index: usize,
        register: RegisterIndex,
        kind: &Type,
    ) {
        if let Type::Array { len, ty_pe } = kind {
            self.flatten_array(register, *len, ty_pe, arg_index);
        } else {
            self.store_argument(arg_index, &register.to_string(), kind);
            self.registers.deallocate(register);
        }
    }

    fn align_arguments(&mut self, arg_count: usize) -> Option<usize> {
        arg_count.checked_sub(5).map(|difference| {
            let stack_allocated_space = difference * 8;
            let align = 16;
            let remainder = stack_allocated_space % align;
            if remainder == 0 {
                stack_allocated_space
            } else {
                let increment = align - remainder;
                self.emit_code("subq", &format!("${increment}"), "%rsp");
                increment + stack_allocated_space
            }
        })
    }

    fn format_print_args(
        &mut self,
        args: &[LLExpr],
        mut arg_count: usize,
    ) -> Option<usize> {
        let alignment = self.align_arguments(arg_count);
        for arg in args.iter().rev() {
            let register = self.expression(arg);
            let kind = arg.to_type();
            self.format_print_argument(arg_count, register, &kind);
            arg_count -= kind.count_elements();
        }
        alignment
    }

    fn print_stmt(&mut self, format: &str, args: &[LLExpr], arg_count: usize) {
        let alignment = self.format_print_args(args, arg_count);
        let format_string = self.string(format);
        self.emit_code("movq", &format_string, "%rdi");
        self.registers.deallocate(format_string);
        self.emit_code("xor", "%eax", "%eax");
        self.emit_code("call", "printf@PLT", "");
        if let Some(value) = alignment {
            self.emit_code("addq", format!("${value}"), "%rsp");
        }
    }

    fn emit_data(
        &mut self,
        symbol_name: &str,
        size: &str,
        value: impl std::fmt::Debug,
    ) {
        writeln!(&mut self.assembly.data, "{symbol_name}:")
            .expect("Unable to write into data.");
        writeln!(&mut self.assembly.data, "{:8}.{size} {value:?}", " ")
            .expect("Unable to write into data.");
    }

    fn emit_code(
        &mut self,
        instruction: impl fmt::Display,
        first_operand: impl fmt::Display,
        second_operand: impl fmt::Display,
    ) {
        write!(&mut self.assembly.code, "{:8}{instruction:7}", " ")
            .expect("Unable to write code.");
        let first_operand = first_operand.to_string();
        let second_operand = second_operand.to_string();
        if !first_operand.is_empty() && !second_operand.is_empty() {
            write!(
                &mut self.assembly.code,
                "{first_operand}, {second_operand}"
            )
            .expect("Unable to write code.");
        } else if !first_operand.is_empty() {
            write!(&mut self.assembly.code, "{first_operand}")
                .expect("Unable to write code.");
        }
        writeln!(&mut self.assembly.code).expect("Unable to write code.");
    }

    fn emit_label<L: Into<Label>>(&mut self, label: L) {
        writeln!(&mut self.assembly.code, "{}:", label.into())
            .expect("Unable to write label.");
    }

    fn global_variable_decl(&mut self, name: &str, init: &LLExpr) {
        self.emit_data(name, "quad", init);
    }

    fn expression(&mut self, expr: &LLExpr) -> RegisterIndex {
        match expr {
            LLExpr::Binary {
                left,
                oper,
                right,
                ty_pe,
            } => self.binary(left, oper, right, ty_pe),
            LLExpr::Number(num) => self.number(*num),
            LLExpr::Char(value) => self.character(*value),
            LLExpr::Variable {
                name, ty_pe, kind, ..
            } => self.variable(name, ty_pe, kind),
            LLExpr::Boolean(value) => self.boolean(*value),
            LLExpr::Call {
                callee,
                args,
                ty_pe,
            } => self.function_call(callee, args, ty_pe),
            LLExpr::Assign {
                value,
                index,
                ty_pe,
            } => self.assignment(value, *index, ty_pe),
            LLExpr::Unary { oper, right, .. } => self.unary(oper, right),
            LLExpr::Group { value, .. } => self.expression(value),
            LLExpr::String(literal) => self.string(literal),
            LLExpr::Array {
                ty_pe,
                elements,
                index,
            } => self.array(ty_pe, elements, *index),
            LLExpr::Subscript { base, ty_pe, index } => {
                self.subscript(base, ty_pe, index)
            }
        }
    }

    fn move_index_to_eax(&mut self, index: &LLExpr) -> RegisterIndex {
        let index = self.expression(index);
        let result = self.registers.allocate(&Type::String);
        self.emit_code("neg", &index, "");
        self.emit_code("movslq", &index, &result);
        self.registers.deallocate(index);
        result
    }

    fn increment_base(
        &mut self,
        base: &RegisterIndex,
        ty_pe: &Type,
        index: RegisterIndex,
    ) {
        let offset = self.registers.allocate(&Type::String);
        let element_size = if ty_pe.is_array() { 8 } else { ty_pe.as_size() };
        self.emit_code("leaq", format!("0(,{index},{element_size})"), &offset);
        self.emit_code("addq", &offset, base);
        self.registers.deallocate(offset);
        self.registers.deallocate(index);
    }

    fn subscript(
        &mut self,
        array: &LLExpr,
        ty_pe: &Type,
        index: &LLExpr,
    ) -> RegisterIndex {
        let base = self.expression(array);
        let index = self.move_index_to_eax(index);
        self.increment_base(&base, ty_pe, index);
        let result = self.registers.allocate(ty_pe);
        self.emit_code(move_for(ty_pe), format!("({base})"), &result);
        self.registers.deallocate(base);
        result
    }

    fn emit_string(&mut self, label: &Label, literal: &str) {
        writeln!(&mut self.assembly.header, "{label}:")
            .expect("Unable to emit string.");
        writeln!(&mut self.assembly.header, "{:8}.string \"{literal}\"", " ")
            .expect("Unable to emit string.");
        writeln!(&mut self.assembly.header, "{:8}.text", " ")
            .expect("Unable to emit string.");
    }

    fn array(
        &mut self,
        ty_pe: &Type,
        elements: &[LLExpr],
        index: usize,
    ) -> RegisterIndex {
        let mut start = index;
        let size = if ty_pe.is_array() { 8 } else { ty_pe.as_size() };
        for ele in elements {
            let value = self.expression(ele);
            self.emit_code(move_for(ty_pe), &value, &format!("-{start}(%rbp)"));
            self.registers.deallocate(value);
            start += size;
        }
        let register = self.registers.allocate(&Type::String);
        self.emit_code("leaq", &format!("-{index}(%rbp)"), &register);
        register
    }

    fn string(&mut self, literal: &str) -> RegisterIndex {
        let label = self.labels.create();
        self.emit_string(&label, literal);
        let register = self.registers.allocate(&Type::String);
        self.emit_code("leaq", format!("{label}(%rip)"), &register);
        register
    }

    fn unary(&mut self, oper: &UnaryOperand, right: &LLExpr) -> RegisterIndex {
        let operand = self.expression(right);
        match oper {
            UnaryOperand::LogicalNot => {
                self.emit_code("not", &operand, "");
                self.emit_code("inc", &operand, "");
                self.emit_code("inc", &operand, "");
            }
            UnaryOperand::Negate => self.emit_code("neg", &operand, ""),
        }
        operand
    }

    fn assignment(
        &mut self,
        value: &LLExpr,
        index: usize,
        ty_pe: &Type,
    ) -> RegisterIndex {
        let value = self.expression(value);
        self.emit_code(move_for(ty_pe), &value, format!("-{index}(%rbp)"));
        value
    }

    fn emit_arguments(&mut self, args: &[LLExpr]) -> Option<usize> {
        let alignment = self.align_arguments(args.len());
        for (arg_index, arg) in args.iter().enumerate().rev() {
            let register = self.expression(arg);
            let kind = arg.to_type();
            self.store_argument(arg_index, &register.to_string(), &kind);
            self.registers.deallocate(register);
        }
        alignment
    }

    fn function_call(
        &mut self,
        callee: &str,
        args: &[LLExpr],
        ty_pe: &Type,
    ) -> RegisterIndex {
        let alignment = self.emit_arguments(args);
        self.emit_code("pushq", "%r10", "");
        self.emit_code("pushq", "%r11", "");
        self.emit_code("call", callee, "");
        self.emit_code("popq", "%r11", "");
        self.emit_code("popq", "%r10", "");
        if let Some(value) = alignment {
            self.emit_code("addq", format!("${value}"), "%rsp");
        }
        self.move_return_value(ty_pe)
    }

    fn move_return_value(&mut self, ty_pe: &Type) -> RegisterIndex {
        if ty_pe != &Type::Unit {
            let result = self.registers.allocate(ty_pe);
            self.emit_code(move_for(ty_pe), rax_for(ty_pe), &result);
            result
        } else {
            let result = self.registers.allocate(&Type::Number);
            self.emit_code("movl", "$0", &result);
            result
        }
    }

    fn boolean(&mut self, value: bool) -> RegisterIndex {
        let number = u8::from(value);
        let register = self.registers.allocate(&Type::Bool);
        self.emit_code("movb", format!("${number}"), &register);
        register
    }

    fn variable(
        &mut self,
        name: &str,
        ty_pe: &Type,
        kind: &Kind,
    ) -> RegisterIndex {
        let register = self.registers.allocate(ty_pe);
        if let Kind::Parameter(index) | Kind::LocalVariable(index) = kind {
            self.emit_code(
                move_for(ty_pe),
                format!("-{index}(%rbp)"),
                &register,
            );
        } else {
            let symbol = format!("{name}(%rip)");
            self.emit_code(move_for(ty_pe), symbol, &register);
        }
        register
    }

    fn binary(
        &mut self,
        left: &LLExpr,
        oper: &BinaryOperand,
        right: &LLExpr,
        ty_pe: &Type,
    ) -> RegisterIndex {
        let left_register = self.expression(left);
        let right_register = self.expression(right);
        match &oper {
            BinaryOperand::Add => {
                self.emit_code("addl", &left_register, &right_register);
                self.registers.deallocate(left_register);
                right_register
            }
            BinaryOperand::Subtract => {
                self.emit_code("subl", &right_register, &left_register);
                self.registers.deallocate(right_register);
                left_register
            }
            BinaryOperand::Multiply => {
                self.emit_code("imull", &left_register, &right_register);
                self.registers.deallocate(left_register);
                right_register
            }
            BinaryOperand::Divide => {
                let result_register = self.registers.allocate(ty_pe);
                self.emit_code("movl", &left_register, "%eax");
                self.emit_code("cqo", "", "");
                self.emit_code("idivl", &right_register, "");
                self.emit_code("movl", "%eax", &result_register);
                self.registers.deallocate(left_register);
                self.registers.deallocate(right_register);
                result_register
            }
            BinaryOperand::Modulus => {
                let result_register = self.registers.allocate(ty_pe);
                self.emit_code("movl", &left_register, "%eax");
                self.emit_code("cqo", "", "");
                self.emit_code("idivl", &right_register, "");
                self.emit_code("movl", "%edx", &result_register);
                self.registers.deallocate(left_register);
                self.registers.deallocate(right_register);
                result_register
            }
            BinaryOperand::Compare(comparison_operand) => {
                let result = self.registers.allocate(ty_pe);
                let false_label = self.labels.create();
                let done_label = self.labels.create();
                self.emit_code(
                    cmp_for(&left.to_type()),
                    &right_register,
                    &left_register,
                );
                self.comparison_operation(comparison_operand, &false_label);
                self.emit_code("movb", "$1", &result);
                self.emit_code("jmp", &done_label, "");
                self.emit_label(false_label);
                self.emit_code("movb", "$0", &result);
                self.emit_label(done_label);
                self.registers.deallocate(left_register);
                self.registers.deallocate(right_register);
                result
            }
            BinaryOperand::Logic(operand) => {
                self.logical_operation(operand, left_register, right_register)
            }
        }
    }

    fn logical_operation(
        &mut self,
        operand: &Logical,
        lhs: RegisterIndex,
        rhs: RegisterIndex,
    ) -> RegisterIndex {
        let result = self.registers.allocate(&Type::Bool);
        match operand {
            Logical::Or => {
                let true_branch = self.labels.create();
                let false_branch = self.labels.create();
                let end = self.labels.create();
                self.emit_code("cmpb", "$0", &lhs);
                self.emit_code("jne", &true_branch, "");
                self.emit_code("cmpb", "$0", &rhs);
                self.emit_code("je", &false_branch, "");
                self.emit_label(true_branch);
                self.emit_code("movb", "$1", &result);
                self.emit_code("jmp", &end, "");
                self.emit_label(false_branch);
                self.emit_code("movb", "$0", &result);
                self.emit_label(end);
            }
            Logical::And => {
                let false_branch = self.labels.create();
                let end = self.labels.create();
                self.emit_code("cmpb", "$0", &lhs);
                self.emit_code("je", &false_branch, "");
                self.emit_code("cmpb", "$0", &rhs);
                self.emit_code("je", &false_branch, "");
                self.emit_code("movb", "$1", &result);
                self.emit_code("jmp", &end, "");
                self.emit_label(false_branch);
                self.emit_code("movb", "$0", &result);
                self.emit_label(end);
            }
        }
        self.registers.deallocate(lhs);
        self.registers.deallocate(rhs);
        result
    }

    fn number(&mut self, value: i32) -> RegisterIndex {
        let register = self.registers.allocate(&Type::Number);
        self.emit_code("movl", format!("${}", value).as_str(), &register);
        register
    }

    fn character(&mut self, value: char) -> RegisterIndex {
        let register = self.registers.allocate(&Type::Char);
        self.emit_code("movb", format!("${}", value as u8).as_str(), &register);
        register
    }
}
