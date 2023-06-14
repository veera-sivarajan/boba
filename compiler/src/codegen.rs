use crate::expr::Expr;
use std::collections::HashMap;
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

pub struct CodeGen {
    registers: ScratchRegisters,
}

impl CodeGen {
    pub fn new() -> Self {
        Self {
            registers: ScratchRegisters::new(),
        }
    }

    pub fn generate_assembly(&mut self, _ast: &[Expr]) {
        for _ in 0..7 {
            let register = self.registers.allocate();
            println!("{}", self.registers.name(&register));
            self.registers.deallocate(register);
        }
    }
}
