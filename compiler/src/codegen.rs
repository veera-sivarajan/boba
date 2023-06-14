use std::collections::HashMap;

struct ScratchRegisters {
    values: [(Box<str>, bool); 7],
}

struct Register(u8);

impl ScratchRegisters {
    pub fn new() -> Self {
        Self {
            values: [
                ("%rbx".into(), false),
                ("%r10".into(), false),
                ("%r11".into(), false),
                ("%r12".into(), false),
                ("%r13".into(), false),
                ("%r14".into(), false),
                ("%r15".into(), false),
            ]
        }
    }

    pub fn allocate(&self) -> Register {


    }

    pub fn deallocate(&self, register: Register) {


    }

    pub fn name(&self, register: Register) -> Box<str> {


    }
}

struct CodeGen {
    registers: ScratchRegisters,
}
