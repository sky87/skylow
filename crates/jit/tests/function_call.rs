//! Test for function calls between JIT-compiled functions

use jit::{compile_program, ExecutableMemory};
use mir::{BinOp, CmpOp, InstKind, MirFunction, MirParam, MirProgram};

#[test]
fn test_function_call() {
    // Create add function: return a + b
    let mut add_fn = MirFunction::new_function("add".to_string());
    let a = add_fn.alloc_reg();  // Reg(0)
    let b = add_fn.alloc_reg();  // Reg(1)
    add_fn.params.push(MirParam { name: "a".to_string(), reg: a });
    add_fn.params.push(MirParam { name: "b".to_string(), reg: b });
    let sum = add_fn.alloc_reg();  // Reg(2)
    add_fn.emit(InstKind::BinOp { op: BinOp::Add, dst: sum, left: a, right: b });
    add_fn.emit(InstKind::RetVal { value: sum });

    // Create test function that calls add(2, 3)
    let mut test_fn = MirFunction::new("test_add".to_string());
    let r0 = test_fn.alloc_reg();  // Reg(0)
    let r1 = test_fn.alloc_reg();  // Reg(1)
    let r2 = test_fn.alloc_reg();  // Reg(2)
    let r3 = test_fn.alloc_reg();  // Reg(3)
    let r4 = test_fn.alloc_reg();  // Reg(4)
    test_fn.emit(InstKind::LoadImm { dst: r0, value: 2 });
    test_fn.emit(InstKind::LoadImm { dst: r1, value: 3 });
    test_fn.emit(InstKind::Call { dst: r2, func_name: "add".to_string(), args: vec![r0, r1] });
    test_fn.emit(InstKind::LoadImm { dst: r3, value: 5 });
    test_fn.emit(InstKind::Cmp { op: CmpOp::Eq, dst: r4, left: r2, right: r3 });
    test_fn.emit(InstKind::Assert { cond: r4, msg_id: 0 });
    test_fn.emit(InstKind::Ret);

    // Create program with both functions
    let mut program = MirProgram::default();
    program.functions.push(add_fn);
    program.functions.push(test_fn);

    // Compile
    let compiled = compile_program(&program);
    
    eprintln!("Compiled {} functions:", compiled.functions.len());
    for (i, func) in compiled.functions.iter().enumerate() {
        eprintln!("  {}: {} at offset {} with {} call sites", 
            i, func.name, func.offset, func.call_sites.len());
        for site in &func.call_sites {
            eprintln!("    Call to {} at position {}", site.func_name, site.bl_pos);
        }
    }
    eprintln!("Total code size: {} bytes", compiled.code.len());
    
    // Dump first bytes
    eprintln!("\nCode (first 100 bytes):");
    for (i, byte) in compiled.code.iter().take(100).enumerate() {
        if i % 16 == 0 {
            eprint!("\n{:04x}: ", i);
        }
        eprint!("{:02x} ", byte);
    }
    eprintln!();

    // Execute
    let mem = ExecutableMemory::new(&compiled.code).unwrap();
    
    // Run the test function (index 1)
    eprintln!("\nRunning test function at offset {}...", compiled.functions[1].offset);
    let test_fn_ptr: extern "C" fn() -> u8 = unsafe { 
        mem.as_fn_at_offset(compiled.functions[1].offset) 
    };
    let result = test_fn_ptr();
    eprintln!("Result: {}", result);
    
    assert_eq!(result, 0, "Test should pass (add(2,3) == 5)");
}
