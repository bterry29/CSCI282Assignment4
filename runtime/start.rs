// runtime/start.rs
// This file provides the entry point for compiled programs
#[no_mangle]
pub extern "C" fn snek_error(errcode:i64){
    if errcode == 1{
        eprintln!("invalid argument");
    }else if errcode == 2{
        eprintln!("overflow");
    }
    std::process::exit(1);
}
#[no_mangle]
pub extern "C" fn snek_print(val: i64) -> i64 {
    if val & 1 == 0{
        println!("{}", val >> 1);
    }else if val == 3{
        println!("true");
    }else if val == 1{
        println!("false");
    }
    val
}
#[link(name = "our_code")]
extern "C" {
    // The \x01 here is an undocumented feature of LLVM that ensures
    // it does not add an underscore in front of the name on macOS
    #[link_name = "\x01our_code_starts_here"]
    fn our_code_starts_here() -> i64;
}

fn main() {
    let i: i64 = unsafe {
        our_code_starts_here()
    };
    snek_print(i);
}
