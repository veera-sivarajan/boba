fn factorial(num: i32) -> i32 {
    let a = 2;
    let b = 3;
    if num == 0 {
        return 1;
    } else {
        return num * factorial(num - 1);
    }
}

fn main() {
    let c = 4;
    println("{}", factorial(5));
}
