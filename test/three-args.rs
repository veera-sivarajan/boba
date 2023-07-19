fn compute(a: i32, b: i32, c: i32) -> i32 {
    let x = a + b + c;
    let y = x * 5;
    return y;
}

fn main() {
    println(compute(1, 2, 3));
}
