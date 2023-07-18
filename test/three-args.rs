fn compute(a: i64, b: i64, c: i64) -> i64 {
    let x = a + b + c;
    let y = x * 5;
    return y;
}

fn main() {
    println(compute(1, 2, 3));
}
