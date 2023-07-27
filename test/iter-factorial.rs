fn factorial(num: i32) -> i32 {
    let mut product = 1;
    for (let mut i = num; i >= 1; i = i - 1) {
        product = product * i;
    }
    return product;
}

fn main() {
    println("{}", factorial(5));
}
