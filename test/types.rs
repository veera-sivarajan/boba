fn hello() -> str {
    return "hello";
}

fn number() -> i32 {
    for (let mut i = 0; i < 10; i = i + 1) {
        if is_even(i) {
            return i;
        }
    }
    return 10;
}

fn is_even(num: i32) -> bool {
    return num % 2 == 0;
}

fn main() {
    println("{}", hello());
    println("{}", number());
    println("{}", is_even(3));
}

