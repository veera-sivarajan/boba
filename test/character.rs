fn print_values(name: str, age: i32, is_even: bool, win: char) {
    println("{}, {}, {}, {}", name, age, is_even, win);
}


fn main() {
    let a = 'a';
    println("{}", a);

    let b = 'b';
    let c = b;
    println("{}", c);

    print_values("billa", 32, false, 'w');
}
