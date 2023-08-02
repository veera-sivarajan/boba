fn number() -> i32 {
    return 25;
}

fn boolean() -> bool {
    return false;
}

fn string() -> str {
    return "hello, world";
}

fn character() -> char {
    return 'W';
}

fn main() {
    println("{} {} {} {}", number(), boolean(), string(), character());
}
