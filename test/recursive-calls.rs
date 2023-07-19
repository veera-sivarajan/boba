fn one() {
    println("Function one");
    two();
}

fn two() {
    println("Function two");
    three();
}

fn three() {
    println("Function three");
}

fn main() {
    one();
}
