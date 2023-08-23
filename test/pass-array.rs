fn greet(names: [str; 5]) {
    println("{}", names);
}

fn sum(numbers: [i32; 10]) -> i32 {
    println("{}", numbers);
    return 50;
}

fn sample_space(bools: [bool; 2]) {
    println("{}", bools);
}

fn compute(matrix: [[i32; 5]; 5]) {
    let local = matrix;
    println("{}", local);
}

fn main() {
    let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
    greet(["Billa", "Venba", "Tamara", "Liliana", "Anuradha"]);
    sum(numbers);
    sample_space([true, false]);
    let a = [1, 2, 3, 4, 5];
    let b = [6, 7, 8, 9, 10];
    let c = [11, 12, 13, 14, 15];
    let d = [16, 17, 18, 19, 20];
    let e = [21, 22, 23, 24, 25];
    compute([a, b, c, d, e]);
}
