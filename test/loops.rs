fn for_loop(upto: i32) {
    for (let mut i = 0; i < upto; i = i + 1) {
        println(i);
    }
}

fn while_loop(upto: i32) {
    let mut i = 0;
    while (i < upto) {
        println(i);
        i = i + 1;
    }
}

fn main() {
    for_loop(10);
    while_loop(10);
}
