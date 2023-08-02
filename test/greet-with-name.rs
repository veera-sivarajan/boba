fn greet(name: str) {
    println("Hello, {}! Hope you're doing great!!", name);
}


fn greet_in_spanish(name: str) {
    println("Hola, {}", name);
}

fn main() {
    greet("billa");
    greet("liliana");
    greet("toby");

    
    greet_in_spanish("billa");
    greet_in_spanish("liliana");
    greet_in_spanish("toby");
}
