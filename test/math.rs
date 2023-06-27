let global = 100;

fn dummy() -> Number {}

fn factorial(x: Number, y: Number) -> Number {
    let apple = 10;
    let ball = 20;
    if x > y {
        let car = 30;
        println(apple + car);
    } else {
        let apple = 100;
        let ball = dummy();
        println(apple + ball);
    }
}

fn main() -> Number {
    factorial();
}
