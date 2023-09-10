fn is_even(num: i32) -> bool {
    return num % 2 == 0;
}

fn main() {
    println("Or: {}, {}, {}, {}", true || false, false || true, false || false, true || true);
    println("And: {}, {}, {}, {}", true && false, false && true, false && false, true && true);

    // or checks
    if is_even(2) || is_even(3) {
        println("Or check 1 Passed.");
    } else {
        println("Or check 1 failed.");
    }

    if is_even(5) || is_even(6) {
        println("Or check 2 Passed.");
    } else {
        println("Or check 2 failed.");
    }

    if is_even(7) || is_even(9) {
        println("Or check 3 Failed.");
    } else {
        println("Or check 3 Passed.");
    }

    if is_even(10) || is_even(12) {
        println("Or check 4 Passed.");
    } else {
        println("Or check 4 Failed.");
    }


    // and checks
    if is_even(2) && is_even(3) {
        println("And check 1 failed.");
    } else {
        println("And check 1 Passed.");
    }

    if is_even(5) && is_even(6) {
        println("And check 2 failed.");
    } else {
        println("And check 2 passed.");
    }

    if is_even(7) && is_even(9) {
        println("And check 3 failed.");
    } else {
        println("And check 3 Passed.");
    }

    if is_even(10) && is_even(12) {
        println("And check 4 Passed.");
    } else {
        println("And check 4 Failed.");
    }
    

}

