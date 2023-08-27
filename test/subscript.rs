fn print(array: [[char; 2]; 3]) {
    println(
        "{}, {}, {}, {}, {}, {}",
        array[0][0],
        array[0][1],
        array[1][0],
        array[1][1],
        array[2][0],
        array[2][1]
    );
}

fn main() {
    // 1 dimensional arrays
    let array = [1, 2, 3, 4, 5];
    println(
        "{}, {}, {}, {}, {}",
        array[0],
        array[1],
        array[2],
        array[3],
        array[4]
    );

    let bools = [true, false, true, false];
    println("{}, {}, {}, {}", bools[0], bools[1], bools[2], bools[3]);

    let strings = ["Hello", "Mister", "Edirkatchi", "Kelvi", "badhil"];
    println(
        "{}, {}, {}, {}, {}",
        strings[0],
        strings[1],
        strings[2],
        strings[3],
        strings[4]
    );

    let alphabet = ['a', 'b', 'c', 'd', 'e'];
    println(
        "{}, {}, {}, {}, {}",
        alphabet[0],
        alphabet[1],
        alphabet[2],
        alphabet[3],
        alphabet[4]
    );

    // 2 dimensional arrays
    let pairs = [[1, 2], [3, 4], [5, 6]];
    println(
        "{}, {}, {}, {}, {}, {}",
        pairs[0][0],
        pairs[0][1],
        pairs[1][0],
        pairs[1][1],
        pairs[2][0],
        pairs[2][1]
    );

    let booms = [[true, false], [false, true], [true, false]];
    println(
        "{}, {}, {}, {}, {}, {}",
        booms[0][0],
        booms[0][1],
        booms[1][0],
        booms[1][1],
        booms[2][0],
        booms[2][1]
    );

    let lyrics = [
        ["Hello", "Mister"],
        ["Edirkatchi", "Kelvi"],
        ["badhil", "ennachu"]
    ];
    println(
        "{}, {}, {}, {}, {}, {}",
        lyrics[0][0],
        lyrics[0][1],
        lyrics[1][0],
        lyrics[1][1],
        lyrics[2][0],
        lyrics[2][1]
    );

    let letters = [['a', 'b'], ['c', 'd'], ['e', 'f']];
    println(
        "{}, {}, {}, {}, {}, {}",
        letters[0][0],
        letters[0][1],
        letters[1][0],
        letters[1][1],
        letters[2][0],
        letters[2][1]
    );
    print(letters);

    // mega arrays
    let hundred = [
        [0, 1],
        [2, 3],
        [4, 5],
        [6, 7],
        [8, 9],
        [10, 11],
        [12, 13],
        [14, 15],
        [16, 17],
        [18, 19],
        [20, 21],
        [22, 23],
        [24, 25],
        [26, 27],
        [28, 29],
        [30, 31],
        [32, 33],
        [34, 35],
        [36, 37],
        [38, 39],
        [40, 41],
        [42, 43],
        [44, 45],
        [46, 47],
        [48, 49],
        [50, 51],
        [52, 53],
        [54, 55],
        [56, 57],
        [58, 59],
        [60, 61],
        [62, 63],
        [64, 65],
        [66, 67],
        [68, 69],
        [70, 71],
        [72, 73],
        [74, 75],
        [76, 77],
        [78, 79],
        [80, 81],
        [82, 83],
        [84, 85],
        [86, 87],
        [88, 89],
        [90, 91],
        [92, 93],
        [94, 95],
        [96, 97],
        [98, 99]
    ];
    println(
        "{}, {}, {}, {}, {}",
        hundred[0][0],
        hundred[49][1],
        hundred[25][0],
        hundred[12][1],
        hundred[37][1]
    );

    let essay = [
        "Lorem",
        "ipsum",
        "dolor",
        "sit",
        "amet,",
        "consectetur",
        "adipiscing",
        "elit.",
        "Praesent",
        "commodo",
        "neque",
        "eu",
        "ligula",
        "ullamcorper,",
        "eu",
        "tincidunt",
        "nulla",
        "fermentum.",
        "Sed",
        "at",
        "bibendum",
        "nisl.",
        "Quisque",
        "id",
        "felis",
        "sed",
        "ligula",
        "mattis",
        "vulputate.",
        "Duis",
        "quis",
        "est",
        "quis",
        "felis",
        "volutpat",
        "posuere.",
        "Nulla",
        "facilisi.",
        "Proin",
        "bibendum",
        "tortor",
        "id",
        "sapien",
        "vestibulum,",
        "eu",
        "feugiat",
        "ex",
        "aliquam.",
        "Integer",
        "vel",
        "nulla",
        "nec",
        "odio",
        "finibus",
        "cursus.",
        "Curabitur",
        "ac",
        "ante",
        "nec",
        "ipsum",
        "eleifend",
        "varius",
        "nec",
        "id",
        "odio.",
        "Ut",
        "maximus",
        "diam",
        "eget",
        "nunc",
        "venenatis,",
        "ut",
        "viverra",
        "risus",
        "bibendum.",
        "Nam",
        "aliquam",
        "condimentum",
        "dolor,",
        "eget",
        "interdum",
        "nibh.",
        "Suspendisse",
        "potenti."
    ];
    println(
        "{}, {}, {}, {}, {}",
        essay[0],
        essay[1],
        essay[83],
        essay[82],
        essay[50]
    );

    let para = [
        ["Lorem", "ipsum"],
        ["dolor", "sit"],
        ["amet,", "consectetur"],
        ["adipiscing", "elit."],
        ["Praesent", "commodo"],
        ["neque", "eu"],
        ["ligula", "ullamcorper,"],
        ["eu", "tincidunt"],
        ["nulla", "fermentum."],
        ["Sed", "at"],
        ["bibendum", "nisl."],
        ["Quisque", "id"],
        ["felis", "sed"],
        ["ligula", "mattis"],
        ["vulputate.", "Duis"],
        ["quis", "est"],
        ["quis", "felis"],
        ["volutpat", "posuere."],
        ["Nulla", "facilisi."],
        ["Proin", "bibendum"],
        ["tortor", "id"],
        ["sapien", "vestibulum,"],
        ["eu", "feugiat"],
        ["ex", "aliquam."],
        ["Integer", "vel"],
        ["nulla", "nec"],
        ["odio", "finibus"],
        ["cursus.", "Curabitur"],
        ["ac", "ante"],
        ["nec", "ipsum"],
        ["eleifend", "varius"],
        ["nec", "id"],
        ["odio.", "Ut"],
        ["maximus", "diam"],
        ["eget", "nunc"],
        ["venenatis,", "ut"],
        ["viverra", "risus"],
        ["bibendum.", "Nam"],
        ["aliquam", "condimentum"],
        ["dolor,", "eget"],
        ["interdum", "nibh."],
        ["Suspendisse", "potenti."]
    ];
    println(
        "{}, {}, {}, {}, {}",
        para[0][0],
        para[0][1],
        para[41][0],
        para[41][1],
        para[40][0]
    );
}
