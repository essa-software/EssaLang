x ∈ Z and <1;11>

x + 1 ∈ Z and <2;12>

```swift
func someFunction() : int(1..12) {}

int(1..12) levelId = someFunction();
// levelId ∈ <1;11>

if (levelId == 11) {
    return;
}
// levelId ∈ <1;11>\{11} = <1;10>

levelId += 1;
// levelId ∈ <2;11>   OK

// MODE = check,ub,wraparound
int(44000-44100,MODE) bitrate = readFromFile();

bitrate += 1 // may overflow because bitrate ∈ <44001;44101>

bitrate +1

if !(bitrate > 44000) && !(bitrate < 44100)
    throw Error;

func readFromFile() : int(44050..44101)

let bitrate: int<44000..44100> = readFromFile();                    // Compile error: int(44050..44101) is not a subset of int(44000..44100)
let bitrate: int<44000..44100> [overflow=unsafe] = readFromFile();          // OK, but UB/crash on overflow
let bitrate: int<44000..44100> [overflow=wraparound] = readFromFile();      // OK and will wraparound

type u16 = int(0..65535) // In stdlib


// explicitly require compiler to run fibonacci for every n
// until it finds case that overflows (that must be laggy)
func fibonacci(n: int<0..{n: fibonacci(n) > u64::max}>) : u64 {
    mut a: u64 = 1;     // a: {0} IN <0;2^64-1>
    mut a1: u64 = 1;    // a1: {0} IN <0;2^64-1>
    mut a2: u64 = 1;    // a2: {1} IN <0;2^64-1>

    if (n < 2) {
        return 0;
    } // n: <2;2^64-1> IN <0;2^64-1>

    for (let i in 2..n) { // i: <2;2^64-1> IN <2;2^64-1>
        a = a1 + a2; // a: {2} IN <0;2^64-1>
        a2 = a1; // a2: {1} IN <0;2^64-1>
        a1 = a; // a1: {2} IN <0;2^64-1>
    } // ASSUME i > 2 => loop repeats
    return a;
}
```
