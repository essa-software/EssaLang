x ∈ Z and <1;11>

x + 1 ∈ Z and <2;12>

```swift
func someFunction() : (int & range(1, 12))

let levelId = someFunction();
// levelId ∈ int&<1;11>

if (levelId == 11) {
    return;
}
// levelId ∈ <1;11>\{11} = <1;10>

levelId += 1;
// levelId ∈ <2;11>   OK

// MODE = check,ub,wraparound
let bitrate = readFromFile();


bitrate += 1 // may overflow because bitrate ∈ <44001;44101>

bitrate +1

if !(bitrate > 44000) && !(bitrate < 44100)
    throw Error;

func readFromFile() : int(44050..44101)

let bitrate: int<44000..44100> = readFromFile();                    // Compile error: int(44050..44101) is not a subset of int(44000..44100)
let bitrate: int<44000..44100> [overflow=unsafe] = readFromFile();          // OK, but UB/crash on overflow
let bitrate: int<44000..44100> [overflow=wraparound] = readFromFile();      // OK and will wraparound

type int = builtin::types::bigint;
type uint = int & (n => n >= 0);

// Infinite precision version
func fibonacci(n: uint) : uint {
    mut a: uint = 1;
    mut a1: uint = 1;
    mut a2: uint = 1;

    if (n < 2) {
        return 0;
    }

    for (let i in 2..n) {
        a = a1 + a2;
        a2 = a1;
        a1 = a;
    }
    return a;
}

// Optimized version (Runtime crash on overflow)
type u64 = builtin::types::u64;

func fibonacciFast(n: u64)
```
