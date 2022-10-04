# EssaLang

EssaLang (ESL) is a memory-safe alternative to C++.

It is in heavy development and definitely not ready for production use :^).

## Main concepts
- Memory safety:
     - unsafe block?
     - unsafe_ptr?
- Memory model:
     - ? Raw pointers        |-
     - ? Borrow checker      |
     - ? Refcounting         |+
     - ? Garbage collection  |-
     - Const by default      |+
- Compiled
     - LLVM  |+-
     - C     |+

- You need to add something to expressions that may panic at runtime ??

## Syntax
- Braces everywhere, semicolons after every statement

## Operators

### Arithmetic

#### Option 1 (Zig): Wrapping operators, runtime error/UB by default
```swift
mut a: u8 = 10;
a +%= 256;  // Will wrap.
a += 256;   // Crash, you can't tell statically.
a +!= 256;  // May crash and you can immediatelly tell, but you need to know the operator.
```

#### Option 2 (Old Zig): Wrapping types
```swift
mut a: u8 = 10;
a += 256;   // Crash, you can't tell statically.
a +!= 256;  // May crash and you can immediatelly tell, but you need to know the operator.

mut a: wu8 = 10; // w means wrapping
a += 256;   // Will wrap.
a +!= 256;  // error: doesn't make sense because this is never UB
```

#### Option 3 (Rust): Overflow is always UB
#### Option 4 (C++ and all other languages): Overflow is wrapping for uints, UB for ints

## Control

### Conditionals
```swift
if (a == 5) {
    // () and {} are required.
} else if (b == 10 and a > 10) {
    // Siema tej
} else {
    // something
}
```

### Loops

While
```swift
while (condition) {

}
```

For each
```swift
let values: [5]int;
for (let i of values) { // iterate on values
    print(i);
    i = 5; // Error
}

for (let [idx, i] of values) { // iterate on indices/keys

}

for (let [idx, _] of values) { // '_' as placeholder for unused value

}
```

Iteration on range
```swift
for(let a of arr)
for(let i of 1..100)

int[5] array;
array[2..3] -> int[]
string[2..3] -> string
```

### Iterators
```swift
```

## Types, variables
```swift
let a: int = 5;
a = 5; // error: 'a' is not writable
let a = 5;
let a; // error: uninitialized variable

mut b = 5; // creates mutable binding
mut b: int = 5;
mut b: int; // error: uninitialized variable
b = 5; // ok
```

### Arrays
```swift
let a: [5]int; // static, on stack
let a: []int = something(); // slice (std::span) czy auto
let a: [&]int = { 1, 2, 3 }; // array with auto detected
```

### Standard objects
```swift
let a: List<int>;
let a: Vector<int>;
let a: Map<int, string>;
```

### Strings
```swift
enum StringEncoding {
    Utf8,
    ASCII,
    Internal
}
class string<enc: StringEncoding = Internal> {
    // ...
}
```

```swift
"testął€œ¢€ŋ’œ€©" // by default, string<Internal>

let str: string = "działają polskie znaki"; // some decoded internal representation, something like Util::UString
let str: string<Utf8> = "działają polskie znaki"; // is guaranteed to be utf8
let utf8str: string<Utf8> = str.encode<Utf8>();
let ascii_string: string<ASCII> = "nie działają polskie znaki"; // error: character 'ł' is invalid for encoding 'ASCII'
print(str[5]); // random access
print(str); // prints in output's encoding, utf8 by default
```

## Array OOB
```swift
let a = 5;
// ...
let array: [5]int = zero();
array[a] = 1234; // UB [-], runtime error (abort), exception [-], wraparound :2retard2troll:
```

## Functions
```swift
func f(b: bool, opts: SomethingOpts) : int {

}
unsafe func at(idx: size): int {
    return .array[idx];
}
```

## Interfaces
```swift
interface SomethingOptions {
    a: int;
    b: int;
}

func f(b: bool, opts: SomethingOpts) : int {

}
```

## Pointer types
All pointers are non-null.

```swift
*T          // pointer to a single T object
*[&]T       // pointer to array with compile-time detected size
[]T         // span<T>
[*]T        // T* but indexable (technically the same as C's T* but nonnull, and doesn't allow arithmetic)
*[N]T       // pointer to [N]T
[*][N]T     // indexable pointer to [N]T
```

## Convertions, casts

### Number casts
```swift
let int_: int;
let float_: float;

// Int to float
let a: float = as<float>(int_)!;         // runtime assertion if this would narrow
let b: float = narrow<float>(int_);     // equivalent of `float b = (float)int_;`

// Float to int
let c: int = as<int>(float_);           // runtime assertion if this would narrow
let d: int = floor<int>(float_);        // floor
let e: int = ceil<int>(float_);         // ceil
```

### Class hierarchy casts
```swift
let base: Base;
let derived: Derived;

// Base to derived
let derived: Derived = base;                            // error: base is not derived
let derived: Derived*? = down_cast<Derived>(&derived);  // ok, noop
let derived: Derived*? = down_cast<Derived>(&base);     // derived will be empty
let derived: Derived* = verify_cast<Derived>(&base);    // runtime assertion

// Derived to base
let base: Base = derived;               // error: implicit slicing
let base: Base = slice<Base>(derived);  // ok
```

## Dynamic allocation
No implicit dynamic allocation is allowed.

```swift
let allocator : GPA;

pointer[1][5]; // indexing

struct Vector<T> {
    storage: T*[];
}
```

## Smart pointers
```swift
struct Node {
    parent: Node?;
    children: Vector<OwnPtr<Node>>;
}
```

## Function objects
```swift
// Function object variable declaration
let on_click : func<(b: bool, opts: SomethingOpts): int>; // equivalent to std::function
```

### Lambda scope checking
```swift
let lambda: function...;
{
    let a = 0;
    lambda = [&a]() { // error: lambda has greater scope than captured variable
        return a
    };
}
lambda(); // UB
```

### Error handling
Rust-like
```swift
let a = fs.open("nonexistent.txt", Write);
```

```swift
func a() throws SystemError: int {
    let file = try fs.open("nonexistent.txt", Write);
    return file.read().to_int();
}

class Result<T> {
    public:
        operator is<U>() : bool // |+

    public {                    // |+
        operator is<U>() : bool {

        }
    }
}

func b() {
    // Version 1
    let maybe_file = a();
    if (maybe_file is SystemError(error)) {
        GUI::message_box(message());
    } else if (maybe_file is OtherError(error)) {
        panic("Unexpected error {}", e);
    } else (file) {

    }

    // Version 2
    let file = a() catch(e: SystemError) {
        GUI::message_box(e.message());
    } catch(e: OtherError) {
        panic("Unexpected error {}", e);
    }

    // Version 3
    try {
        let file = a();
    } catch(e: SystemError) {
        GUI::message_box(e.message());
    } catch(e: OtherError) {
        panic("Unexpected error {}", e);
    }
}
```

### Static reflection
```swift
struct Type {
    a: int;
    b: int;
    func do_something() {
        println("hello world! {}", a);
    }
}

for (let member of reflect.members<Type>()) {
    if (member is reflect.Method) {

    } else if (member is reflect.Field) {

    }
}
```

### Static type generation
```swift
struct Type = comptime {
    mut type : reflect.Type;
    type.add_field("test", reflect.BuiltinTypes.Int, 1234);
    type.add_method("do_something", []{
        println("hello world!");
    });
    return type;
}
```
