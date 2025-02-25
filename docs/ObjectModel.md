# Inspirations

- Rust for mutability
- High level languages

# Invariants

- (I1) `let` binding never changes its value.
- (I2) `mut` binding never changes its value except through an `LValue`

# Type system

- Static typing
- You can have types like `int in range 0..10`, `int - {0}` etc.
- Primitive types
  - Always semantically copied except when explicitly referenced
    - Semantically == compiler may choose to reference it anyway as optimization
  - Primitive types are: numbers, structs, arrays of primitive types
  - They still can have destructors
  - Destructor makes type NON COPYABLE e.g strings
- Identity types
  - Cannot be copied or compared (by default)
  - Therefore, they cannot be passed "by value" (`T` is not allowed, only `&T` or `weak &T`)
  - They can be identified e.g `obj1 == obj2` tells you if this is the SAME object.

# Reference types

- Raw reference
  - Checked for lifetimes
- Strong reference
  - Can be taken only from identity type
- Weak reference
  - Can be taken only from identity type

# Operations

## Assignment

- `lhs = rhs`:
  - `lhs` must be `LValue`
  - if `rhs` is primitive:
- `lhs = `:
