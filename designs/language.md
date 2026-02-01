# SkyLow Language Design

A low-level programming language in the SkyHigh style: customizable syntax via `syntax` declarations, indentation-based blocks, and expression-oriented design. Unlike SkyHigh, SkyLow targets low-level execution without garbage collection or closures.

## Design Principles

1. **No hidden allocations**: Memory allocation is always explicit
2. **No closures**: Lambdas cannot capture variables from enclosing scope
3. **Predictable layout**: Data types have well-defined memory representation
4. **Nested definitions**: Types and functions can be defined locally within any scope
5. **Namespaces everywhere**: Namespaces, structs, and enums all act as namespaces

## Type System

### Primitive Types

```
I8        # 8-bit signed integer
I16       # 16-bit signed integer
I32       # 32-bit signed integer
I64       # 64-bit signed integer
Int       # alias for I64

U8        # 8-bit unsigned integer
U16       # 16-bit unsigned integer
U32       # 32-bit unsigned integer
U64       # 64-bit unsigned integer
Uint      # alias for U64

F32       # 32-bit IEEE 754 float
F64       # 64-bit IEEE 754 float
Float     # alias for F64

Bool      # boolean (true/false)
Unit      # zero-sized type, single value ()
Never     # bottom type, no values (for diverging functions)
```

### Pointer Types

```
*T        # pointer to T (nullable)
**T       # pointer to pointer to T
```

Pointers are nullable, always readable and writable. No safety guarantees - you can shoot yourself in the foot.

**Operators:**

| Operator | Description |
|----------|-------------|
| `&x` | Take address of `x`, returns `*T` |
| `*p` | Dereference pointer `p` |

```
let x = 42
let p: *Int = &x        # take address
let val = *p            # dereference, val == 42
*p = 100                # write through pointer

# Nested pointers
let pp: **Int = &p      # pointer to pointer
**pp = 200              # write through both levels

# Danger: nothing stops you from:
let bad: *Int = null
# let boom = *bad       # segfault at runtime
```

### Slices

```
T[]             # slice - syntactic sugar for Slice[T]
```

A slice is just a struct with a pointer and length:

```
struct Slice[T]:
  ptr: *T
  len: Uint
```

The `T[]` notation is sugar for `Slice[T]`. There are no fixed-size array types - use pointers like in C.

### Tuple Types

```
(T, U)          # pair
(T, U, V)       # triple
...
```

### Function Types

```
(T) -> U                      # function pointer taking T, returning U
(T, U) -> V                   # function pointer taking T and U
() -> Unit                    # function pointer taking nothing, returning Unit
(T) -> Never                  # diverging function
```

Function types represent non-capturing function pointers only.

## Structs

Structs group related data with a defined memory layout.

```
struct Point:
  x: Float
  y: Float

struct Rect:
  origin: Point
  size: Size
```

### Generic Structs

```
struct Pair[A, B]:
  first: A
  second: B

struct Buffer[T]:
  data: *T
  len: Uint
  cap: Uint
```

### Struct Literals

```
let p = Point(x: 1.0, y: 2.0)
let p2 = Point(3.0, 4.0)

# Partial initialization - omitted fields use their defaults
let r = Rect(origin: Point(0.0, 0.0))  # size gets its default
```

### Field Access and the `.` Operator

The `.` operator is used for:
1. Accessing struct fields
2. Qualifying names through namespaces
3. Method-like call syntax (syntactic sugar)

```
p.x                   # field access
rect.origin.x         # nested field access
math.sin(x)           # namespace qualification
Color.Red             # enum variant access
Vec2.new(1.0, 2.0)    # associated function in struct namespace
```

### Method-like Call Syntax

When `a` has type `A`, the expression `a.b(args)` is syntactic sugar for `A.b(a, args)` if such a function exists. This provides a familiar method-call syntax without actual methods.

The receiver is automatically adjusted to match the first parameter:
- If the function expects `*A` and the receiver is `A`, the address is taken automatically
- If the function expects `A` and the receiver is `A`, the value is passed directly

```
struct Vec2:
  x: Float
  y: Float

  fn length(v: *Vec2) -> Float:
    sqrt(v.x * v.x + v.y * v.y)

  fn add(a: *Vec2, b: *Vec2) -> Vec2:
    Vec2(a.x + b.x, a.y + b.y)

  fn scale(v: Vec2, factor: Float) -> Vec2:
    Vec2(v.x * factor, v.y * factor)

let v = Vec2(3.0, 4.0)
let other = Vec2(1.0, 2.0)

# Pointer parameter: address taken automatically
v.length()            # becomes Vec2.length(&v)
v.add(&other)         # becomes Vec2.add(&v, &other)

# Value parameter: passed directly
v.scale(2.0)          # becomes Vec2.scale(v, 2.0)
```

Note: The lookup is based on the receiver's type. `v.scale(2.0)` looks for `Vec2.scale`, not for any `scale` in scope.

## Enums

Enums define a set of variants within a namespace. Variants can optionally carry data (tagged union / sum type).

### Simple Enums

Variants without data are assigned integer values:

```
enum Color:
  Red
  Green
  Blue

enum Flags:
  None = 0
  Read = 1
  Write = 2
  Execute = 4
```

Enum values are accessed through the enum namespace: `Color.Red`, `Flags.Write`.

The underlying discriminant type defaults to `I32` but can be specified:

```
enum U8 SmallColor:
  Red
  Green
  Blue
```

### Enums with Data (Tagged Unions)

Variants can carry data, making the enum a tagged union / sum type:

```
enum Option[T]:
  Some(T)
  None

enum Result[T, E]:
  Ok(T)
  Err(E)

enum Shape:
  Circle(radius: Float)
  Rectangle(width: Float, height: Float)
  Point
```

### Variant Construction

Variants must always be qualified with their enum name:

```
let x: Option[Int] = Option.Some(42)
let y: Option[Int] = Option.None
```

Use `open` to bring variants into scope:

```
open Option
let x: Option[Int] = Some(42)
let y: Option[Int] = None
```

### Pattern Matching

When the type is known from context, variants can be unqualified. The compiler checks the expected type to resolve the qualified reference:

```
match opt:
  Some(x) => x * 2
  None => 0

match shape:
  Circle(r) => 3.14159 * r * r
  Rectangle(w, h) => w * h
  Point => 0.0
```

## Nested Definitions

Types and functions can be defined inside any scope and are local to that scope.

### Nested Structs

```
fn process_data(input: U8[]) -> Int:
  # Local struct only visible within this function
  struct Header:
    magic: U32
    version: U16
    size: U32

  let header = *(input.ptr as *Header)
  header.size as Int
```

### Nested Enums

```
fn parse_token(s: *U8, len: Uint) -> Token:
  enum Kind:
    Number = 0
    Ident = 1
    Symbol = 2

  # Kind is local to this function
  let kind = classify(s)
  match kind:
    Kind.Number => Token.Number(parse_int(s))
    Kind.Ident => Token.Ident(s)
    Kind.Symbol => Token.Symbol(s)
```

### Nested Functions

```
fn sort(arr: *Int, len: Uint):
  fn swap(a: *Int, b: *Int):
    let tmp = *a
    *a = *b
    *b = tmp

  fn partition(arr: *Int, lo: Int, hi: Int) -> Int:
    # ... uses swap
    0

  fn quicksort(arr: *Int, lo: Int, hi: Int):
    if lo < hi:
      let p = partition(arr, lo, hi)
      quicksort(arr, lo, p - 1)
      quicksort(arr, p + 1, hi)

  quicksort(arr, 0, len as Int - 1)
```

Note: Nested functions cannot capture variables. They are equivalent to module-level functions with restricted visibility.

## Namespaces

Namespaces, structs, and enums all act as namespaces. Names are accessed with dot notation.

### Namespace Definition

```
namespace math:
  const PI: Float = 3.14159265359

  fn sin(x: Float) -> Float:
    # implementation
    0.0

  fn cos(x: Float) -> Float:
    # implementation
    0.0

# Access via namespace
let y = math.sin(math.PI / 2.0)
```

### Structs as Namespaces

Structs can contain associated functions and nested types.

```
struct Person:
  name: *U8
  name_len: Uint
  age: Int
  address: Person.Address   # nested struct used as field type

  struct Address:
    street: *U8
    street_len: Uint
    city: *U8
    city_len: Uint
    zip: U32

  fn new(name: *U8, name_len: Uint, age: Int, addr: Person.Address) -> Person:
    Person(name, name_len, age, addr)

  fn is_adult(p: *Person) -> Bool:
    p.age >= 18

# Usage
let addr = Person.Address(street, street_len, city, city_len, 12345)
let bob = Person.new(name, name_len, 30, addr)
let adult = Person.is_adult(&bob)

# Or with dot notation
let adult = bob.is_adult()

# Or with piping
let adult = &bob | Person.is_adult
```

### Enums as Namespaces

Enum variants are accessed through their type namespace.

```
let c = Color.Red
let opt = Option.Some(42)
```

Enums can also contain associated functions:

```
enum Option[T]:
  Some(T)
  None

  fn unwrap_or(opt: *Option[T], default: T) -> T:
    match *opt:
      Some(x) => x
      None => default

  fn map[U](opt: *Option[T], f: (T) -> U) -> Option[U]:
    match *opt:
      Some(x) => Some(f(x))
      None => None

# Usage
let x = Option.unwrap_or(&opt, 0)

# Or with dot notation
let x = opt.unwrap_or(0)

# Or with piping
let x = &opt | Option.unwrap_or(_, 0)
```

## The `open` Statement

The `open` statement brings names from a namespace into the current scope, allowing unqualified access.

### Basic Open

```
fn draw_shapes():
  open Color

  # Now Red, Green, Blue are directly accessible
  let bg = Red
  let fg = Blue
```

### Selective Open

```
fn render():
  open math { sin, cos, PI }

  # Only sin, cos, PI are imported
  let x = cos(PI / 4.0)
```

### Open with Rename

```
fn compute():
  open math { sin as s, cos as c }

  let x = c(angle) * radius
  let y = s(angle) * radius
```

### Open All

```
fn use_vectors():
  open Vec2.*

  # All names from Vec2 namespace available
  let v = new(1.0, 2.0)  # Vec2.new
  let z = zero()         # Vec2.zero
```

### Scoped Open

`open` respects lexical scope:

```
fn outer():
  let x = Color.Red

  do:
    open Color
    let y = Green  # OK, Color is open here

  let z = Green    # ERROR: Color not open here, must use Color.Green
```

## Functions

### Function Definitions

```
fn add(x: Int, y: Int) -> Int:
  x + y

fn greet(name: *U8, len: Uint):
  print("Hello, ")
  println(name, len)
```

### Generic Functions

```
fn swap[T](a: *T, b: *T):
  let tmp = *a
  *a = *b
  *b = tmp

fn max[T: Ord](a: T, b: T) -> T:
  if a > b:
    a
  else:
    b
```

### External Functions

```
extern "C" fn malloc(size: Uint) -> *U8
extern "C" fn free(ptr: *U8)
extern "C" fn memcpy(dst: *U8, src: *U8, n: Uint) -> *U8
```

## Non-Capturing Lambdas

Lambdas define anonymous functions but cannot capture variables from the enclosing scope.

```
let double = \x: Int -> x * 2
let add = \x: Int, y: Int -> x + y

# Type annotation on result
let parse: (*U8, Uint) -> Int = \s, len -> str_to_int(s, len)
```

Attempting to capture is a compile error:

```
fn bad():
  let multiplier = 10
  let scale = \x: Int -> x * multiplier  # ERROR: cannot capture 'multiplier'
```

### Lambda Type Inference

When a lambda is passed to a function with a known signature, parameter types can be inferred:

```
fn map[T, U](arr: T[], f: (T) -> U) -> Vec[U]:
  # ...

let numbers = [1, 2, 3]
let doubled = map(numbers, \x -> x * 2)  # x inferred as Int
```

## Memory Management

### Stack Allocation

Local variables are stack-allocated by default:

```
fn example():
  let x: Int = 42           # on stack
  let p = Point(1.0, 2.0)   # struct on stack
```

For arrays, allocate on the heap or use `stackalloc`:

```
fn example():
  let arr: *Int = stackalloc[Int](10)  # compiler intrinsic, on stack
  # arr is valid until function returns
```

### Heap Allocation

Explicit allocation functions for heap memory:

```
fn alloc[T]() -> *T
fn alloc_array[T](count: Uint) -> *T
fn dealloc[T](ptr: *T)
fn dealloc_array[T](ptr: *T, count: Uint)
```

Usage:

```
fn example():
  # Allocate single value
  let p: *Point = alloc[Point]()
  *p = Point(1.0, 2.0)

  # Allocate array
  let arr: *Int = alloc_array[Int](100)

  # Use...

  # Must deallocate
  dealloc(p)
  dealloc_array(arr, 100)
```

## Typeclasses

Typeclasses define interfaces with explicit type parameters. They can have multiple parameters like in Lean 4.

```
class Eq[T]:
  fn eq(a: *T, b: *T) -> Bool

class Ord[T] extends Eq[T]:
  fn cmp(a: *T, b: *T) -> Ordering

class Clone[T]:
  fn clone(x: *T) -> T

class Drop[T]:
  fn drop(x: *T)

# Multi-parameter typeclasses
class Add[A, B, C]:
  fn add(a: A, b: B) -> C

class Into[From, To]:
  fn into(x: From) -> To
```

### Instances

```
instance Eq[Point]:
  fn eq(a: *Point, b: *Point) -> Bool:
    a.x == b.x && a.y == b.y

instance Clone[Point]:
  fn clone(p: *Point) -> Point:
    Point(p.x, p.y)

# Multi-parameter instance
instance Add[Vec2, Vec2, Vec2]:
  fn add(a: Vec2, b: Vec2) -> Vec2:
    Vec2(a.x + b.x, a.y + b.y)

instance Into[Int, Float]:
  fn into(x: Int) -> Float:
    x as Float
```

### Generic Constraints

For single-parameter typeclasses, use the shorthand `[T: Eq]` syntax:

```
fn find[T: Eq](arr: *T, len: Uint, value: *T) -> Option[Uint]:
  let mut i: Uint = 0
  while i < len:
    if Eq.eq(arr + i, value):
      return Option.Some(i)
    i = i + 1
  Option.None

fn max[T: Ord](a: T, b: T) -> T:
  if Ord.cmp(&a, &b) == Ordering.Greater:
    a
  else:
    b
```

For multi-parameter typeclasses, use `where`:

```
fn sum[T](arr: *T, len: Uint) -> T where Add[T, T, T]:
  # ...

fn convert[A, B](x: A) -> B where Into[A, B]:
  Into.into(x)
```

## Piping Operator

The `|` operator pipes a value into a function, passing it as the first argument:

```
# These are equivalent:
Vec2.length(&v)
&v | Vec2.length

# Chaining multiple operations
data
  | parse
  | validate
  | transform
  | serialize
```

For functions with multiple arguments, use `_` as a placeholder for the piped value:

```
# These are equivalent:
Option.unwrap_or(&opt, default_value)
&opt | Option.unwrap_or(_, default_value)

# Piping into second argument
value | some_func(first_arg, _)
```

## Control Flow

### If Expressions

```
let max =
  if a > b:
    a
  else:
    b

# Chained
let sign =
  if x < 0:
    -1
  else if x > 0:
    1
  else:
    0
```

### Match Expressions

```
# Enum variants can omit the type qualifier when the matched type is known
let name = match color:
  Red => "red"
  Green => "green"
  Blue => "blue"

# With guards
match value:
  x if x < 0 => "negative"
  0 => "zero"
  x => "positive"
```

### Loops

```
# While loop
while condition:
  body

# Loop with break
loop:
  if done:
    break value
  continue

# For loop over iterators
for item in collection:
  process(item)

# For loop with index
for i in 0..n:
  arr[i] = i * 2
```

### Early Return

```
fn find_first_negative(arr: *Int, len: Uint) -> Option[Int]:
  for i in 0..len:
    if arr[i] < 0:
      return Some(arr[i])
  None
```

## Constants and Statics

```
# Compile-time constant
const MAX_SIZE: Uint = 1024
const PI: Float = 3.14159265359

# Runtime static (global variable, always mutable)
static COUNTER: Int = 0

fn increment():
  COUNTER = COUNTER + 1
```

## Namespaces and Imports

### Nested Namespaces

```
namespace collections:
  struct Vec[T]:
    ptr: *T
    len: Uint
    cap: Uint

  fn new_vec[T]() -> Vec[T]:
    Vec(ptr: null, len: 0, cap: 0)

  namespace internal:
    fn grow[T](v: *Vec[T]):
      # ...
```

### Imports

```
# Import entire module
import collections

# Selective import
import collections { Vec, new_vec }

# Import with rename
import collections { Vec as Vector }

# Import from submodule
import collections.internal { grow }
```

### Visibility

By default, all items are private to their namespace. Use `pub` for public visibility:

```
namespace math:
  pub const PI: Float = 3.14159

  pub fn sin(x: Float) -> Float:
    # ...
    0.0

  # Private helper
  fn normalize_angle(x: Float) -> Float:
    # ...
    0.0
```

## Attributes

Attributes provide metadata for declarations:

```
@inline
fn small_helper(x: Int) -> Int:
  x + 1

@repr(C)
struct CCompatible:
  field1: I32
  field2: I32

@packed
struct Compact:
  a: U8
  b: U32
  c: U8

@align(16)
struct Vec4:
  x: Float
  y: Float
  z: Float
  w: Float
```

## Tests

SkyLow includes built-in support for test declarations and assertions.

### Test Declarations

```
test arithmetic works as expected:
  assert(2 + 2 == 4)
  assert(2 + 3*2 == 8)
  assert((1 + 2) * (3 + 4) == 21)
  assert(10 / 2 - 3 == 2)
```

A test declaration starts with `test` followed by a descriptive name (any characters except `:` and newline), then a colon and an indented body of statements.

### Assertions

```
assert(expr)
```

The `assert` statement evaluates `expr` and fails the test if the result is falsy (zero). The expression is typically a comparison.

### Comparison Operators

| Operator | Description |
|----------|-------------|
| `==` | Equality comparison, returns 1 if equal, 0 otherwise |
| `!=` | Inequality comparison, returns 1 if not equal, 0 otherwise |
| `<` | Less than |
| `<=` | Less than or equal |
| `>` | Greater than |
| `>=` | Greater than or equal |

Comparison operators have lower precedence than arithmetic operators:

```
2 + 3 == 5      # parsed as (2 + 3) == 5
```

## Example Program

```
namespace geometry:
  pub struct Point:
    x: Float
    y: Float

    pub fn new(x: Float, y: Float) -> Point:
      Point(x, y)

    pub fn origin() -> Point:
      Point(0.0, 0.0)

    pub fn distance(a: *Point, b: *Point) -> Float:
      let dx = a.x - b.x
      let dy = a.y - b.y
      sqrt(dx * dx + dy * dy)

  pub enum Shape:
    Circle(center: Point, radius: Float)
    Rectangle(origin: Point, width: Float, height: Float)

    pub fn area(s: *Shape) -> Float:
      match *s:
        Circle(_, r) => 3.14159 * r * r
        Rectangle(_, w, h) => w * h

fn main():
  open geometry
  open Point { new, origin }

  let p1 = new(3.0, 4.0)
  let p2 = origin()
  let dist = Point.distance(&p1, &p2)

  let shape = Shape.Circle(center: p1, radius: 5.0)
  let area = &shape | Shape.area

  # Local type for this computation
  struct Result:
    distance: Float
    area: Float

  let result = Result(distance: dist, area: area)
  result
```
