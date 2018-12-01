# MiniOOL

A mini object-oriented language.<br>
Created by Zachary Ferguson for Honors Programming Languages (Fall 2018).

## Building MiniOOL

MiniOOL can be built using the included make file. Run `make` to compile the
source code and run `make examples` to run the example programs. Optionally
`make docs` will build the documentation for the source code using `ocamldoc`.

### Dependancies
The only external dependency for MiniOOL is
[Menhir](http://gallium.inria.fr/~fpottier/menhir/) for parsing. Menhir can be
installed using [opam](http://opam.ocaml.org/doc/Install.html) using the
command `opam install menhir`.

## Using MiniOOL

To start an interactive session run the `./MiniOOL` from the project directory.

The optional command line arguments to MiniOOL are

```
Usage: MiniOOL [--verbose] [--no-logo] [-help|--help]
  --verbose Verbosly print steps of interpretation
  --no-logo Do not display the logo and information at start up
  -help  Display this list of options
  --help  Display this list of options
```

To run a program from a source file, `source.mini` use the command

```bash
printf "$(cat source.mini | tr -d '\n')\n" | ./MiniOOL --no-logo
```

### Commands:
 * `var x`: Declare a variable `x`.
 * `e₀(e₁)`: Call procedure evaluated from e₀ with an argument of `e₁` evaluated,
 procedures do not return a value explicitly and are not expressions.
 * `malloc(x)`: Allocate `x` on the heap, making it an object with unlimited fields.
 * `x = e`: Assign the value of the expression `e` to `x`.
 * `e₀.e₁ = e₂`: Assign the value of the expression `e₂` to the field `e₁` of
 variable `e₀`.
 * `skip`: Do nothing.
 * `{C₀; C₁; ...}`: Execute commands sequentially.
 * `while b C | while b do C`:
 While b evaluates to true execute the command C.
 * `if b C₀ else C₁ | if b then C₀ else C₁`: If b evaluates to true execute
 the command C₀ else execute C₁.
 * `{C₀ ||| C₁}`: Execute commands C₀ and C₁ in parallel.
 * `atom(C)`: Execute commands C without executing any other command.

### Expressions:
 * `(['A'-'Z'])(['a'-'z'] | ['A'-'Z'] | ['0'-'9'] | '_')*`: Fields
 * `[0-9]+`: Integers
 * `e₀ + e₁ | e₀ - e₁ | e₀ * e₁ | e₀ / e₁ | e₀ % e₁ | e₀ mod e₁ | -e₀`:
 Arithmetic operations
 * `null`: Null location
 * `(['a'-'z'])(['a'-'z'] | ['A'-'Z'] | ['0'-'9'] | '_')*`: Variables
 * `e₀.e₁`: Field access
 * `proc y: C`: Procedure declaration

### Boolean Expressions:
 * `true | false`: Boolean literals
 * `e₀ == e₁ | e₀ != e₁ | e₀ <> e₁ | e₀ < e₁ | e₀ <= e₁ | e₀ > e₁ | e₀ >= e₁`:
 Comparison operations
 * `not b | !b | b₀ && b₁ | b₀ and b₁ | b₀ || b₁ | b₀ or b₁`: Boolean operations

### Interpreter Directives:
 * `\exit` | `\quit`: Exit MiniOOL
 * `\help`: Display helpful information about and how to use MiniOOL.
 * `\clear`: Clear the stack and heap of all values (this will also reset the
     unique naming of variables).
 * `\verbose`: Verbosely display the steps of interpretation including the
 abstract syntax tree (same as the command line option --verbose).
 * `\quite`: The opposite of `\verbose`, turn off verbose information.


## Example Programs

Provided in the `examples/` directory are some example programs for MiniOOL. Run
all of these examples by using the command `make examples`.

### Factorial (`factorial.mini`)

This example program compute factorial of 17.

```js
var r;
var factorial;
factorial = proc i:
    if (i <= 0) {
        r = 1;
    } else {
        factorial(i - 1);
        r = i * r;
    };
factorial(17);
r = 355687428096000 - r
```

The resulting value of `r` is 0

### Object creation (`object_creation.mini`)

This show how to create and object and assign values to its fields.

```js
var x; malloc(x);
x.C = 0;
x.F = proc y:if y < 1 then x.R = x.C else x.F(y - 1);
x.F(2);
var r;
r = x.R;
```

The resulting value of `r` is 0.

### Currying Parameters (`curry.mini`)

In MiniOOL, procedures only have one formal parameter.  Although this may seem
a limitation to procedures, in fact a procedure can take multiple arguments by
using “currying”. While a procedure `sum(x, y)` is not directly possible it can
be achieved by defining a function `sum(x)` that “returns” a function
`foo(y) = x + y` that takes one argument and “returns” the sum of `x` and `y`.
The following MiniOOL program shows how to define and use such a function

```js
var r;
var sum;
sum = proc x: {
    r = proc y: {
        r = x + y;
    };
};
var x; x = 100;
var y; y = -100;
sum(x);
r(y);
```

The resulting value of `r` is 0.
