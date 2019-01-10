<a name="logo"/>
<p align="center">
<img src="logo.svg" alt="MiniOOL Logo" width="450px">
</p>

**MiniOOL** is a **mini** **o**bject-**o**riented **l**anguage. Written in OCaml,
this mini programming language features integer arithmetic, boolean operations,
control structures, recursive procedures, parallelism, and basic objects with
fields.

Created by Zachary Ferguson for CSCI-GA.3110: Honors Programming Languages
(Fall 2018) at New York University.

See `ferguson-zachary-MiniOOL-report.pdf` for a discussion of design decisions,
implementation details, and some examples covering the features of MiniOOL.

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

To start an interactive session run `./MiniOOL` from the project directory.

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
 * `\quiet`: The opposite of `\verbose`, turn off verbose information.


## Example Programs

Provided in the `examples/` directory are some example programs for MiniOOL. Run
all of these examples by using the command `make examples`.

### Factorial (`factorial.mini`)

This example program compute factorial of 17 by defining a recursive procedure
to compute factorial.

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

Objects are created by using the `malloc(x)` operator. All fields of an object
are implicitly declared as `null`. Fields are accessed using the `e₀.e₁`
operator where all fields start with an uppercase letter.

```js
var x; malloc(x);
x.C = 0;
x.F = proc y: if y < 1 then x.R = x.C else x.F(y - 1);
x.F(2);
var r;
r = x.R;
```

The resulting value of `r` is 0.

### Currying Parameters (`curry.mini`)

In MiniOOL, procedures only have one formal parameter.  Although this may seem
a limitation to procedures, in fact a procedure can take multiple arguments by
using “currying”. While a procedure `sum(x, y)` is not directly possible it can
be achieved by defining a procedure `sum(x)` that “returns” a procedure
`foo(y) = x + y` that takes one argument and “returns” the sum of `x` and `y`.
The following MiniOOL program shows how to define and use such a procedure:

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

The resulting value of `r` is 0. The value of `x` is accessible in
`r = proc y: {r = x + y;};` because it creates a closure containing the current
stack, which stores a location for the value of x.

#### Currying Unlimited Parameters (`curry_inf.mini`)

A procedure can be defined that takes an unlimited number of parameters. This is
done by defining a procedure that takes one argument and "returns" two values:
the intermediate results and a procedure to continue computations.

```js
var r;
malloc(r);
var sum;
sum = proc x: {
    r.Result = x;
    r.SumRest = proc y: {
        r.Result = r.Result + y;
    };
};

sum(0);
var i; i = 1;
while (i <= 100) {
    r.SumRest(i);
    i = i + 1
};
r = 5050 - r.Result
```

The resulting value of `r` is 0.

### Parallelism and Race Conditions (`race.mini`)

The `|||` operator allows two commands to be run in parallel, but can lead to
uncertain outcomes.

```js
var x; x = 10; var r; {if x == 10 then r = x * 2 else skip ||| x = 5}
```

Depending on the order of commands evaluated by the parallel operator the
resulting value of r can be:

* `r = null` if `x = 5` is run first resulting in `x == 10` being false and `r`
not being set
* `r = 10` if the check `x == 10` is evaluated first then the command `x = 5` is
evaluated and the command `r = x * 2` is evaluated last.
* `r = 20` if the check `x == 10` and command `r = x * 2` are evaluated before
the command `x = 5` is evaluated.

#### Atomic Commands (`atomic_race.mini`)

By using the `atom(C)` operator the enclosed commands, `C`, run without
interruption.

```js
var x; x = 10; var r; {atom(if x == 10 then r = x * 2 else skip) ||| x = 5}
```

Depending on the order of commands evaluated by the parallel operator the
resulting value of r can be:

* `r = null` if `x = 5` is run first resulting in `x == 10` being false and `r`
not being set
* `r = 20` if the command `atom(if x == 10 then r = x * 2 else skip)` is evaluated before
the command `x = 5`.

### Implicit Global Variables with Dynamic Scope (`dynamic_globals.mini`)

Any undeclared variable is implicitly declared as global with dynamic scope.

```js
var r; var p; p = proc y: r = y+h; h=2; p(4);
```

The resulting value of r is 6 in this example. The variable `h` is never
declared, so it is treated as an implicit global variable. Although, h is
initialized after the procedure `p` is declared, the value of `h` is used
inside of `p` dynamically. After renaming variable to be unique the program is

```js
var r₀; var p₀; p₀ = proc y₀: r₀ = y₀+h₋₁; h₋₁=2; p₀(4);
```

## Source files

All of the source code for MiniOOL is written in OCaml and can be found in the
`src` directory.

* `MiniOOL.ml`: Main cmd-line interface for MiniOOL
* `lexer.mll`: Lexer tokens and regular expressions for MiniOOL.
* `parser.mly`: Menhir parser for MiniOOL. Takes the tokens from the lexer and
builds an abstract syntax tree for of the input program. Runs the input program
by checking the scope, uniquely renaming vairables, and evaluating the commands.
* `abstractSyntaxTree.ml`: Abstract syntax tree types for MiniOOL.
* `abstractSyntaxTreeString.ml`: Create a string of a tree of the abstract
syntax tree types for MiniOOL.
* `programString.ml`: Create a string of of the abstract syntax tree types for
MiniOOL.
* `staticSemantics.ml`: Static semantics for MiniOOL. Checks if an identifier
is in scope.
* `semanticDomains.ml`: Type definitions for the semantic domains of MiniOOL.
* `operationalSemantics.ml`: Evaluate the abstract syntax tree of a MiniOOL
program.
* `flags.ml`: Global flags for different options.
* `utils.ml`: Utilities for MiniOOL
