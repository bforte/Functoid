# Functoid

One instruction pointer, a direction, a stack for the command-line arguments from
which you can only pop, a mutable 2D source code and a single expression that you
can apply new arguments to.

In case you don't have a Haskell compiler, you can try `functoid` online at
[Try it online!][functoid-tio]
which is provided by [DennisMitchell](https://github.com/DennisMitchell).


You can find the list of all commands [here](#commands).


## Interpreter

    usage: functoid [OPTIONS] [-e expr | -t expr | file] [INPUTS]
      -e  --expression  use command-line argument as source
      -f  --force       always force evaluation
      -n  --no-clear    don't clear the current lambda term when printing
      -v  --verbose     print steps taken
      -q  --quiet       don't print the final lambda term
      -t  --transform   transform lambda expression to SKIBCW
      -x  --exit        automatically exit on first print statement


## Installation

Make sure you've got either `cabal` or `stack` installed, then clone the repo:

```
$ git clone git://github.com/bforte/Functoid.git
$ cabal install
```


#### If you're using `cabal`

Simply install it:

```
$ cabal install
```

#### If you're using `stack`

Setup the environment and then install it:

```
$ stack setup
$ stack install
```


## Introduction

There's no reason to give a formal definition for the [lambda
calculus][LC-wiki] here, instead I will showcase some of the internal
definitions that `functoid` uses and how they can be used to do arithmetic. If
you're not familiar with DeBruijn notation, you should probably check it out
(for example [here][DB-wiki]) because this explanation will make use of it -
however for clarity *xN* will be used instead of *N*.

Internally `functoid` has no types to represent Booleans, numbers, characters
or even strings. Any expression is defined in terms of lambda terms, please
refer to the [*Commands*](#commands) section for how logic is defined.
Characters are just another representation of integers (modulo 128 and
converted to ASCII), numbers are defined as Church numerals:

- Zero is: *λλx1*
- The successor function is: λλλ(x2 (x3 x2 x1))

Now every other number *N* gets constructed by applying *N* times the successor
function. For example 2 would be *(succ (succ 0))* which can be expanded and
simplified:

1. *succ (λλλ(x2 (x3 x2 x1)) λλx1)*
2. *succ λλ(x2 (λλx1 x2 x1))*
3. *succ λλ(x2 (λx1 x1))*
4. *succ λλ(x2 x1)*
5. *λλλ(x2 (x3 x2 x1)) λλ(x2 x1)*
6. *λλ(x2 (λλ(x2 x1) x2 x1))*
7. *λλ(x2 (λ(x3 x1) x1))*
8. *λλ(x2 (x2 x1))*

As you can see in step 4 we arrive at an expression *(succ X)* and by
definition of the number 2 we can infer that *1 == λλ(x2 x1)*, probably you
notice the pattern that *N* is *λλ(x2 (…(x2 x1)…))* with *x2* repeated exactly
*N* times.

All built-ins that work with numbers in `functoid` work like this and this is
the reason that programs can be quite slow, it also implies that there are no
negative numbers by default. Although you can simply define them how you want
and work with your own definitions.


## The first program

The interpreter internally keeps track of one single function and consecutively
applies new expressions to that function. Apart from that it stores the current
position and travel direction that the pointer moves next. At the beginning the
internal function is the identity function (*λx1*), the position is `(0,0)`
(top-left) and the pointer will move to the right.

So the program `1@` would simply apply `1` (as we saw in the previous section
this is *λλ(x2 x1)*) to the identity function which should give us `1` back,
then it moves one to the right and will terminate (`@` terminates the program).

Let us run that program (`-v` flag prints the steps and `-e` let's you specify
the source via command-line):

```
$ functoid -ve "1@"
(0,0) '1' [R]
(1,0) '@' [R]

Final expression: λλ(x2 x1)    [Church numeral: 1]
```

At the end of the program the current function is printed to *stderr* and if it
evaluates to a Boolean (see [*Commands*](#commands) for the definition used) or
Church numeral that will be displayed.

Of course you can take user-input as well: When the interpreter is invoked all
command-line arguments get parsed (you can input either lambda-terms or
shortcuts for numbers) and pushed to a stack. The command `$` will pop one
argument and apply it to the current function (note how `v>^<` alter the flow
of the program unconditionally):

```
$ cat succ.f
v@ <
>+$^
$ functoid examples/succ.f 1

Final expression: λλλ(x2 (x3 x2 x1))
```

It's not surprising that this returns the successor function, since the only
functions that get applied are `+` and `$` (which evaluates to *1*).


## Hello, World!

You might ask how you would use numbers larger than `9` without doing a lot of
additions, that's where `"` comes into play. This special control character
delimits multi-digit numbers which get applied once they're read, characters
`<>^v@?` still work inside delimited numbers and everything else gets converted
to their ASCII code and used as base10 "digit" - for example `"abc"` which has
corresponding codes of `97,98,99` is converted to `10779`.

One thing to note is that by default (`-n` flag disables this) the commands
`;,.` will clear the current function by overriding it with the identity
function and print the current function (only if it evaluates to the
corresponding "type").  Here's a simple way to write a "Hello, World!" which
makes use of this ability to evaluate multiple functions sequentially:

```
$ functoid -qe '"H","e","l","l","o",","," ","W","o","r","l","d","!",@'
Hello, World!
```


## Lambda Calculus REPL

The fact that everything internally is handled as lambda terms allows us to
code up a REPL for the lambda calculi in just three characters:

```
$ cat examples/lc-repl.f
~:p
```

This demonstrates how user input (either `~` or by command-line arguments) are
parsed and for the first time we see how the pointer simply wraps around
whenever it would move out of the source code. The character `~` asks the user
for input (without printing to the screen), parses it* (you can either use `\`
or `λ` as lambda) and applies it to the current term:

For clarity pressing <kbd>Enter</kbd> is highlighted with `⏎`:

```
$ functoid examples/lc-repl.f
\\\(x2 (x3 x2 x1)) ⏎
λλλ(x2 (x3 x2 x1))
1 ⏎
λλ(x2 x1)
\\\(x2 (x3 x2 x1)) (\\x2 x1) ⏎
λλ(x2 (x3 x1))
^C
```

**Note:** The spaces between for example `x2` and `(` are mandatory, they
denote function application.

You can also run it with the `-n` flag such that `:` won't clear the current
expression, this allows to successively apply the lines:

```
$ functoid -n lambda-repl.f
\\(x2 (x2 x1)) ⏎
λλ(x2 (x2 x1))
\\\(x2 (x3 x2 x1)) ⏎
λλλ(x2 (x2 (x3 x2 x1)))
1 ⏎
λλ(x2 (x2 (x2 x1)))
^C
```

<sub>* It allows all commands that define an expression, eg. `1`,`T`,`S` etc.</sub>


## Laziness

By default `functoid` is lazy which means it won't evaluate (*β*-reduce) the
sometimes huge expression which is good. Not only would it be very inefficient
but some expressions don't simplify to a stable normal form. Let's try this:

```
functoid -e "WWW@"

Final expression: ^C
```

It won't terminate and you'll have to kill it with <kbd>Ctrl</kbd>+<kbd>C</kbd>
(that's the `^C` you can see). The character `r` will override the current
expression with the identity function, so let's do that before terminating and
see what happens:

```
functoid -e "WWWr@"

Final expression: λx1
```

This time it terminates, that's because `functoid` only ever evaluates stuff if
it really needs to (in fact try running the first version with the `-q` flag
and see what happens). However if you don't like this behaviour you can force
evaluation at each step with the `-f` flag - meaning `functoid -qfe "WWWr@"`
wont' terminate.


## Control flow

Though there's not really a need for control flow other than recursion, there's
still the possibility of modifying the direction of the instruction pointer
conditionally like in Befunge. The reflectors `|` and `_` force evaluation of
the current term and iff it evaluates to *false* (this is the same as *0*) to
down (for `|`) and right (for `_`) and to the opposite direction in the other
case.

Let's use this behaviour to program a truth machine - meaning a program that
takes a Boolean and prints `0` if it is *false* and otherwise produces an
infinite stream of `1`s:

    $ cat examples/truth-machine.f
    $_.@v
    p.1r<
    $ functoid -q examples/truth-machine.f F
    0
    $ functoid -q examples/truth-machine.f T
    1
    1
    1
    ⋮

**Explanation:** First the program applies `$` the command-line argument, moves
on to the next character `_` and this will happen:

- if the argument was *false* the pointer gets set to right, `.` prints the
  current expression (*false* as an integer is *0*) and aborts with `@`.
- in the other case it gets set to left and wraps around, follows `v` and `<`,
  resets the expression with `r`, applies `1`, prints it with `.`, prints a
  new-line with `p`. And this will continue because it wraps around.

As you probably noticed `_` (and it's the same with `|`) didn't reset the
current expression, this is helpful because it allows to conditionally do
something in the case of *0* and otherwise follow a different flow which could
for example be used as an alternative for the base case of some kind of
recursion.


## Modifying the source

OK, let's be honest the previous example for a truth machine was kind of boring
in the sense that this wasn't functional at all. Let us explore the functional
way of modifying the source at run-time and introduce some other concepts of
`functoid`, for this we do the same as in the previous example but in case of
*false* input, we'll simply exit without printing anything:

    $ cat examples/semi-truth-machine.f
    #v%"19"0(i[I$"64")f v
     >r1.p             >>

First the instruction `#` *bridges* the next one, so the program applies `%`
next.  This function expects 3 arguments, `x` `y` and `c` and once it gets
evaluated (except inside nested functions) modifies the character at position
`(x,y)` to `c`: In our case `x` will be 19, `y` will be 0 (that's the ` `
character between `f` and `v`) and `c` will be whatever the nested expression
inside the parentheses will be:

This expression first checks with `i` whether the argument `$` is *true* and in
that case it will evaluate to `[` (*pred* - predecessor) and to `I` (identity)
otherwise. It then applies 64 to one of these functions, evaluating to 63 for
*true* and *64* else which corresponds to characters `?` and `@` respectively.

By forcing evaluation of the term with `f` the next character will exit or set
the direction randomly to either one of the 4 directions. The rest of the
program follows the same idea as in the previous example.

**Note**: If we wouldn't force evaluation, the modification wouldn't get caught
and the program would simply follow `v`,`>` and print out `1`s in any case!


## Recursion combinators

Now suppose we want to check if a Church numeral is even or odd, in a
high-level language this could be described by a recursive function like this:

```haskell
even x
  | 1 >= x    = 0 == x
  | otherwise = even (x - 2)
```

But this requires a named function such that it is able to recursively call
itself, the standard way to solve in lambda calculus is to use the
[*Y*-combinator][y-combinator] (in Haskell this is `fix`) and rewrite `even` as
follows:

```haskell
rec f x
  | 1 >= x    = 0 == x
  | otherwise = f (x - 2)

even = fix rec
```

Because this can get quite verbose really fast and recursion is a very
important concept `functoid` currently provides three helper functions `x`,`y`
and `z` for defining functions like `rec`, here's a high-level definition of
`x`:

```haskell
functoid_x baseP baseF recF f x
  | baseP x   = baseF x
  | otherwise = recF f x
```

So if we apply `baseP`,`baseF` and `recF` to the `x` combinator we get a
exactly a function that we can use with `Y` and `even` would simply become:

```haskell
even = fix $ functoid_x (\x-> 1 >= x) (\x-> x == 0) (\f x-> f (x - 2))
```

In `functoid` the base predicate could be `G1`, the base function is simply `Z`
and the recursive function `CB(2[)` (or `BBCB2[` without parentheses) which
gives us the expression `x(G1)Z(BBCB2[)` for `rec`.  Now we simply need to
apply this to `Y` and have a function to check if a number is even (we can drop
the `()` around `G1` by using `B` once again):

    $ functoid -qe "Y(BxG1Z(BBCB2[))$;@" 4
    True
    $ functoid -qe "Y(BxG1Z(BBCB2[))$;@" 23
    False

**Note:** The functions `y` and `z` work very similar to `x` except that their
combinators `baseP`,`baseF` and `recF` all expect additional arguments.


## Commands

At the moment there is no shortage of characters and thus no reason not to have
multiple characters with the same meaning, for example `*` and `B` are the same -
this allows programs to be more expressive.

This is the full list of all commands that `functoid` currently knows each with
a description and possibly the lambda term that gets applied to the current
function.


| Character    | Description                          | Lambda term  |
|:------------:|--------------------------------------|--------------|
|    `@`       | end execution                        |              |
|    `E`       | functional version of `@`            | `@`          |
|    `<`       | set direction left                   |              |
|    `>`       | set direction right                  |              |
|    `^`       | set direction up                     |              |
|    `v`       | set direction down                   |              |
|    `?`       | set random direction                 |              |
|    `_`       | if term is 0 -> set direction right; else left | |
|    `\|`      | if term is 0 -> set direction down; else up    | |
|    `#`       | jump instruction                     |              |
|    `$`       | pop & apply argument                 |              |
|    `~`       | ask user for input & apply *         |              |
|    `:`       | output value current lambda term     |              |
|    `;`       | output value as Boolean              |              |
|    `,`       | output value as ASCII (mod 128) char |              |
|    `.`       | output value as number               |              |
|    `p`       | print newline                        |              |
|    `f`       | force evaluation                     |              |
|    `r`       | replace current expression with id   | set to *λx1* |
|    `R`       | functional version of `r`            | `r`          |
|    `%`       | *modify x3 x2 x1* -> set *(x3,x2)* to *x1* | *λλλ[x3,x2,x1]* |
|    `"`       | number delimiter                     |              |
|   `(…)`      | apply *…* to current term            |              |
|   `)…(`      | apply current term to *…*            |              |
|              |                                      |              |
|    `B`       | *B*-combinator                       | *λλλ(x3 (x2 x1))* |
|    `C`       | *C*-combinator                       | *λλλ(x3 x1 x2)* |
|    `I`       | *I*-combinator                       | *λx1*        |
|    `K`       | *K*-combinator                       | *λλx2*       |
|    `O`       | *ω*-combinator                       | *λ(x1 x1)*     |
|    `S`       | *S*-combinator                       | *λλλ(x3 x1 (x2 x1))* |
|    `U`       | *U*-combinator                       | *λλ(x1 (x2 x2 x1))* |
|    `W`       | *W*-combinator                       | *λλ(x2 x1 x1)* |
|    `Y`       | *Y*-combinator                       | *λ(λ(x2 (x1 x1)) λ(x2 (x1 x1)))* |
|    `p`       | compose both of binary               | *λλλλ(x2 (x1 x4) (x1 x3))* |
|    `q`       | compose each of binary               | *λλλλλ(x3 (x2 x5) (x1 x4))* |
|    `b`       | compose last of ternary              | *λλλλλ(x5 x4 x3 (x2 x1))* |
|    `x`       | 1 argument recursion                 | *λλλλλ(x5 x1 (x4 x1) (x3 x2 x1))* |
|    `y`       | 2 argument recursion                 | *λλλλλλ(x6 x2 x1 (x5 x2 x1) (x4 x3 x2 x1))* |
|    `z`       | 3 argument recursion                 | *λλλλλλλ(x7 x3 x2 x1 (x6 x3 x2 x1) (x5 x4 x3 x2 x1))* |
|              |                                      |              |
|    `T`       | true                                 | *λλx2*       |
|    `F`       | false                                | *λλx1*       |
|    `i`       | if *x1* then *x3* else *x2*          | *λλλ(x1 x3 x2)* |
|    `n`       | not                                  | *λ(x1 λλx1 λλx2)* |
|    `A`       | and                                  | *λλ(x2 x1 x2)* |
|    `V`       | or                                   | *λλ(x2 x2 x1)* |
|    `X`       | xor                                  | *λλ(x2 (x1 λλx1 λλx2) x1)* |
|              |                                      |              |
|   `0…9`      | Church numerals                      | *λλx1*,*λλ(x2 x1)*,…,λλ(x2 (…(x2 x1)…))|
|    `]`       | succ                                 | *λλλ(x2 (x3 x2 x1))* |
|    `[`       | pred                                 | *λλλ(x3 λλ(x1 (x2 x4)) λx2 λx1)* |
|    `+`       | plus                                 | *λλλλ(x4 x2 (x3 x2 x1))* |
|    `-`       | sub **                               | *λλ(x1 λλλ(x3 λλ(x1 (x2 x4)) λx2 λx1) x2)* |
|    `*`       | mult                                 | *λλλ(x3 (x2 x1))* |
|<code>`</code>| pow ***                              | *λλ(x1 x2)*    |
|    `=`       | equality (for Church numerals)       | *λλ(x1 λλλ(x3 λλ(x1 (x2 x4)) λx2 λx1) x2 λλλx1 λλx2 (x2 λλλ(x3 λλ(x1 (x2 x4)) λx2 λx1) x1 λλλx1 λλx2) (x1 λλλ(x3 λλ(x1 (x2 x4)) λx2 λx1) x2 λλλx1 λλx2))* |
|    `L`       | leq                                  | *λλ(x1 λλλ(x3 λλ(x1 (x2 x4)) λx2 λx1) x2 λλλx1 λλx2)* |
|    `l`       | le                                   | *λλ(x1 λλλ(x3 λλ(x1 (x2 x4)) λx2 λx1) λλ(x2 (x4 x2 x1)) λλλx1 λλx2)* |
|    `G`       | geq                                  | *λλ(x2 λλλ(x3 λλ(x1 (x2 x4)) λx2 λx1) x1 λλλx1 λλx2)* |
|    `g`       | ge                                   | *λλ(x2 λλλ(x3 λλ(x1 (x2 x4)) λx2 λx1) λλ(x2 (x3 x2 x1)) λλλx1 λλx2)* |
|    `Z`       | is zero                              | λ(x1 λλλx1 λλx2) |

<sub>* see the section [*Lambda calculus REPL*](#lambda-calculus-repl) on how inputs are read </sub>

<sub>** *sub a b* with *a < b* will result in *0* </sub>

<sub>*** *pow a b* only works for *a,b > 0* </sub>



  [functoid-tio]: https://tio.run/##SyvNSy7Jz0z5/1/JQ0lHKRWIc6A4H4hBUAGIw6H8IqhcChArKuk4/P8PAA
  [LC-wiki]: https://en.wikipedia.org/wiki/Lambda_calculus_definition
  [DB-wiki]: https://en.wikipedia.org/wiki/De_Bruijn_index
  [y-combinator]: https://en.wikipedia.org/wiki/Y_combinator
