# UCSD-CSE-230-project
Class project in Haskell for Ranjit Jhala's CSE 230 (Programming Languages) class at UCSD

## Introduction

Our project proposal is a calculator for real numbers that encodes all floating point numbers with built-in margins of error and keeps track of the error when performing calculations.

## Design

At the most basic level there will be a command line interface, where the user cas input a numerical expression and the software will compute the value of the expression and print the decimal value up to the margin of error, or throw a warning if the margin of error is too large. The language will support both standard floating-point arithmetic on 32-bit and 64-bit decimal types, for the sake of memory, and on arbitrary-precision decimal numbers. In addition, the language will be able to perform comparison between floating point numbers returning True, False, or indeterminate depending on their margins of error.

We plan to implement this program in Haskell by using a state monad to parse expressions. The default error range for a value will be the double-precision floating point error, but other error ranges can be implemented manually by using the ± operator: a±b represents the range (a - b, a + b).

## Features

So far, we have implemented the following features:

[x] Addition, subtraction, multiplication, and division of the FloatWithError type
[x] Evaluation of expression trees
[ ] Parsing of input text
[ ] Command line interface
[ ] Ability to infer margins of error from inherent floating-point error
[ ] Extra functions, like logarithmic, exponential, and trigonometric functions

## Future implementation ideas

[ ] Incorporate float-with-error type into a programming language to do numerical computing activities with well-defined floating point errors.
[ ] Store the probability distribution of a variable, not just its range
[ ] Allow the range to be computed correctly for expressions whose intermediate values are dependent. For example, if x = 0.5 +/- 0.1, then x and 1 - x are both 0.5 +/- 0.1. The product of two values in this range can be anywhere in (0.16, 0.36), but x*(1 - x) is in the range (0.24, 0.25). This may be done using symbolic calculus to compute the maximum and minimum of a multivariable function, but it is unlikely we can compute the range exactly in the general case because this is known to be a very difficult problem. <sup>[2]</sup>
[ ] Allow to plot graphs of expressions containing "fuzzy values" by plotting a curve as a thin strip using all possible ranges of the output value. Places where the strip got wide would reveal where the precision was not high enough to give an accurate plot image--for example, points close to (1, 0) in the graph of y = (cos(x) - 1)/x^2.

## Background and possible applications

This calculator could be useful to physicists and engineers, allowing them to easily compute quantities based on experimental measurements without the risk of the compounding of errors from multiple measurements going unnoticed. In fact, floating point error is a very common source of bugs in programs, sometimes leading to disastrous effects. <sup>[1]</sup>

The problems of rounding error and floating point error could be resolved with a type that stores real numbers symbolically (i.e. as formulas with roots, e, pi, etc.), with their approximate floating-point values being determined whenever needed using algorithms for computing these numbers. However, this design has issues. For one, it is very hard if not impossible to tell whether two such formulas are equal in general. Furthermore, storing reals as symbolic expressions will result in data whose size is linearly proportional to the amount of calculations involved, potentially yielding very long expressions.

Currently, we have implemented addition and multiplication of the FloatWithError type, as well as a basic expression parser. The expression parser uses simple recursion and does not use monads yet.

References
1. https://web.ma.utexas.edu/users/arbogast/misc/disasters.html
2. _Structure and Interpretation of Computer Programs_, Abelson, Sussman, Sussman, page 88.
