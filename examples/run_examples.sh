#!/bin/bash
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )"

function test_file {
printf "\e[32;1m$1\n\e[0m"
printf "$(cat $2 | tr -d '\n')\n" | $DIR/../MiniOOL --no-logo --verbose
printf "\e[33;1mPress anything to continue...\e[0m"
read -n1 -r -p "" key
printf "\n"
}

test_file "Static scoping (r should equal 5)" $DIR/static_scoping.mini
test_file "Recursive procedures (p should equal 1)" $DIR/recursive_procedure.mini
test_file "Object creation (r should equal 0)" $DIR/object_creation.mini
test_file "Factorial 17 (r should equal 0)" $DIR/factorial.mini
test_file "Fibonacci 17 recursivly (r should equal 0)" $DIR/fibonacci.mini
test_file "Fibonacci 90 iterativly (r should equal 0)" $DIR/fibonacci_iterative.mini
test_file "Parameter currying (r should equal 0)" $DIR/curry.mini
test_file "Infinite parameter currying (r should equal 0)" $DIR/curry_inf.mini
test_file "Race condition (1/3) (possible values of r: null, 10, 20)" $DIR/race.mini
test_file "Race condition (2/3) (possible values of r: null, 10, 20)" $DIR/race.mini
test_file "Race condition (3/3) (possible values of r: null, 10, 20)" $DIR/race.mini
test_file "Race condition with atom (1/2) (possible values of r: null, 20)" $DIR/atomic_race.mini
test_file "Race condition with atom (2/2) (possible values of r: null, 20)" $DIR/atomic_race.mini
test_file "Implicit global variables with dynamic scope (r should be 6)" $DIR/dynamic_globals.mini
