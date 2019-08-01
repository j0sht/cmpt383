package main

import "fmt"

// One way to declare variables in Go is to use var
// If you don't give an initial value to a variable, Go automatically
//  assigns the zero-value associated with that type.
var n int    // n is an int initialized to 0
var s string // s is a string initialized to ""

// You can also provide initial values
var a, b int = 2, 3 // declare a to be an int with initial value 2 (and b 3)

// Multiple variable declarations can be grouped like this:
var (
	num int
	str string = "apple"
	x, y = 3, 4
)

// Go also lets you declare variables without explicitly declaring a type
var num2 = 2 // 2 is an int, so n is inferred to be of type int
var m = num2 // m is inferred to be of type int because n is an int

func main() {
	// Another way to declare a variable is using the ':=' operator
	// ':=' is often used in practice
	// You can only use ':=' inside a function
	num3 := 3        // declare num3 and initialize it to 3
	str2 := "apple"  // declare str2 and initialize it to "apple"

	// You can declare multiple variables with a single ':='
	name, age := "Jane", 22

	// You can also assign multiple variables using '='
	// You can easily swap values using this
	j, k := 1, 3
	j, k := k, j // swap j and k

	// Redeclaration
	// You can redeclare variables with ':=' as long as there is
	//  at least one new variable on the left side
	z := 1
	// z:= 2 // compile error
	r, z := 0, 2 // okay

	// Go considers unused variables errors (Everything above won't
	//  compile unless you use them)

	// Blank identifier
	// Go uses '_' when you must create a variable you don't care about
}
