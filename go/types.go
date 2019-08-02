package main

import "fmt"

// You can create your own types in Go using the type keyword
// In this example, int is said to be the underlying type of Score
// One reason to create types is to make your source code clearer.
// Another reason is that you can define methods on a type Score, but you
//  cannot define methods on a basic type such as int.
type Score int

func show(s Score) {
	fmt.Println("mark =", s)
}

func main() {
	s := Score(5) // s is declared to be of type Score
	show(s)

	// This sort of assignment is permitted
	// Strictly speaking, 5 is an int, and so it is not of type Score.
	// However, because Score has an underlying type of int, this
	//  statement is not an error.
	show(5)
}
