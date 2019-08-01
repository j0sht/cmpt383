// Go supports the use of pointer through the familiar C-style '*' notation
// However, it does not allow for pointer arithmetic.
package main

import "fmt"

var n int = 5

// An important application of pointers in Go is to simulate
// passing parameters by reference. For example invert takes a pointer to
// a float64 as input, and replaces the number it points to with its
// reciprocal (so long as it is not 0)
func invert(p *float64) {
	if *p != 0.0 {
		*p = 1 / *p
	}
}

func main() {
	fmt.Println("           n: ", n)
	fmt.Println("address of n: ", &n) // & is the address-of operator

	// A pointer is a variable that stores the address of a variable
	var x int = 8
	var p *int = &x // *int is the type "pointer to int"

	fmt.Println("n: ", n)
	fmt.Println("p: ", p)

	// Type inference also works with pointers
	y := 9
	py := &y

	fmt.Println(" y: ", y)
	fmt.Println("py: ", py)

	// The zero-value for a pointer is nil
	var pz *int
	fmt.Println("pz: ", pz)

	if pz == nil {
		fmt.Println("pz has the value nil")
	}

	// Calling the invert function
	z := 3.0
	invert(&z) // note use of &
	fmt.Println("x: ", z)
}
