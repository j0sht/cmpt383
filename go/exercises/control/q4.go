// Write a loop that prints the numbers from 10 down to 1
// Do it in Go using a C-like for loop and while loop
package main

import "fmt"

func main() {
	// a.
	for i := 10; i > 0; i-- {
		fmt.Printf("%d\n", i)
	}

	// b.
	i := 10
	for i > 0 {
		fmt.Printf("%d\n", i)
		i--
	}
}
