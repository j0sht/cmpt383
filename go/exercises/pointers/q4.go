package main

import "fmt"

func main() {
	// Does the following compile? Yes
	// What does it print? 3
	x := 3
	fmt.Println(*&x)
}
