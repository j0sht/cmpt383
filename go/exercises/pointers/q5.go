// Write a function called swap that takes two int pointers as input
//  and exchanges the values they point to
package main

import "fmt"

func swap(x *int, y *int) {
	*x, *y = *y, *x
}

func main() {
	x := 1
	y := 2
	fmt.Printf("Before swap: x = %d, y = %d\n", x, y)
	swap(&x, &y)
	fmt.Printf(" After swap: x = %d, y = %d\n", x, y)
}
