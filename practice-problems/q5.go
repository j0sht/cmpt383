package main

import "fmt"

// a. Write a function called add(x, y) that returns the sum of two ints
func add(x int, y int) int {
	return x + y
}

// b. Write a function called c_add that is a curried version of add
func c_add(x int) (func(int) int) {
	return func(y int) int {
		return x + y
	}
}

// c. Write a function called curry(f) that returns a curried version of f.
//    Assume that f is a (non-curried) function that takes two ints as input
//    and returns an int.
func curry(f (func(int,int) int)) func(int) (func(int) int) {
	return func(x int) (func(int) int) {
		return func(y int) int {
			return f(x,y)
		}
	}
}

func main() {
	fmt.Println(add(3,5))
	add3 := c_add(3)
	fmt.Println(add3(5))
	fmt.Println(c_add(2)(4))

	curr_add := curry(add)
	add2 := curr_add(2)
	fmt.Println(add2(6))
	add5 := curr_add(5)
	fmt.Println(add5(5))
}
