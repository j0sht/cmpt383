package main

import "fmt"

// a)
// Write a function called negpos(s) that takes a slice of ints as input
// and returns two new slices, named neg and pos, where neg contains all
// the numbers in s that are less than 0, and pos contains all the numbers
// in s that are greater than or equal to 0
func negpos(s []int) ([]int, []int) {
	neg, pos := make([]int, 0, 0), make([]int, 0, 0)
	for _, v := range s {
		if v < 0 {
			neg = append(neg, v)
		} else {
			pos = append(pos, v)
		}
	}
	return neg, pos
}

// b)
// Write a function called all(s, pred) where:
//   - s is a slice of int values
//   - pred is a function that takes on int as input, and returns a bool
// all(s, pred) returns true if every int in s is true for pred, and
// false otherwise.
func all(s []int, pred func(int) bool) bool {
	for _, v := range s {
		if !pred(v) {
			return false
		}
	}
	return true

func main() {
	s := []int{6, -2, 0, 2, -6, -4, 3}
	neg, pos := negpos(s)
	fmt.Println(neg)
	fmt.Println(pos)
	// c) Write a single Go statement of the form all(s, pred) that
	//    returns true just when every element in s is < 0, false
	//    otherwise.
	fmt.Println(all(neg, func(x int) bool { return x < 0 }))
	fmt.Println(all(pos, func(x int) bool { return x >= 0 }))
}
