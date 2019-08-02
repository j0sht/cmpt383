// Like most programming languages, Go has a data structure called an array
//  that stores a sequence of objects of the same type.
// Less commonly, Go has a second - and more useful - data structure called
//  a slice that lets you refer to a contiguous sub-sequence of elements
//  from an underlying array.
package main

import "fmt"

// You can pass an array to a function. As with all parameters, Go passes
//  arrays by value, i.e., it makes a copy of the array.
// Copying an array is generally inefficient, and one way around this is
//  to accept a pointer to the array
// NOTE: That this function only works with arrays of length 5
//       The length of an array is part of the array's type, so arrays of
//       different lengths have different types. Go addresses these
//       problems by introducing a new data structure called a slice.
func add1(arr *[5]int) {
	for i := range *arr {
		(*arr)[i]++
	}
}

func main() {
	// Arrays
	// Arrays are a fundamental data structure in Go
	// The following declares arr to be an array of 5 ints,
	// all initialized to 0
	var arr [5]int

	// You can iterate through items in an array using a ranged
	//  for-loop. This is often the nicest way to traverse arrays in Go
	for i, val := range arr {
		fmt.Printf("arr[%d] = %d\n", i, val)
	}

	// You can also create an array using an array literal
	// NOTE: You can't change the size of an array in Go. Once it is
	//       created, it's length remains fixed until it is disposed
	//       by the garbage collector
	var arr1 = [5]int{1,2,3,4,5} // array literal

	// Or you can iterate through an array in a traditional C-style loop
	// The len function returns the length of an array
	for i := 0; i < len(arr1); i++ {
		// Square bracket syntax is used to access individual
		//  elements at a given position.
		// The first index of a Go array is always 0
		fmt.Printf("arr[%d] = %d\n", i, arr1[i])
	}

	// Note the '&'
	add1(&arr)
	for i, val := range arr {
		fmt.Printf("arr[%d] = %d\n", i, val)
	}
}
