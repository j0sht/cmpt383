package main

import (
	"fmt"
	"io/ioutil"
)

// A Go function consists of a signature (or header) followed by a block of
//  code that will be executed when the function is called.

// The function name is hello
// It takes a single parameter of type string called name.
// Notice that the name of a parameter comes first, followed by its data type
// It prints a message to the screen without returning a value
func hello(name string) {                  // function signature
	fmt.Println("Hello " + name + "!") // function body
}

// In Go, function parameters are always passed by value.
// For example, calling f(x) means that the value of x is copied. Thus, you
//  cannot modify the value of x, and, if x is big, the copying takes
//  extra time and space.

// rect_area takes two int parameters, width and height, and returns an int
func rect_area(width, height int) int {
	return width * height
}

// ecount counts the number of e's in a string
func ecount(s string) int {
	result := 0
	for _, c := range s {
		if c == 'e' || c == 'E' {
			result++
		}
	}
	return result
}

// Another way to write ecount with a named result parameter
func ecount2(s string) (result int) {
	for _, c := range s {
		if c == 'e' || c == 'E' {
			result++
		}
	}
	return
}

// Functions can return multiple values.
// A common use case for this is to return a value and an error flag.
func ecountFile(fname string) (int, error) {
	bytes, err := ioutil.ReadFile(fname) // bytes is a byte slice
	result := 0
	if err == nil { // err is nil when file is read correctly
		for _, c := range bytes {
			if c == 'e' || c == 'E' {
				result++
			}
		}
	}
	return result, err
}

func main() {
	hello("Dave")
	fmt.Printf("rect_area(10, 5) = %d\n", rect_area(10, 5))
}
