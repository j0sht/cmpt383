// Write a Go ranged loop that prints just the characters of a string
//  s in reverse order. For example, if s is "apple", the loop should print
//  e l p p a (each on a new line)
package main

import "fmt"

func main() {
	s := "apple"
	for i := len(s)-1; i >= 0; i-- {
		fmt.Printf("%c\n", s[i])
	}
}
