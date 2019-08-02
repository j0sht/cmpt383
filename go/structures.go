package main

import "fmt"

// Go structs are similar to the structures in C-like languages.
// For example, here is how you can represent a 2D point
type Point struct {
	x, y int
}

// Here is an example of a struct built from two points
type Segment struct {
	start, end Point
}

func main() {
	// One way to create a struct is to use the new function
	p := new(Point) // new initializes p.x and p.y to their 0 values
	fmt.Println("p.x: ", p.x)
	fmt.Println("p.y: ", p.y)

	// The new function always sets the variables in a struct
	//  to their zero values. If you want to initialize them to some
	//  other values, you can use a composite literal
	// Note that new is not used here
	dest := Point{-8, 11} // composite literal
	fmt.Println("dest.x: ", dest.x)
	fmt.Println("dest.y: ", dest.y)

	trip := Segment{Point{1, 2}, Point{3, 4}}
	fmt.Printf("%v\n", trip) // %v prints Go values in nice way

	// Another way to write a composite literal is with explicit
	//  parameter names.
	otherTrip := Segment{start: Point{1, 2}, end: Point{3, 4}}
	fmt.Println("%v\n", otherTrip)

	// Note that order doesn't matter when you provide names
	anotherTrip := Segment{end: Point{1, 2}, start: Point{3, 4}}
	fmt.Println("%v\n", anotherTrip)
}
