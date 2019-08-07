// A slice is a data structure that describes a contiguous sequence of
//  elements in an array.
// A slice is not an array, but for most purposes it acts like one.

/* How Slices are Implemented */
/*
   type sliceHeader struct {
       Length        int
       Capacity      int
       ZerothElement *int
   }
*/
package main

import "fmt"

// sl can be any int slice of any length
// unlike arrays, the length of a slice always refers to an underlying
//  array, and if that array changes, then so does the slice
func display(sl []int) {
	for i, val := range sl {
		fmt.Printf("sl[%d] = %d\n", i, val)
	}
}

func main() {
	// One way to make a slice is from an array
	var arr [5]int // an array with 5 elements
	for i := range arr {
		arr[i] = i
	}

	// a slice referring to arr[1], arr[2], arr[3]
	sl := arr[1:4]

	// you can use a slice pretty much the same way that you use
	//  an array
	for i := 0; i < len(sl); i++ {
		fmt.Printf("sl[%d] = %d\n", i, sl[i])
	}

	// NOTE:
	//  It's important to understand that a slice always refers to an
	//   underlying array, and if that array changes, then so does
	//   the slice.
	arr[1] = -100 // change second element of arr

	for i := 0; i < len(sl); i++ {
		fmt.Printf("sl[%d] = %d\n", i, sl[i])
	}

	// Note:
	// For all slices sl[a:b], 0 <= a <= b <= n
	sl2 := arr[2:5] // sl refers to arr[2], arr[3], and arr[4]
	display(sl2)

	s := arr[0:5] // okay: s is the entire array
	s = arr[0:4] // okay: s is the first first 4 elements of the array
	s = arr[5:5] // okay: s is them empty slice, []

	// If a > b, you get an inverted slice error
	// s = arr[3:1] // slice indices are inverted
	s = arr[a:] // same as arr[a:n]
	s = arr[:b] // same as arr[0:b]
	s = arr[:]  // same as arr[0:n]
	display(s)

	// You can create slices from another slice.
	s2 := s[0:2] // a slice of a slice
	display(s)

	fmt.Printf("s  is %v\n", s)  // %v prints the value of a Go object
	fmt.Printf("s2 is %v\n", s2)

	// Both slices refer to the same underlying array, so if that
	//  changes, the slices change too
	arr[1] = 77
	fmt.Printf("\ns  is %v\n", s)
	fmt.Printf("s2 is %v\n", s2)

	s2[0] = -55
	fmt.Printf("arr is %v\n", arr)
	fmt.Printf("  s is %v\n", s)
	fmt.Printf(" s2 is %v\n", s2)

}
