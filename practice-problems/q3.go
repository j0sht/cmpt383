package main

import "fmt"

func vc_count(s string) (int, int) {
	v, c := 0, 0
	for _, char := range s {
		switch char {
		case 'y':
			v++
			c++
		case 'a','e','i','o','u':
			v++
		default:
			c++
		}
	}
	return v, c
}

func main() {
	v, c := vc_count("yay")
	fmt.Printf("v = %d, c = %d\n", v, c)
}
