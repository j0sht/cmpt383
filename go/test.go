package main

import "fmt"

func sum(p [3]float64) float64 {
	return p[0] + p[1] + p[2]
}

func main() {
	// arr := [3]float64{1,2,3}
	sl := [3]float64{5.0, 6.0}
	result := sum(sl)
	fmt.Printf("result = %.f\n", result)
}
