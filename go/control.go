package main

import "fmt"

func main() {
	fmt.Print("Please enter your score (0 to 100): ")

	var score float64 // score is a 64-bit floating point number (0.0)

	// Scanf returns the number of items scanned and an error status
	// Here we're ignoring the number of items scanned
	_, ok := fmt.Scanf("%f", &score)

	if ok != nil {
		panic("input error")
	}

	fmt.Printf("Your score: %.f\n", score)

	// If statements are similar to those in C-like languages
	// Notice you don't need to put the if-conditions inside ()
	// They must always use curly braces, even for a body with only
	//  a single statement.
	if score < 0 {
		fmt.Println("Negative scores are not allowed.")
	} else if score < 50 {
		fmt.Println("Sorry, you failed.")
	} else if score < 100 {
		fmt.Println("Congratulations, you passed.")
	} else if score == 100 {
		fmt.Println("Perfect! You passed.")
	} else {
		fmt.Println("Scores over 100 are not allowed.")
	}

	// The Go switch statement is similar to the one in C, although
	//  more flexible and with different default behavior.
	switch { // notice there is no variable here
	case score < 0:
		fmt.Println("Negative scores are not allowed.")
	case score < 50:
		fmt.Println("Sorry, you failed.")
	case score < 100:
		fmt.Println("Congratulations you passed.")
	case score == 100:
		fmt.Println("Perfect! You passed.")
	default:
		fmt.Println("Scores over 100 are not allowed.")
	}

	grade := "C+"
	// Switch statements are not limited to characters and integers
	// Importantly, Go's switch statements do not fall-through to the
	//  next case by default. If you want that behavior, add the
	//  'fallthrough' keyword as the last statement of the case.
	switch grade {
	case "A+", "A", "A-":
		fmt.Println("Excellent")
	case "B+", "B", "B-":
		fmt.Println("Good")
	case "C+", "C", "C-":
		fmt.Println("Average")
	case "D":
		fmt.Println("Below average")
	case "F":
		fmt.Println("Failure")
	default:
		// panic is a built-in function that will crash the program
		// It should be used in cases where a very serious problem
		//  occurs that does not allow the program to continue
		//  executing normally.
		panic("Uknown grade \"" + grade + "\"")
	}

	// Go has only one looping structure --> The for loop
	// It is flexible enough to simulate C-style while loops and
	//  for loops, plus range-style loops.
	// Notice: There are no () brackets required around the statments
	//  just after the for.
	// As with if-statements, the curly braces are always required.
	var sum int
	// sums the squares of numbers from 1 to 100
	for i := 0; i < 100; i++ {
		sum += (i + 1) * (i + 1)
	}
	fmt.Printf("sum: %d\n", sum)

	// Go can simulate while loops by leaving out the initialization
	//  and increment statements of a for loop
	i := 100
	for i <= 100 {
		sum += i * i
		i++
	}

	// If you want an infinite loop, you can write this:
	// for {
	// 	fmt.Println("and again and again ")
	// }

	// Ranged for-loops iterate through a collection
	// This works with arrays, slices, and maps.
	// In many cases, ranged for-loops are preferred because they
	//  handle the details of accessing all the elements of a
	//  collection.
	s := "apple"
	for i, c := range s { // := used so that i and c are created
		fmt.Printf("%d, %c\n", i, c)
	}
}
