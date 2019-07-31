// hello.go

// To run this, type the following at the command-line:
//
//  $ go run hello.go
//

// Go code is organized into packages
// A runnable Go program must have one package called main, and within that,
//  it must have one function called main.
package main

// fmt is a package in the Go standard library that contains basic reading
//  and writing functions such as fmt.Println
import "fmt"

// All functions start with 'func'
func main() {
	// Statements can end with a ';', but don't need to.
	// In practice, they are rarely used to end statements.
	fmt.Println("Hello from Go!")
}
