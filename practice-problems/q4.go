package main

import "fmt"

func char_gen(s string, out chan rune) {
	for _, c := range s {
		out <- c
	}
	close(out)
}

func main() {
	ch := make(chan rune)
	go char_gen("hello world", ch)
	for i := 0; i < len("hello world"); i++ {
		fmt.Println(string(<-ch))
	}
}
