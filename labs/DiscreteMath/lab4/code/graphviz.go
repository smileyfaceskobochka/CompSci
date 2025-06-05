package main

import (
	"bufio"
	"fmt"
	"os"
)

func WriteDOT(matrix [][]int, filename, graphName string) error {
	n := len(matrix)

	f, err := os.Create(filename)
	if err != nil {
		return fmt.Errorf("не удалось создать файл %s: %v", filename, err)
	}
	defer f.Close()

	w := bufio.NewWriter(f)

	fmt.Fprintf(w, "graph %s {\n", graphName)

	for i := 1; i <= n; i++ {
		fmt.Fprintf(w, "    %d;\n", i)
	}

	for i := 0; i < n; i++ {
		for j := i + 1; j < n; j++ {
			if matrix[i][j] != 0 {
				fmt.Fprintf(w, "    %d -- %d;\n", i+1, j+1)
			}
		}
	}

	fmt.Fprintln(w, "}")

	if err := w.Flush(); err != nil {
		return fmt.Errorf("ошибка при записи в файл %s: %v", filename, err)
	}
	return nil
}
