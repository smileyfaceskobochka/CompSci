package main

import (
	"bufio"
	"flag"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func readAdjacencyMatrix(filename string) ([][]int, error) {
	f, err := os.Open(filename)
	if err != nil {
		return nil, fmt.Errorf("не открыть файл %s: %v", filename, err)
	}
	defer f.Close()

	sc := bufio.NewScanner(f)
	var lines []string
	for sc.Scan() {
		line := strings.TrimSpace(sc.Text())
		if line != "" {
			lines = append(lines, line)
		}
	}
	if err := sc.Err(); err != nil {
		return nil, fmt.Errorf("ошибка чтения %s: %v", filename, err)
	}

	n := len(lines)
	if n == 0 {
		return nil, fmt.Errorf("пустой файл %s", filename)
	}

	matrix := make([][]int, n)
	for i, line := range lines {
		parts := strings.Fields(line)
		if len(parts) != n {
			return nil, fmt.Errorf(
				"строка %d: %d элементов, нужно %d", i+1, len(parts), n)
		}
		row := make([]int, n)
		for j, tok := range parts {
			val, err := strconv.Atoi(tok)
			if err != nil || (val != 0 && val != 1) {
				return nil, fmt.Errorf("элемент [%d,%d] = %v", i, j, tok)
			}
			row[j] = val
		}
		matrix[i] = row
	}

	for i := 0; i < n; i++ {
		if matrix[i][i] != 0 {
			return nil, fmt.Errorf("диагональ [%d,%d] должна быть 0", i, i)
		}
		for j := i + 1; j < n; j++ {
			if matrix[i][j] != matrix[j][i] {
				return nil, fmt.Errorf(
					"несимметрично: [%d,%d]=%d, [%d,%d]=%d",
					i, j, matrix[i][j], j, i, matrix[j][i],
				)
			}
		}
	}
	return matrix, nil
}

func complementMatrix(orig [][]int) [][]int {
	n := len(orig)
	comp := make([][]int, n)
	for i := 0; i < n; i++ {
		comp[i] = make([]int, n)
		for j := 0; j < n; j++ {
			if i == j {
				comp[i][j] = 0
			} else {
				comp[i][j] = 1 - orig[i][j]
			}
		}
	}
	return comp
}

func printMatrices(orig, comp [][]int) {
	n := len(orig)
	const (
		reset = "\033[0m"
		blue  = "\033[1;34m"
		green = "\033[1;32m"
	)

	fmt.Println("         ОРИГИНАЛЬНАЯ МАТРИЦА      |           ДОПОЛНЕНИЕ")
	fmt.Print("  # ", blue)
	for i := 1; i <= n; i++ {
		fmt.Printf("%2d ", i)
	}
	fmt.Print(reset, " | ", blue)
	for i := 1; i <= n; i++ {
		fmt.Printf("%2d ", i)
	}
	fmt.Println(reset)

	for i := 0; i < n; i++ {
		fmt.Print(green, fmt.Sprintf("%3d ", i+1), reset)
		for j := 0; j < n; j++ {
			fmt.Printf("%2d ", orig[i][j])
		}
		fmt.Print(" | ")
		for j := 0; j < n; j++ {
			fmt.Printf("%2d ", comp[i][j])
		}
		fmt.Println()
	}
}

func main() {
	viz := flag.Bool("viz", false, "визуализировать граф")
	vizLong := flag.Bool("visualize", false, "визуализировать граф")
	flag.Parse()
	visualize := *viz || *vizLong

	mat, err := readAdjacencyMatrix("input.txt")
	if err != nil {
		fmt.Fprintf(os.Stderr, "Ошибка: %v\n", err)
		os.Exit(1)
	}

	comp := complementMatrix(mat)
	printMatrices(mat, comp)

	if visualize {
		if err := WriteDOT(mat, "original_graph.dot", "OriginalGraph"); err != nil {
			fmt.Fprintln(os.Stderr, "Не создать original_graph.dot:", err)
		} else {
			fmt.Fprintln(os.Stderr, "Создан: original_graph.dot")
		}

		if err := WriteDOT(comp, "complement_graph.dot", "ComplementGraph"); err != nil {
			fmt.Fprintln(os.Stderr, "Не создать complement_graph.dot:", err)
		} else {
			fmt.Fprintln(os.Stderr, "Создан: complement_graph.dot")
		}
	}
}
