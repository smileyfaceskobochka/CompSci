use std::fs::File;
use std::io::{self, Write};

pub fn save_dot(matrix: &Vec<Vec<usize>>, n: usize, filename: &str) -> io::Result<()> {
    let mut dot = String::new();
    dot.push_str("digraph G {\n");
    dot.push_str("  rankdir=LR;\n");
    dot.push_str("  node [shape=circle];\n");

    for i in 1..=n {
        dot.push_str(&format!("  {};\n", i));
    }

    for i in 0..n {
        for j in 0..n {
            if matrix[i][j] == 1 {
                dot.push_str(&format!("  {} -> {};\n", i + 1, j + 1));
            }
        }
    }
    dot.push_str("}\n");

    let mut file = File::create(filename)?;
    file.write_all(dot.as_bytes())?;

    println!("\nГраф сохранён в файле: {}", filename);
    println!("Для просмотра установите Graphviz и выполните:");
    println!("dot -Tpng {} -o graph.png && open graph.png", filename);

    Ok(())
}