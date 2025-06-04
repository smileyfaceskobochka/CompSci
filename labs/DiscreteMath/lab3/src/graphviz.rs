use std::fs::File;
use std::io::{self, Write};
use std::collections::HashSet;

pub fn save_dot(edges: &[(usize, usize, String)], filename: &str) -> io::Result<()> {
    let mut dot = String::new();
    dot.push_str("digraph G {\n");
    dot.push_str("  rankdir=LR;\n");
    dot.push_str("  node [shape=circle];\n");

    let nodes: HashSet<usize> = edges.iter()
        .flat_map(|(u, v, _)| vec![*u, *v])
        .collect();
    for node in nodes {
        dot.push_str(&format!("  {};\n", node));
    }

    for (u, v, label) in edges {
        if *u == *v {
            dot.push_str(&format!("  {} -> {} [label=\"{}\"];\n", u, v, label));
        } else {
            dot.push_str(&format!("  {} -> {} [label=\"{}\"];\n", u, v, label));
        }
    }

    dot.push_str("}\n");
    let mut file = File::create(filename)?;
    file.write_all(dot.as_bytes())?;

    println!("Граф сохранён в файл: {}", filename);
    Ok(())
}