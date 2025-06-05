use std::collections::{HashMap, HashSet};
use std::env;
use std::fs::File;
use std::io::{self, BufRead, BufReader};

mod graphviz; // Модуль для визуализации графа

fn main() {
    let enable_visualization = {
        let args: Vec<String> = env::args().collect();
        args.contains(&"-viz".to_string()) || args.contains(&"--visualize".to_string())
    };

    println!("Содержимое файла \"input.txt\":");
    if let Ok(file) = File::open("input.txt") {
        let reader = BufReader::new(file);
        for line_result in reader.lines() {
            if let Ok(line) = line_result {
                println!("{}", line);
            }
        }
    }
    println!("----------------------------------------\n");

    let (edges, _edge_labels) = match read_incidence_matrix("input.txt") {
        Ok(res) => res,
        Err(err) => {
            eprintln!("Ошибка чтения матрицы инцидентности: {}", err);
            return;
        }
    };

    if enable_visualization {
        if let Err(e) = graphviz::save_dot(&edges, "graph.dot") {
            eprintln!("Ошибка сохранения графа: {}", e);
            return;
        }
    } else {
        println!("Визуализация отключена. Для включения используйте флаг --visualize или -viz.");
    }

    let edge_map: HashMap<(usize, usize), String> = edges
        .iter()
        .map(|(u, v, label)| ((*u, *v), label.clone()))
        .collect();

    let mut found_bidirectional_arcs = HashSet::new();
    for (u, v, label_uv) in &edges {
        if let Some(label_vu) = edge_map.get(&(*v, *u)) {
            if *u != *v {
                let ordered_pair = if *u < *v { (*u, *v) } else { (*v, *u) };
                found_bidirectional_arcs.insert((ordered_pair, label_uv.clone(), label_vu.clone()));
            }
        }
    }

    let mut unique_bidirectional = HashMap::new();
    for ((a, b), lab1, lab2) in found_bidirectional_arcs {
        unique_bidirectional.entry((a, b)).or_insert((lab1, lab2));
    }

    println!("\n--- Результаты ---");
    println!(
        "Количество двунаправленных дуг: {}",
        unique_bidirectional.len()
    );
    println!("Множество найденных дуг:");
    if unique_bidirectional.is_empty() {
        println!("  Двунаправленные дуги не найдены.");
    } else {
        for ((a, b), (lab_ab, lab_ba)) in unique_bidirectional {
            println!(
                "  ({}, {}) — дуги \"{}\" (от {} к {}) и \"{}\" (от {} к {})",
                a, b, lab_ab, a, b, lab_ba, b, a
            );
        }
    }
    println!("------------------\n");
}

fn read_incidence_matrix(
    filename: &str,
) -> Result<(Vec<(usize, usize, String)>, HashMap<String, (usize, usize)>), io::Error> {
    let file = File::open(filename)?;
    let reader = BufReader::new(file);
    let mut lines = reader.lines();

    let header = lines.next().ok_or(io::Error::new(
        io::ErrorKind::InvalidData,
        "Файл пуст или нет заголовка",
    ))??;
    let edge_labels: Vec<String> = header
        .split_whitespace()
        .map(|s| s.trim().to_string())
        .filter(|s| !s.is_empty())
        .collect();

    let mut edges_list = Vec::new();
    let mut temp_edge_data: HashMap<String, (usize, usize)> = HashMap::new();
    let mut vertex_name_to_id = HashMap::new();
    let mut next_vertex_id = 1;

    for line_result in lines {
        let line = line_result?;
        let parts: Vec<&str> = line.split_whitespace().collect();
        if parts.is_empty() {
            continue;
        }

        let vertex_name = parts[0];
        let current_vertex_id = *vertex_name_to_id
            .entry(vertex_name.to_string())
            .or_insert_with(|| {
                let id = next_vertex_id;
                next_vertex_id += 1;
                id
            });

        if parts.len() - 1 != edge_labels.len() {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!(
                    "Неверное число столбцов для вершины '{}'. Ожидалось {}, получено {}",
                    vertex_name,
                    edge_labels.len(),
                    parts.len() - 1
                ),
            ));
        }

        for (j, &value_str) in parts.iter().skip(1).enumerate() {
            let edge_label = &edge_labels[j];
            let incidence_value: i32 = value_str.parse().map_err(|e| {
                io::Error::new(
                    io::ErrorKind::InvalidData,
                    format!(
                        "Неверное значение '{}' для дуги '{}': {}",
                        value_str, edge_label, e
                    ),
                )
            })?;

            let (mut start_node, mut end_node) =
                temp_edge_data.get(edge_label).cloned().unwrap_or((0, 0));

            match incidence_value {
                -1 => {
                    // Эта вершина – начало дуги
                    start_node = current_vertex_id;
                }
                1 => {
                    // Эта вершина – конец дуги
                    end_node = current_vertex_id;
                }
                2 => {
                    // Петля (начало и конец в одной вершине)
                    start_node = current_vertex_id;
                    end_node = current_vertex_id;
                }
                0 => {
                    // Нет связи, пропуск
                }
                _ => {
                    return Err(io::Error::new(
                        io::ErrorKind::InvalidData,
                        format!(
                            "Недопустимое значение '{}' для дуги '{}' и вершины '{}'. Ожидаются -1, 1, 0 или 2.",
                            incidence_value, edge_label, vertex_name
                        ),
                    ));
                }
            }

            temp_edge_data.insert(edge_label.clone(), (start_node, end_node));
        }
    }

    for (label, &(u, v)) in temp_edge_data.iter() {
        if u == 0 || v == 0 {
            eprintln!(
                "Предупреждение: Дуга '{}' не полностью определена. Игнорируется.",
                label
            );
            continue;
        }
        edges_list.push((u, v, label.clone()));
    }

    Ok((edges_list, temp_edge_data))
}
