use std::collections::{HashMap, HashSet};
use std::env;
use std::fs::File;
use std::io::{self, BufRead, BufReader};

mod graphviz; // Модуль для визуализации графа

fn main() {
    // Определяем, включена ли визуализация
    let enable_visualization = {
        let args: Vec<String> = env::args().collect();
        args.contains(&"-viz".to_string()) || args.contains(&"--visualize".to_string())
    };

    // Читаем матрицу инцидентности из файла "input.txt"
    let (edges, _edge_labels) = read_incidence_matrix("input.txt").expect("Ошибка чтения файла");

    // Если визуализация включена, сохраняем граф в формате DOT
    if enable_visualization {
        if let Err(e) = graphviz::save_dot(&edges, "graph.dot") {
            eprintln!("Ошибка сохранения графа: {}", e);
            return;
        }
    } else {
        println!("Визуализация отключена. Для включения используйте флаг --visualize или -viz.");
    }

    // Создаём HashMap для быстрого поиска обратных рёбер
    let reverse_edges: HashMap<(usize, usize), String> = edges
        .iter()
        .map(|(u, v, label)| ((*v, *u), label.clone()))
        .collect();

    let mut found_bidirectional_arcs = HashSet::new();

    // Проверяем каждое ребро на наличие обратного
    for (u, v, label) in &edges {
        // Ищем ребро из v в u
        if let Some(reverse_label) = reverse_edges.get(&(*v, *u)) {
            // Исключаем петли (u == v) и убеждаемся, что это разные дуги
            if *u != *v {
                // Упорядочиваем вершины, чтобы избежать дубликатов (A-B и B-A)
                let ordered_pair = if *u < *v { (*u, *v) } else { (*v, *u) };
                // Добавляем найденную двунаправленную дугу
                found_bidirectional_arcs.insert((ordered_pair, label.clone(), reverse_label.clone()));
            }
        }
    }

    // Удаляем дубликаты для вывода (A-B и B-A считаем одной парой)
    let mut unique_bidirectional_arcs_for_output = HashMap::new();
    for ((u, v), label1, label2) in found_bidirectional_arcs {
        unique_bidirectional_arcs_for_output.entry((u, v)).or_insert((label1, label2));
    }

    // Выводим результаты
    println!("\n--- Результаты ---");
    println!("Количество двунаправленных дуг: {}", unique_bidirectional_arcs_for_output.len());
    println!("Множество найденных дуг:");
    if unique_bidirectional_arcs_for_output.is_empty() {
        println!("  Двунаправленные дуги не найдены.");
    } else {
        for ((u, v), (label_uv, label_vu)) in unique_bidirectional_arcs_for_output {
            println!("  ({}, {}) — дуги \"{}\" (от {} к {}) и \"{}\" (от {} к {})", u, v, label_uv, u, v, label_vu, v, u);
        }
    }
    println!("------------------\n");
}

/// Функция для чтения матрицы инцидентности из файла.
/// Возвращает список дуг (начало, конец, метка) и карту меток дуг.
fn read_incidence_matrix(
    filename: &str,
) -> Result<(Vec<(usize, usize, String)>, HashMap<String, (usize, usize)>), io::Error> {
    let file = File::open(filename)?;
    let reader = BufReader::new(file);
    let mut lines = reader.lines();

    // Читаем метки дуг из заголовка файла
    let header = lines
        .next()
        .ok_or(io::Error::new(io::ErrorKind::InvalidData, "Файл пуст или отсутствует заголовок"))??;
    let edge_labels: Vec<String> = header
        .split_whitespace()
        .map(|s| s.trim().to_string())
        .filter(|s| !s.is_empty())
        .collect();

    let mut edges_list = Vec::new(); // Список дуг
    let mut temp_edge_data: HashMap<String, (usize, usize)> = HashMap::new(); // Временная карта для сборки дуг
    let mut vertex_name_to_id = HashMap::new(); // Карта имен вершин в ID
    let mut next_vertex_id = 1; // Счётчик ID вершин

    // Обрабатываем строки файла, представляющие вершины
    for line_result in lines {
        let line = line_result?;
        let parts: Vec<&str> = line.split_whitespace().collect();
        if parts.is_empty() {
            continue; // Пропускаем пустые строки
        }

        let vertex_name = parts[0]; // Имя вершины
        // Получаем или генерируем числовой ID для вершины
        let current_vertex_id = *vertex_name_to_id.entry(vertex_name.to_string()).or_insert_with(|| {
            let id = next_vertex_id;
            next_vertex_id += 1;
            id
        });

        // Проверяем количество столбцов
        if parts.len() - 1 != edge_labels.len() {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!(
                    "Неверное количество столбцов для вершины '{}'. Ожидалось {}, получено {}",
                    vertex_name,
                    edge_labels.len(),
                    parts.len() - 1
                ),
            ));
        }

        // Обрабатываем значения для каждой дуги в строке
        for (j, &value_str) in parts.iter().skip(1).enumerate() {
            let edge_label = &edge_labels[j];
            let incidence_value: i32 = value_str
                .parse()
                .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, format!("Неверное значение '{1}' для дуги '{0}': {2}", edge_label, value_str, e)))?;

            let (mut start_node, mut end_node) = temp_edge_data.get(edge_label).cloned().unwrap_or((0, 0));

            match incidence_value {
                1 => { // Начало дуги
                    start_node = current_vertex_id;
                }
                -1 => { // Конец дуги
                    end_node = current_vertex_id;
                }
                2 => { // Петля
                    start_node = current_vertex_id;
                    end_node = current_vertex_id;
                }
                0 => { /* Нет связи */ }
                _ => { // Недопустимое значение
                    return Err(io::Error::new(
                        io::ErrorKind::InvalidData,
                        format!(
                            "Недопустимое значение '{}' в матрице инцидентности для дуги '{}' и вершины '{}'. Ожидаются 1, -1, 0 или 2.",
                            incidence_value, edge_label, vertex_name
                        ),
                    ));
                }
            }
            temp_edge_data.insert(edge_label.clone(), (start_node, end_node));
        }
    }

    // Преобразуем собранные данные о дугах в финальный список
    for (label, (u, v)) in temp_edge_data {
        if u == 0 || v == 0 {
            eprintln!("Предупреждение: Дуга '{}' не полностью определена. Игнорируется.", label);
            continue;
        }
        edges_list.push((u, v, label));
    }

    Ok((edges_list, temp_edge_data))
}