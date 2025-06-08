#include <stdio.h>
#include <stdlib.h>

// Структуры для представления графа
typedef struct {
  int tail; // начальная вершина (1-based)
  int head; // конечная вершина (1-based)
} Edge;

typedef struct {
  int n;       // число вершин
  int m;       // число дуг
  int **inc;   // матрица инцидентности [n][m]
  Edge *edges; // массив дуг длиной m
} Graph;

// Глобальные переменные для DFS
int target;     // номер целевой вершины
int *used_edge; // флаг использования дуги
int *path;      // текущий путь (вершины)

// Рекурсивный DFS для перебора путей
void dfs(Graph *g, int v, int depth) {
  if (v == target) {
    printf("Path: ");
    for (int i = 0; i < depth; i++) {
      printf("%d", path[i]);
      if (i < depth - 1)
        printf(" -> ");
    }
    printf("\n");
    // не ранний выход — ищем и другие пути
  }
  for (int e = 0; e < g->m; e++) {
    if (!used_edge[e] && g->edges[e].tail == v) {
      used_edge[e] = 1;
      path[depth] = g->edges[e].head;
      dfs(g, g->edges[e].head, depth + 1);
      used_edge[e] = 0;
    }
  }
}

// Загрузка графа из файла input.txt
Graph *load_graph(const char *filename) {
  FILE *f = fopen(filename, "r");
  if (!f) {
    perror("Не удалось открыть input.txt");
    exit(EXIT_FAILURE);
  }

  Graph *g = malloc(sizeof(Graph));
  if (fscanf(f, "%d %d", &g->n, &g->m) != 2) {
    fprintf(stderr, "Некорректный формат: ожидаются числа n и m\n");
    exit(EXIT_FAILURE);
  }

  // Выделяем память под матрицу инцидентности
  g->inc = malloc(g->n * sizeof(int *));
  for (int i = 0; i < g->n; i++) {
    g->inc[i] = malloc(g->m * sizeof(int));
    for (int j = 0; j < g->m; j++) {
      fscanf(f, "%d", &g->inc[i][j]);
    }
  }
  fclose(f);

  // Выделяем и заполняем массив дуг
  g->edges = malloc(g->m * sizeof(Edge));
  for (int j = 0; j < g->m; j++) {
    int tail = -1, head = -1;
    for (int i = 0; i < g->n; i++) {
      if (g->inc[i][j] == -1)
        tail = i + 1;
      if (g->inc[i][j] == 1)
        head = i + 1;
    }
    if (tail < 1 || head < 1) {
      fprintf(stderr, "Ошибка: некорректное определение дуги в столбце %d\n",
              j + 1);
      exit(EXIT_FAILURE);
    }
    g->edges[j].tail = tail;
    g->edges[j].head = head;
  }
  return g;
}

// Очистка памяти
void free_graph(Graph *g) {
  for (int i = 0; i < g->n; i++)
    free(g->inc[i]);
  free(g->inc);
  free(g->edges);
  free(g);
}

int main() {
  Graph *g = load_graph("input.txt");

  printf("Введите номер целевой вершины (1..%d): ", g->n);
  if (scanf("%d", &target) != 1 || target < 1 || target > g->n) {
    fprintf(stderr, "Неверный номер вершины\n");
    free_graph(g);
    return EXIT_FAILURE;
  }

  used_edge = calloc(g->m, sizeof(int));
  path = malloc((g->m + 1) * sizeof(int));

  // Запускаем DFS из всех вершин
  for (int v = 1; v <= g->n; v++) {
    path[0] = v;
    dfs(g, v, 1);
  }

  free(used_edge);
  free(path);
  free_graph(g);
  return EXIT_SUCCESS;
}
