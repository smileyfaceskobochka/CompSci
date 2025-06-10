#include <stdio.h>
#include <stdlib.h>

typedef struct {
  int tail;
  int head;
} Edge;

typedef struct {
  int n;
  int m;
  int **inc;
  Edge *edges;
} Graph;

int target;
int *used_edge;
int *path;

void dfs(Graph *g, int v, int depth) {
  if (v == target) {
    for (int i = 0; i < depth; i++) {
      printf("%d", path[i]);
      if (i < depth - 1)
        printf(" -> ");
    }
    printf("\n");
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

void generate_dot_file(Graph *g, const char *filename) {
  FILE *f = fopen(filename, "w");
  if (!f) {
    perror("Не удалось создать dot-файл");
    return;
  }

  fprintf(f, "digraph G {\n");

  for (int i = 1; i <= g->n; i++) {
    fprintf(f, "  %d;\n", i);
  }

  for (int i = 0; i < g->m; i++) {
    fprintf(f, "  %d -> %d;\n", g->edges[i].tail, g->edges[i].head);
  }

  fprintf(f, "}\n");
  fclose(f);

  printf("DOT-файл успешно создан: %s\n", filename);
}

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

  printf("Матрица инцидентности:\n");

  g->inc = malloc(g->n * sizeof(int *));
  for (int i = 0; i < g->n; i++) {
    g->inc[i] = malloc(g->m * sizeof(int));
    for (int j = 0; j < g->m; j++) {
      if (fscanf(f, "%d", &g->inc[i][j]) != 1) {
        fprintf(stderr, "Некорректный формат: ожидается число в матрице\n");
        exit(EXIT_FAILURE);
      }
      printf("%3d ", g->inc[i][j]);
    }
    printf("\n");
  }
  fclose(f);

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

void free_graph(Graph *g) {
  for (int i = 0; i < g->n; i++)
    free(g->inc[i]);
  free(g->inc);
  free(g->edges);
  free(g);
}

int main() {
  Graph *g = load_graph("input.txt");

  generate_dot_file(g, "graph.dot");

  printf("Введите номер целевой вершины (1..%d): ", g->n);
  if (scanf("%d", &target) != 1 || target < 1 || target > g->n) {
    fprintf(stderr, "Неверный номер вершины\n");
    free_graph(g);
    return EXIT_FAILURE;
  }

  used_edge = calloc(g->m, sizeof(int));
  path = malloc((g->m + 1) * sizeof(int));

  printf("\nПути до целевой вершины %d:\n", target);
  for (int v = 1; v <= g->n; v++) {
    path[0] = v;
    dfs(g, v, 1);
  }

  free(used_edge);
  free(path);
  free_graph(g);
  return EXIT_SUCCESS;
}