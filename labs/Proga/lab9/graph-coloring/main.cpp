// #3
#include <bits/stdc++.h>
using namespace std;
typedef vector<int> vi;
typedef vector<vi> vvi;
int main() {
  ios::sync_with_stdio(false);
  cin.tie(NULL);

  int n, m;
  if (!(cin >> n >> m))
    return 0;
  vvi adj(n);
  adj.reserve(n);
  int u, v;
  for (int i = 0; i < m; ++i) {
    cin >> u >> v;
    --u;
    --v;
    adj[u].push_back(v);
    adj[v].push_back(u);
  }

  vi color(n, 0);
  vector<unordered_set<int>> neigh_colors(n);
  vi degree(n);
  for (int i = 0; i < n; ++i) {
    degree[i] = adj[i].size();
  }
  vector<bool> used(n, false);

  for (int it = 0; it < n; ++it) {
    // select vertex with max saturation, break ties by degree
    int best = -1;
    int best_sat = -1, best_deg = -1;
    for (int i = 0; i < n; ++i) {
      if (used[i])
        continue;
      int sat = neigh_colors[i].size();
      if (sat > best_sat || (sat == best_sat && degree[i] > best_deg)) {
        best_sat = sat;
        best_deg = degree[i];
        best = i;
      }
    }
    // assign smallest possible color
    int c = 1;
    auto &nc = neigh_colors[best];
    while (nc.find(c) != nc.end())
      ++c;
    color[best] = c;
    used[best] = true;
    // update neighbors
    for (int w : adj[best]) {
      if (!used[w]) {
        neigh_colors[w].insert(c);
      }
    }
  }
  int max_color = 0;
  for (int i = 0; i < n; ++i)
    max_color = max(max_color, color[i]);
  cout << max_color << '\n';
  for (int i = 0; i < n; ++i) {
    cout << color[i] << (i + 1 < n ? ' ' : '\n');
  }
  return 0;
}
