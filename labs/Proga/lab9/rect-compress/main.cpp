// #4
#include <bits/stdc++.h>
using namespace std;

int main(int argc, char **argv) {
  ios::sync_with_stdio(false);
  cin.tie(nullptr);

  if (argc < 2) {
    cerr << "Usage: " << argv[0] << " <input_file>\n";
    return 1;
  }

  ifstream fin(argv[1]);
  if (!fin.is_open()) {
    cerr << "Cannot open input file\n";
    return 1;
  }

  int n, m;
  fin >> n >> m;
  vector<string> a(n);
  for (int i = 0; i < n; i++) {
    fin >> a[i];
  }
  fin.close();

  // Результат
  vector<array<int, 4>> rects;

  vector<vector<bool>> used(n, vector<bool>(m, false));

  for (int i = 0; i < n; i++) {
    for (int j = 0; j < m; j++) {
      if (used[i][j])
        continue;
      char val = a[i][j];

      int right = j;
      while (right < m && a[i][right] == val && !used[i][right])
        right++;
      int width = right - j;

      // Находим высоту
      int down = i;
      bool ok = true;
      while (ok && down < n) {
        for (int k = j; k < j + width; k++) {
          if (a[down][k] != val || used[down][k]) {
            ok = false;
            break;
          }
        }
        if (ok)
          down++;
      }
      int height = down - i;

      for (int x = i; x < i + height; x++) {
        for (int y = j; y < j + width; y++) {
          used[x][y] = true;
        }
      }

      rects.push_back({i, j, i + height - 1, j + width - 1});
    }
  }

  cout << rects.size() << '\n';
  for (auto &r : rects) {
    cout << r[0] << ' ' << r[1] << ' ' << r[2] << ' ' << r[3] << '\n';
  }

  return 0;
}