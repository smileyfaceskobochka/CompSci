// #2
#include <bits/stdc++.h>
using namespace std;

vector<int> small_primes;

void init_small_primes() {
  if (!small_primes.empty())
    return;
  int n = 1000000;
  vector<bool> is_prime(n + 1, true);
  for (int i = 2; i <= n; i++) {
    if (is_prime[i]) {
      small_primes.push_back(i);
      for (long long j = (long long)i * i; j <= n; j += i) {
        is_prime[j] = false;
      }
    }
  }
}

struct BigInt {
  static const uint32_t BASE = 1000000000;
  vector<uint32_t> a;
  BigInt() {}
  BigInt(const string &s) { read(s); }
  BigInt(uint32_t x) {
    if (x == 0)
      return;
    a.push_back(x);
  }

  void read(const string &s) {
    a.clear();
    for (int i = (int)s.size(); i > 0; i -= 9) {
      int l = max(0, i - 9);
      a.push_back(stoi(s.substr(l, i - l)));
    }
    trim();
  }

  string toString() const {
    if (a.empty())
      return "0";
    string s = to_string(a.back());
    char buf[16];
    for (int i = (int)a.size() - 2; i >= 0; --i) {
      sprintf(buf, "%09u", a[i]);
      s += buf;
    }
    return s;
  }

  void trim() {
    while (!a.empty() && a.back() == 0)
      a.pop_back();
  }

  bool isZero() const { return a.empty(); }
};

int cmp(const BigInt &x, const BigInt &y) {
  if (x.a.size() != y.a.size())
    return x.a.size() < y.a.size() ? -1 : 1;
  for (int i = (int)x.a.size() - 1; i >= 0; --i)
    if (x.a[i] != y.a[i])
      return x.a[i] < y.a[i] ? -1 : 1;
  return 0;
}

bool operator<(const BigInt &x, const BigInt &y) { return cmp(x, y) < 0; }
bool operator==(const BigInt &x, const BigInt &y) { return cmp(x, y) == 0; }
bool operator!=(const BigInt &x, const BigInt &y) { return !(x == y); }

BigInt add(const BigInt &x, const BigInt &y) {
  BigInt res;
  uint64_t carry = 0;
  size_t n = max(x.a.size(), y.a.size());
  res.a.resize(n);
  for (size_t i = 0; i < n; ++i) {
    uint64_t s = carry;
    if (i < x.a.size())
      s += x.a[i];
    if (i < y.a.size())
      s += y.a[i];
    res.a[i] = s % BigInt::BASE;
    carry = s / BigInt::BASE;
  }
  if (carry)
    res.a.push_back(carry);
  return res;
}

BigInt sub(const BigInt &x, const BigInt &y) {
  BigInt res = x;
  int64_t carry = 0;
  for (size_t i = 0; i < res.a.size(); ++i) {
    int64_t d = (int64_t)res.a[i] - carry - (i < y.a.size() ? y.a[i] : 0);
    carry = d < 0;
    if (carry)
      d += BigInt::BASE;
    res.a[i] = d;
  }
  res.trim();
  return res;
}

BigInt mul(const BigInt &x, const BigInt &y);

BigInt mul_karatsuba(const BigInt &x, const BigInt &y) {
  int n = max(x.a.size(), y.a.size());
  if (n <= 32) {
    return mul(x, y);
  }
  int m = n / 2;
  BigInt x0, x1, y0, y1;
  x0.a.assign(x.a.begin(), x.a.begin() + min<size_t>(m, x.a.size()));
  x1.a.assign(x.a.begin() + min<size_t>(m, x.a.size()), x.a.end());
  y0.a.assign(y.a.begin(), y.a.begin() + min<size_t>(m, y.a.size()));
  y1.a.assign(y.a.begin() + min<size_t>(m, y.a.size()), y.a.end());
  BigInt z0 = mul_karatsuba(x0, y0);
  BigInt z2 = mul_karatsuba(x1, y1);
  BigInt x0x1 = add(x0, x1);
  BigInt y0y1 = add(y0, y1);
  BigInt z1 = sub(sub(mul_karatsuba(x0x1, y0y1), z0), z2);
  BigInt res;
  res.a.resize(z2.a.size() + 2 * m, 0);
  for (size_t i = 0; i < z2.a.size(); ++i)
    res.a[i + 2 * m] += z2.a[i];
  res.a.resize(max(res.a.size(), z1.a.size() + m), 0);
  for (size_t i = 0; i < z1.a.size(); ++i)
    res.a[i + m] += z1.a[i];
  res.a.resize(max(res.a.size(), z0.a.size()), 0);
  for (size_t i = 0; i < z0.a.size(); ++i)
    res.a[i] += z0.a[i];
  uint64_t carry = 0;
  for (size_t i = 0; i < res.a.size(); ++i) {
    uint64_t cur = res.a[i] + carry;
    res.a[i] = cur % BigInt::BASE;
    carry = cur / BigInt::BASE;
  }
  if (carry)
    res.a.push_back(carry);
  res.trim();
  return res;
}

BigInt mul(const BigInt &x, const BigInt &y) {
  if (x.a.size() > 32 || y.a.size() > 32) {
    return mul_karatsuba(x, y);
  }
  BigInt res;
  res.a.assign(x.a.size() + y.a.size(), 0);
  for (size_t i = 0; i < x.a.size(); ++i) {
    uint64_t carry = 0;
    for (size_t j = 0; j < y.a.size() || carry; ++j) {
      uint64_t cur = res.a[i + j] +
                     (uint64_t)x.a[i] * (j < y.a.size() ? y.a[j] : 0) + carry;
      res.a[i + j] = cur % BigInt::BASE;
      carry = cur / BigInt::BASE;
    }
  }
  res.trim();
  return res;
}

pair<BigInt, BigInt> divmod(const BigInt &a, const BigInt &b) {
  BigInt cur, q;
  q.a.assign(a.a.size(), 0);
  for (int i = (int)a.a.size() - 1; i >= 0; --i) {
    cur.a.insert(cur.a.begin(), a.a[i]);
    cur.trim();
    uint32_t lo = 0, hi = BigInt::BASE - 1, best = 0;
    while (lo <= hi) {
      uint32_t mid = (lo + hi) >> 1;
      BigInt t = mul(b, BigInt(mid));
      if (cmp(t, cur) <= 0) {
        best = mid;
        lo = mid + 1;
      } else
        hi = mid - 1;
    }
    q.a[i] = best;
    cur = sub(cur, mul(b, BigInt(best)));
  }
  q.trim();
  cur.trim();
  return {q, cur};
}

BigInt gcd(BigInt a, BigInt b) {
  while (!b.isZero()) {
    BigInt r = divmod(a, b).second;
    a = b;
    b = r;
  }
  return a;
}

BigInt modMul(const BigInt &x, const BigInt &y, const BigInt &m) {
  return divmod(mul(x, y), m).second;
}

BigInt modPow(BigInt x, BigInt e, const BigInt &m) {
  BigInt res("1");
  while (!e.isZero()) {
    if (e.a[0] & 1) {
      res = modMul(res, x, m);
    }
    x = modMul(x, x, m);
    uint64_t carry = 0;
    for (int i = (int)e.a.size() - 1; i >= 0; --i) {
      uint64_t cur = e.a[i] + carry * BigInt::BASE;
      e.a[i] = cur >> 1;
      carry = cur & 1;
    }
    e.trim();
  }
  return res;
}

bool isProbablyPrime(const BigInt &n) {
  if (n.isZero()) {
    return false;
  }
  static const int bases[] = {2, 325, 9375, 28178, 450775, 9780504, 1795265022};
  if (n == BigInt("2"))
    return true;
  if (n.a[0] % 2 == 0)
    return false;

  BigInt d = sub(n, BigInt("1"));
  int s = 0;
  while (!d.isZero() && !(d.a[0] & 1)) {
    uint64_t carry = 0;
    for (int i = (int)d.a.size() - 1; i >= 0; --i) {
      uint64_t cur = d.a[i] + carry * BigInt::BASE;
      d.a[i] = cur >> 1;
      carry = cur & 1;
    }
    d.trim();
    ++s;
  }

  for (int a : bases) {
    if (a == 0)
      break;
    if (cmp(n, BigInt(a)) <= 0)
      return true;

    BigInt x = modPow(BigInt(a), d, n);
    if (x == BigInt("1") || x == sub(n, BigInt("1")))
      continue;

    bool comp = true;
    for (int r = 1; r < s; ++r) {
      x = modMul(x, x, n);
      if (x == sub(n, BigInt("1"))) {
        comp = false;
        break;
      }
    }
    if (comp) {
      return false;
    }
  }
  return true;
}

mt19937_64 rng(chrono::high_resolution_clock::now().time_since_epoch().count());

BigInt pollard(const BigInt &n) {
  if (isProbablyPrime(n)) {
    return n;
  }
  if (n == BigInt("4"))
    return BigInt("2");

  uniform_int_distribution<uint64_t> dist(1, BigInt::BASE - 1);
  while (true) {
    BigInt x = BigInt(dist(rng));
    BigInt y = x;
    BigInt c = BigInt(dist(rng));
    BigInt d("1");
    int iterations = 0;
    const int max_iter = 10000;
    while (d == BigInt("1") && iterations++ < max_iter) {
      x = divmod(add(modMul(x, x, n), c), n).second;
      y = divmod(add(modMul(y, y, n), c), n).second;
      y = divmod(add(modMul(y, y, n), c), n).second;
      BigInt diff = x < y ? sub(y, x) : sub(x, y);
      d = gcd(diff, n);
      if (d == n)
        break;
    }
    if (d != BigInt("1") && d != n) {
      return d;
    }
  }
}

void factor(BigInt n, map<string, int> &result) {
  if (n.isZero())
    return;

  for (int p : small_primes) {
    if (cmp(n, BigInt(p)) < 0)
      break;
    BigInt p_big(p);
    while (true) {
      auto qr = divmod(n, p_big);
      if (!qr.second.isZero())
        break;
      result[to_string(p)]++;
      n = qr.first;
      if (n.isZero())
        break;
    }
    if (n == BigInt(1))
      return;
  }

  stack<BigInt> st;
  st.push(n);

  while (!st.empty()) {
    BigInt x = st.top();
    st.pop();

    if (x == BigInt(1))
      continue;

    if (isProbablyPrime(x)) {
      result[x.toString()]++;
      continue;
    }

    BigInt d = pollard(x);
    if (d == BigInt("1")) {
      result[x.toString()]++;
      continue;
    }

    BigInt q = divmod(x, d).first;
    st.push(d);
    st.push(q);
  }
}

int main() {
  ios::sync_with_stdio(false);
  cin.tie(nullptr);

  init_small_primes();

  string s;
  cin >> s;
  BigInt N(s);

  map<string, int> factorsMap;
  if (N == BigInt("1")) {
    factorsMap["1"] = 1;
  } else {
    factor(N, factorsMap);
  }

  vector<pair<BigInt, int>> factors;
  for (auto &p : factorsMap) {
    factors.push_back({BigInt(p.first), p.second});
  }

  uint64_t total_divisors = 1;
  bool too_many = false;
  for (auto &pr : factors) {
    if (total_divisors > 9999) {
      too_many = true;
      break;
    }
    total_divisors *= (pr.second + 1);
  }
  if (total_divisors > 9999) {
    too_many = true;
  }

  vector<BigInt> divisors;
  if (!too_many) {
    divisors.push_back(BigInt("1"));
    for (auto &pr : factors) {
      BigInt p = pr.first;
      int exp = pr.second;
      vector<BigInt> new_divisors;
      BigInt cur("1");
      for (int e = 0; e <= exp; e++) {
        for (auto &d : divisors) {
          new_divisors.push_back(mul(d, cur));
        }
        if (e < exp) {
          cur = mul(cur, p);
        }
      }
      divisors = std::move(new_divisors);
    }
    sort(divisors.begin(), divisors.end(),
         [](const BigInt &a, const BigInt &b) { return a < b; });
  } else {
    sort(factors.begin(), factors.end(),
         [](const pair<BigInt, int> &a, const pair<BigInt, int> &b) {
           return a.first < b.first;
         });
    int k = factors.size();

    struct State {
      BigInt value;
      int last_index;
      vector<int> exponents;
      State(BigInt v, int li, vector<int> e)
          : value(std::move(v)), last_index(li), exponents(std::move(e)) {}
    };

    auto cmp = [](const State &a, const State &b) { return b.value < a.value; };
    priority_queue<State, vector<State>, decltype(cmp)> pq(cmp);

    vector<int> init_exponents(k, 0);
    pq.push(State(BigInt("1"), 0, init_exponents));

    while (!pq.empty() && divisors.size() < 9999) {
      State s = pq.top();
      pq.pop();
      divisors.push_back(s.value);

      if (divisors.size() >= 9999)
        break;

      for (int j = s.last_index; j < k; j++) {
        if (s.exponents[j] < factors[j].second) {
          vector<int> new_exponents = s.exponents;
          new_exponents[j]++;
          BigInt new_value = mul(s.value, factors[j].first);
          pq.push(State(std::move(new_value), j, std::move(new_exponents)));
        }
      }
    }
    sort(divisors.begin(), divisors.end(),
         [](const BigInt &a, const BigInt &b) { return a < b; });
  }

  cout << divisors.size() << "\n";
  for (auto &d : divisors) {
    cout << d.toString() << "\n";
  }

  return 0;
}