#include <bits/extc++.h>

int main() {
    using namespace std;
    vector<pair<long, long>> pts;
    {
        long x, y;
        while (cin >> x >> y)
            pts.emplace_back(x, y);
    }

    static constexpr unsigned long beam_size{256}, flush_rate{8};

    const auto make_moves{[](auto points) {
        using velocity = pair<long, long>;
        using possible_velocity = array<long, 4>;

        struct state {
            long now_x, now_y;
            unsigned long move_count, previous_count;
            possible_velocity possible_v;
            // vector<unsigned> visit_order;

            unsigned previous_state_idx, idx;

            bool reference;

            bool operator<(const state &rhs) {
                auto &&[lvx, lvy, uvx, uvy]{possible_v};
                auto &&[r_lvx, r_lvy, r_uvx, r_uvy]{rhs.possible_v};
                return move_count == rhs.move_count ? (uvx - lvx) * (uvy - lvy) > (r_uvx - r_lvx) * (r_uvy - r_lvy) : move_count < rhs.move_count;
            }
        };

        vector<state> pool;

        pool.emplace_back(0, 0, 0, 0, possible_velocity{0, 0, 0, 0}, 0, 0, true);

        // const auto flush_pool{[](auto &&pool, auto &&pq) {
        //     for (auto &&s: pool)
        //         s.reference = false;

        //     vector<unsigned> pq_idx;

        //     while (!empty(pq)) {
        //         pq_idx.emplace_back(pq.top());
        //         pq.pop();
        //     }

        //     for (auto &&i: pq_idx)
        //         pool[i].reference = true;

        //     for (const auto &s: pool | views::reverse)
        //         pool[s.previous_state_idx].reference |= s.reference;

        //     map<unsigned, unsigned> mp;

        //     for (unsigned i{}, j{}; auto &&s : pool){
        //         if (s.reference) {
        //             s.idx = j;
        //             mp[i] = j;
        //             assert(mp.contains(s.previous_state_idx));
        //             s.previous_state_idx = mp[s.previous_state_idx];
        //             ++j;
        //         }
        //         ++i;
        //     }

        //     erase_if(pool, [](auto &&x) { return !x.reference; });

        //     for (auto i: pq_idx)
        //         pq.emplace(mp[i]);
        // }};
        
        const int N = size(points);

        const auto cmp{[&pool](auto i, auto j) { return pool[i] < pool[j]; }};
        using my_pq = priority_queue<unsigned, vector<unsigned>, decltype(cmp)>;
        // array<my_pq, 2> _pq{my_pq{cmp}, my_pq{cmp}};
        // vector<my_pq> _pq(size(points) + 1, my_pq{cmp});
        vector<vector<my_pq>> _pq(1 << N, vector<my_pq>(N, my_pq{cmp}));

        _pq[0][0].emplace(0);

        // long nx{}, ny{};
        // unsigned parity{};
        // for (unsigned idxx{}; const auto &[x, y] : points){
        vector<unsigned> bit_perm(1 << N);
        iota(begin(bit_perm), end(bit_perm), 0);
        stable_sort(begin(bit_perm), end(bit_perm), [&](auto i, auto j) {
            return __builtin_popcount(i) < __builtin_popcount(j);
        });

        for (unsigned bit{}; bit < (1 << N); ++bit) {
        // for (const auto bit : bit_perm) {
            cerr << "bit: " << bit << " pool size: " << size(pool) << endl;
        for (unsigned idxx{}; idxx < N; ++idxx) {
            if (bit & (1 << idxx))
                continue;
            const auto &[x, y]{points[idxx]};

            // if (idxx % 100 == 0) cerr << '*';
            // cerr << "[" << x << " " << y << "]" << endl;
            // const auto dx{x - nx}, dy{y - ny};
            // parity ^= 1;
            auto &&pq{_pq[bit | (1 << idxx)][idxx]}; // auto &&pq{_pq[parity]};
            
            for (unsigned idx_prv{}; idx_prv < N; ++idx_prv) {
                if (!((bit == 0 && idx_prv == 0) || ((1 << idx_prv) & bit)))
                    continue;
                auto prv{_pq[bit][idx_prv]};
                // cerr << idx_prv << " -> " << idxx << endl;

                const auto first_size{size(pool)};
                while (!empty(prv)) {
                    const auto [now_x, now_y, cnt, _pcnt, pos_v, _pidx, idx, rf]{pool[prv.top()]};
                    const auto [lvx, lvy, uvx, uvy]{pos_v};
                    const auto dx{x - now_x}, dy{y - now_y};
                    // auto new_visit_ord{visit_ord};
                    // new_visit_ord.emplace_back(idxx);

                    // cerr << "  " << "[" << _now_x << " " << _now_y << "] " << cnt << " " << pcnt << " [" << lvx << " " << uvx << "] [" << lvy << " " << uvy << "]" << endl;
                    for (long k{}; cnt + k < (pq.size() < beam_size ? cnt + 100000 : pool[pq.top()].move_count); ++k) {
                        if (k * lvx - k * (k + 1) / 2 <= dx && dx <= k * uvx + k * (k + 1) / 2 && k * lvy - k * (k + 1) / 2 <= dy &&
                            dy <= k * uvy + k * (k + 1) / 2) {
                            const auto calc_next_lu{[](long l, long u, long k, long to) {
                                const auto lower_b{[&]{
                                    vector<long> lower(k + 1);
                                    auto L{l - k}, R{u + k + 1};
                                    while(L + 1 < R){
                                        const auto M{(L + R) / 2};
                                        lower[0] = M;
                                        for (unsigned i{1}; i <= k; ++i)
                                            lower[i] = clamp(lower[i - 1] - 1, l - k + i, u + k - i);
                                        (reduce(begin(lower), end(lower) - 1) <= to ? L : R) = M;
                                    }
                                    return L;
                                }()};
                                const auto upper_b{[&]{
                                    vector<long> upper(k + 1);
                                    auto L{l - k - 1}, R{u + k};
                                    while(L + 1 < R){
                                        const auto M{(L + R) / 2};
                                        upper[0] = M;
                                        for (unsigned i{1}; i <= k; ++i)
                                            upper[i] = clamp(upper[i - 1] + 1, l - k + i, u + k - i);
                                        (to <= reduce(begin(upper), end(upper) - 1) ? R : L) = M;
                                    }
                                    return R;
                                }()};
                                auto [na, nb]{ranges::minmax({lower_b, upper_b})};
                                return make_pair(na, nb);
                            }};
                            const auto &[next_lvx, next_uvx]{calc_next_lu(lvx, uvx, k, dx)};
                            const auto &[next_lvy, next_uvy]{calc_next_lu(lvy, uvy, k, dy)};
                            pool.emplace_back(x, y, cnt + k, k, possible_velocity{next_lvx, next_lvy, next_uvx, next_uvy}, idx, size(pool), true);
                            pq.emplace(size(pool) - 1);
                            while (size(pq) > beam_size)
                                pq.pop();
                        }
                    }
                    prv.pop();
                }
            }
            
            // nx = x;
            // ny = y;
            // ++idxx;
            // if (bit > *(&bit + 1))
            //     flush_pool(pool, pq);
        }}
        // auto &&pq{_pq[size(points)]};
        // auto &&prv{_pq[size(points) - 1]};
        my_pq pq{cmp};
        for (int i{}; i < N; ++i) {
            auto prv{_pq[(1 << N) - 1][i]};
            while (!empty(prv)) {
                const auto idx{prv.top()};
                prv.pop();
                pq.emplace(idx);

                if (prv.size() == 1) {
                    state now_state{pool[prv.top()]};

                    // for (unsigned i : now_state.visit_order)
                    //     cerr << i << " -> ";
                    // cerr << endl;
                }
            }
        }
        // auto &&pq{_pq[(1 << N) - 1][N - 1]};

        while (size(pq) > 1)
            pq.pop();

        velocity now_v{[](auto &&x) {
            auto &&[lvx, lvy, uvx, uvy]{x};
            return velocity((uvx + lvx) / 2, (uvy + lvy) / 2);
        }(pool[pq.top()].possible_v)};
        auto &&[vx, vy]{now_v};

        const auto solve{[](long d, long l, long u, long n) {
            vector<long> lower(d + 1), upper(d + 1);
            vector<long> result;
            for (long j{1}; j <= d; ++j) {
                lower[j] = upper[j] = clamp(lower[j - 1], l - (d - j), u + (d - j));
                for (long i{j + 1}; i <= d; ++i) {
                    lower[i] = clamp(lower[i - 1] - 1, l - (d - i), u + (d - i));
                    upper[i] = clamp(upper[i - 1] + 1, l - (d - i), u + (d - i));
                }
                auto ls{reduce(begin(lower), end(lower) - 1)}, us{reduce(begin(upper), end(upper) - 1)};
                if (n < ls)
                    lower[j] = upper[j] = clamp(lower[j - 1] - 1, l - (d - j), u + (d - j));
                else if (us < n)
                    lower[j] = upper[j] = clamp(lower[j - 1] + 1, l - (d - j), u + (d - j));
                result.emplace_back(lower[j] - lower[j - 1]);
            }
            return result;
        }};

        vector<unsigned> result;
        points.emplace(begin(points));

        state now_state{pool[pq.top()]};

        // for (unsigned i : now_state.visit_order)
        //     cerr << i << " -> ";
        // cerr << endl;

        while (now_state.previous_state_idx != now_state.idx) {
            const auto d{now_state.previous_count};

            auto x{now_state.now_x}, y{now_state.now_y};
            auto &&prev_state{pool[now_state.previous_state_idx]};
            auto next_x{prev_state.now_x}, next_y{prev_state.now_y};

            auto [lx, ly, ux, uy]{prev_state.possible_v};

            next_x += vx * d;
            next_y += vy * d;
            next_x -= x;
            next_y -= y;
            lx -= vx;
            ly -= vy;
            ux -= vx;
            uy -= vy;

            tie(lx, ly, ux, uy) = make_tuple(-ux, -uy, -lx, -ly);

            const auto xr{solve(d, lx, ux, next_x)}, yr{solve(d, ly, uy, next_y)};

            for (long i{}; i < d; ++i) {
                result.emplace_back(5 + xr[i] + 3 * yr[i]);
                vx -= xr[i];
                vy -= yr[i];
            }

            now_state = prev_state;
        }

        ranges::reverse(result);
        return result;
    }};

    cerr << "there are " << size(pts) << " points." << endl;

    const auto &moves{make_moves(pts)};

    cerr << string(40, '-') << endl;

    /* cerr << "moves:" << endl;

    long x{}, y{}, dx{}, dy{};
    for (const auto m: moves) {
        (dx += ((m - 1) % 3)) -= 1;
        (dy += ((m - 1) / 3)) -= 1;
        x += dx;
        y += dy;
        cerr << x << " " << y << " (" << dx << " " << dy << ")" << '\n';
    }
    cerr << endl;*/

    cerr << size(moves) << endl;

    for (const auto m: moves)
        cout << m;
    cout << endl;
    return 0;
}
