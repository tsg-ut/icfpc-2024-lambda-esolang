#include <iostream>
#include <vector>
#include <string>
#include <queue>
#include <set>
#include <cstring>
#include <unordered_map>

using namespace std;

struct pair_hash {
    template <class T1, class T2>
    std::size_t operator () (const std::pair<T1,T2> &pair) const {
        auto hash1 = std::hash<T1>{}(pair.first);
        auto hash2 = std::hash<T2>{}(pair.second);
        return hash1 ^ hash2; // Combine the two hash values. (Note: This is a simple way; for better distribution, consider using a more complex method)
    }
};

const int MAXN = 1005; // Adjust this based on grid size

int n, m; // dimensions of the grid
char grid[MAXN][MAXN]; // grid storage
bool visited[MAXN][MAXN]; // visited state of each cell
set<pair<int, int>> cellsToVisit; // set of cells to visit
unordered_map<pair<int, int>, pair<int, int>, pair_hash> parent; // parent map for BFS tree

// Direction vectors for moving up, down, left, right
int dx[] = {-1, 0, 1, 0};
int dy[] = {0, -1, 0, 1};
string directions = "ULDR"; // corresponding directions in the output path

void bfs(pair<int, int> start) {
    queue<pair<int, int>> q;
    q.push(start);
    parent[start] = start;
    
    while (!q.empty()) {
        pair<int, int> current = q.front();
        q.pop();
        
        int x = current.first;
        int y = current.second;
        
        for (int dir = 0; dir < 4; ++dir) {
            int nx = x + dx[dir];
            int ny = y + dy[dir];
            
            if (nx >= 0 && nx < n && ny >= 0 && ny < m && grid[nx][ny] == '.' && parent.find({nx, ny}) == parent.end()) {
                parent[{nx, ny}] = {x, y};
                q.push({nx, ny});
            }
        }
    }
}

// DFS function to find a path visiting all '.' cells exactly once
bool dfs(int x, int y, int index, string& path) {
    // cout << "visiting " << x << " " << y << endl;
    // cout << "remaining " << cellsToVisit.size() << endl;
    
    visited[x][y] = true;
    cellsToVisit.erase({x, y});
    
    if (cellsToVisit.size() == 0) {
        // All cells visited, return true
        return true;
    }

    for (int dir = 0; dir < 4; ++dir) {
        int nx = x + dx[dir];
        int ny = y + dy[dir];

        // cout << "trying " << nx << " " << ny << endl;

        pair<int, int> node_parent = parent[{nx, ny}];
        
        if (nx >= 0 && nx < n && ny >= 0 && ny < m && !visited[nx][ny] && grid[nx][ny] != '#' && node_parent.first == x && node_parent.second == y) {
            path.push_back(directions[dir]);
            if (dfs(nx, ny, index + 1, path)) {
                return true;
            }
            path.push_back(directions[(dir + 2) % 4]);
        }
    }
    
    visited[x][y] = false;
    return false;
}

// Function to find the starting position 'L' and '.' cells
pair<int, int> findStartAndCells() {
    pair<int, int> start;
    
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            if (grid[i][j] == 'L') {
                start = {i, j};
            } else if (grid[i][j] == '.') {
                cellsToVisit.insert({i, j});
            }
        }
    }
    
    return start;
}

int main() {
    // Read input
    string line;
    vector<string> input;
    
    while (getline(cin, line)) {
        input.push_back(line);
    }
    
    n = input.size();
    m = input[0].size();
    
    // Initialize the grid
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            grid[i][j] = input[i][j];
        }
    }
    
    // Find the starting position 'L' and all '.' cells
    pair<int, int> start = findStartAndCells();

    // Perform BFS to find the shortest path to all '.' cells
    bfs(start);
    
    // Initialize visited array
    memset(visited, false, sizeof(visited));
    
    // Starting DFS from position 'L'
    string path;
    dfs(start.first, start.second, 0, path);
    
    // Output the path found
    cout << path << endl;
    
    return 0;
}
