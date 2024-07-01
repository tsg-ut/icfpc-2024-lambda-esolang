/*

pseudo-code:

walk := (i, n) -> {
  n times {
    print("URDL"[i % 4] * n)
  }
}

cycle := (i, n) -> {
  if (n > 0) {
    walk(i, n)
    
    cycle(i+3, n/4)
    
    walk(i, n)
    
    cycle(i-1, n/2)
    walk(i+1, 1)
    cycle(i, n/2)
    walk(i+2, 1)
    cycle(i+1, n/2)
    walk(i+3, 1)
    
    walk(i+2, n-1)
    
    if (n > 1) {
      walk(i+1, 1)
      cycle(i+1, n/4)
      walk(i+3, 1)
    }
    
    walk(i+2, n)
  }
}

cycle(0, 32)
walk(2, 1)
cycle(1, 32)
walk(3, 1)
cycle(2, 32)
walk(0, 1)
cycle(3, 32)
walk(1, 1)
*/

import fs from 'fs/promises';

const ret: string[] = [];

const walk = (i: number, n: number) => {
  for (let j = 0; j < n; j++) {
    ret.push("URDL"[((i % 4) + 4) % 4]);
  }
};

const cycle = (i: number, n: number) => {
  if (n > 0) {
    walk(i, n);
    
    cycle(i + 3, Math.floor(n / 4));
    
    walk(i, n);
    
    cycle(i - 1, Math.floor(n / 2));
    walk(i + 1, 1)
    cycle(i, Math.floor(n / 2));
    walk(i + 2, 1)
    cycle(i + 1, Math.floor(n / 2));
    walk(i + 3, 1)

    walk(i + 2, n - 1);
    
    if (n > 1) {
      walk(i + 1, 1);
      cycle(i + 1, Math.floor(n / 4));
      walk(i + 3, 1)
    }
    
    walk(i + 2, n);
  }
};

cycle(0, 32);
walk(2, 1);
cycle(1, 32);
walk(3, 1);
cycle(2, 32);
walk(0, 1);
cycle(3, 32);
walk(1, 1);

console.log(ret.join(''));

let playerX = 0;
let playerY = 0;

let itemsCnt = 0;

const map = (await fs.readFile('../testcases/lambdaman20', 'utf-8')).trim().split('\n').map((row, y) => (
  row.split('').map((cell, x) => {
    if (cell === '#') {
      return 2;
    }
    if (cell === '.') {
      itemsCnt++;
      return 1;
    }
    if (cell === 'L') {
      playerX = x;
      playerY = y;
    }
    return 0;
  })
));

const dx = [0, 1, 0, -1];
const dy = [-1, 0, 1, 0];

let dir = 0;

for (const c of ret) {
  if (c === 'U') dir = 0;
  if (c === 'R') dir = 1;
  if (c === 'D') dir = 2;
  if (c === 'L') dir = 3;
  

  if (playerX + dx[dir] < 0 || playerX + dx[dir] >= map[0].length || playerY + dy[dir] < 0 || playerY + dy[dir] >= map.length) {
    console.log('NG: out of map');
    continue;
  }

  if (map[playerY + dy[dir]][playerX + dx[dir]] !== 2) {
    playerX += dx[dir];
    playerY += dy[dir];
    if (map[playerY][playerX] === 1) {
      itemsCnt--;
    }
    map[playerY][playerX] = 0;
  }
}

console.log(`itemsCnt: ${itemsCnt}`);
console.log(itemsCnt === 0 ? 'OK' : 'NG');

console.log('Map:');

for (const row of map) {
  console.log(row.map(cell => ' .#'[cell]).join(''));
}



