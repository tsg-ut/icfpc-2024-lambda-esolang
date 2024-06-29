type Cell = number | string;
type Board = Cell[][];

interface WarpOperation {
  dx: number;
  dy: number;
  dt: number;
  v: Cell;
}

class Simulator {
  board: Board;
  t: number;
  inputA: Cell;
  inputB: Cell;
  history: Board[];

  constructor(board: string, inputA: Cell, inputB: Cell) {
    this.board = this.parseBoard(board);
    this.t = 1;
    this.inputA = inputA;
    this.inputB = inputB;
    this.history = [];
    this.replaceInputs();
  }

  parseBoard(board: string): Board {
    return board.split('\n').map(row =>
      row.split(/\s+/).map(cell => {
        if (cell === '.') return cell;
        const intVal = parseInt(cell, 10);
        return isNaN(intVal) ? cell : intVal;
      })
    );
  }

  replaceInputs(): void {
    this.board = this.board.map(row =>
      row.map(cell => {
        if (cell === 'A') return this.inputA;
        if (cell === 'B') return this.inputB;
        return cell;
      })
    );
  }

  getCell(x: number, y: number): Cell {
    return this.board[y]?.[x] ?? '.';
  }

  setCell(x: number, y: number, value: Cell): void {
    if (!this.board[y]) this.board[y] = [];
    this.board[y][x] = value;
  }

  simulateTick(): void {
    this.history.push(JSON.parse(JSON.stringify(this.board)));
    const newBoard: Board = JSON.parse(JSON.stringify(this.board));

    const warpOps: WarpOperation[] = [];

    for (let y = 0; y < this.board.length; y++) {
      for (let x = 0; x < this.board[y].length; x++) {
        const cell = this.board[y][x];
        if (typeof cell === 'string') {
          this.applyOperator(x, y, cell, newBoard, warpOps);
        }
      }
    }

    for (const warpOp of warpOps) {
      this.applyWarpOperation(warpOp);
    }

    this.board = newBoard;
    this.t++;
  }

  applyOperator(x: number, y: number, op: string, newBoard: Board, warpOps: WarpOperation[]): void {
    switch (op) {
      case '>':
        this.move(x, y, x + 1, y, newBoard);
        break;
      case '<':
        this.move(x, y, x - 1, y, newBoard);
        break;
      case 'v':
        this.move(x, y, x, y + 1, newBoard);
        break;
      case '^':
        this.move(x, y, x, y - 1, newBoard);
        break;
      case '+':
      case '-':
      case '*':
      case '/':
      case '%':
      case '=':
      case '#':
        this.applyBinaryOperator(x, y, op, newBoard);
        break;
      case '@':
        this.scheduleWarpOperation(x, y, newBoard, warpOps);
        break;
      case 'S':
        this.submit(x, y, newBoard);
        break;
    }
  }

  move(x1: number, y1: number, x2: number, y2: number, newBoard: Board): void {
    const value = this.getCell(x1, y1);
    if (value !== '.') {
      newBoard[y1][x1] = '.';
      newBoard[y2][x2] = value;
    }
  }

  applyBinaryOperator(x: number, y: number, op: string, newBoard: Board): void {
    const [left, right] = [this.getCell(x - 1, y), this.getCell(x + 1, y)];
    if (typeof left === 'number' && typeof right === 'number') {
      let result;
      switch (op) {
        case '+':
          result = left + right;
          break;
        case '-':
          result = left - right;
          break;
        case '*':
          result = left * right;
          break;
        case '/':
          result = Math.trunc(left / right);
          break;
        case '%':
          result = left % right;
          break;
        case '=':
          result = left === right ? left : '.';
          break;
        case '#':
          result = left !== right ? left : '.';
          break;
      }
      newBoard[y][x] = op;
      newBoard[y + 1][x] = result;
      newBoard[y][x + 1] = result;
      newBoard[y][x - 1] = '.';
      newBoard[y][x + 1] = '.';
    }
  }

  scheduleWarpOperation(x: number, y: number, newBoard: Board, warpOps: WarpOperation[]): void {
    const dx = this.getCell(x - 1, y);
    const dy = this.getCell(x + 1, y);
    const dt = this.getCell(x, y + 1);
    const v = this.getCell(x, y - 1);
    if (typeof dx === 'number' && typeof dy === 'number' && typeof dt === 'number') {
      warpOps.push({ dx, dy, dt, v });
    }
  }

  applyWarpOperation({ dx, dy, dt, v }: WarpOperation): void {
    const targetT = this.t - dt;
    if (targetT < 1) return;

    const targetBoard = JSON.parse(JSON.stringify(this.history[targetT - 1]));
    const x = -dx;
    const y = -dy;
    targetBoard[y][x] = v;
    this.history[targetT - 1] = targetBoard;

    this.t = targetT;
    this.board = targetBoard;
  }

  submit(x: number, y: number, newBoard: Board): void {
    const value = this.getCell(x - 1, y);
    if (value !== '.') {
      console.log(`Submit: ${value}`);
      process.exit(0);
    }
  }

  run(maxTicks: number = 1_000_000): void {
    while (this.t <= maxTicks) {
      this.simulateTick();
      if (!this.board.some(row => row.includes('S'))) {
        console.log('Simulation ended without submission');
        break;
      }
    }
  }
}

// Example usage:
const program = `
. . . . 0 . . . .
. B > . = . . . .
. v 1 . . > . . .
. . - . . . + S .
. . . . . ^ . . .
. . v . . 0 > . .
. . . . . . A + .
. 1 @ 6 . . < . .
. . 3 . 0 @ 3 . .
. . . . . 3 . . .
`;
const simulator = new Simulator(program, 3, 4);
simulator.run();
