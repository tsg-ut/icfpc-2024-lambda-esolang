/*
# Syntax

A 3D program is a text file that represents a 2D grid of cells. The rows are separated by newlines. Within each row, cells are represented by non-whitespace tokens separated by whitespace.

It is human-friendly but not mandatory to keep the columns aligned by adding extra whitespace between cells; each row is processed separately. Rows do not have to have the same length; short rows are assumed to be left-aligned: empty on the right.

## Available tokens

* . represents an empty cell
* an integer between -99 and 99 (inclusive) represents that integer
* the following characters
  <, >, ^, v, +, -, *, /, %, @, =, #, S, A, B
  represent operators.
  See [the operator reference](#operator-reference) for their meaning.

# Semantics

The board is an infinite 2D grid of cells. Each cell is either empty or contains a value. A value is either an arbitrarily sized integer or an operator.

Programs cannot contain arbitrarily sized integer literals. Integer literals can only range between -99 and 99, inclusive. This is a source-code-only restriction. At runtime, programs are free to compute with integers of arbitrary size.

## Basic 2D reduction

Time flows in discrete units, called ticks. The initial board is identical to the source code and its time coordinate is t=1. With each tick, we perform one round of reductions across the whole board simultaneously.

[The operator reference](#operator-reference) defines the reduction rules of all operators. Generally, all operators perform local rewriting on their surroundings. For example, the "move right" operator > rewrites x > . to . > x.

Operators are values, too, so + > . reduces to . > +. This way, it's also possible to shuffle operators around.

Binary operators, like +, *, or -, rewrite like this:
. y .     .  .   .
x - .  ~> .  -  x-y
. . .     . x-y  .


Operators A and B are replaced with the input values (if any) after parsing. This mechanism is used to give different inputs to your program.

There is operator S, which you can overwrite to terminate the program and submit the answer. It is an error to submit multiple different values, submitting the same value simultaneously multiple times is fine.

Some reduction principles:

1. If the preconditions of an operator are not met, reduction simply does not take place. For example, if a binary operator
   has only one operand available, things stay as they are until the other operand arrives.

2. Outputs of operators overwrite the output cells.

   1 > + reduces to . > 1

3. Reading a value removes/consumes it.

4. Two operators can read from the same input cell at the same time. Both operators receive a copy of the input value before it's removed from the board.

   . < 6 > . reduces to 6 < . > 6

5. Conflicting writes into the same cell like 3 > . < 3 or 3 > . < 4 are disallowed and will crash the simulation.

6. In every tick, all reads (and removals) happen before all the writes.

   1 > 2 > . reduces to . > 1 > 2

### Operator reference

In the diagrams below, the symbol . generally stands for an empty cell or a non-empty cell containing any value. We use the dot instead of metavariables for readability.

Arrows move values of any type (integers or operators).
. < x   ~>   x < .         x > .   ~>   . > x


  .            x             x            .
  ^     ~>     ^             v     ~>     v
  x            .             .            x


Binary arithmetic operators reduce only for integer arguments. They write their outputs both to the right and below at the same time.
. y .        .  .  .       . y .        .  .  .
x + .   ~>   .  + x+y      x * .   ~>   .  * x*y
. . .        . x+y .       . . .        . x*y .


. y .        .  .  .
x - .   ~>   .  - x-y
. . .        . x-y .


Operators / and % represent the quotient and remainder operations: operator / truncates the result towards zero;
x%y has the same sign as x.

. y .        .  .  .       . y .        .  .  .
x / .   ~>   .  / x/y      x % .   ~>   .  % x%y
. . .        . x/y .       . . .        . x%y .


Equality comparison reduces only if its two operands are equal. It works for both integers and operators.

. x .        . . .         . y .        . y .
x = .   ~>   . = x         x = .   ~>   x = .  (if x!=y)
. . .        . x .         . . .        . . .


Dually, the not-equal operator reduces only when the operands are not equal:
. x .        . x .         . y .        . . .
x # .   ~>   x # .         x # .   ~>   . # y  (if x!=y)
. . .        . . .         . . .        . x .


Operators A and B have no reduction rules defined. They may appear in the program code but they are replaced with the input values (if any) immediately after parsing.

Operator S ("submit") does not have any reduction rules defined, either. The program submits its results by overwriting operator S with the result value.

The time warp operator is described in [its own section](#time-warp).

## Scoring

Your score is the total spacetime volume of the program:
* maximal X coordinate ever used minus minimal X coordinate ever used + 1
* times (maximal Y coordinate ever used minus minimal Y coordinate ever used + 1)
* times (maximal T coordinate ever used minus minimal T coordinate ever used + 1)

In this definition, "ever used" ranges across the entire simulation
and across all time warps.

## Limits

After 1_000_000 ticks, the program is terminated without submitting a value, regardless of its current time coordinate.

## Time warp

You may be able to reduce the time complexity of your program, defined as the maximum time coordinate reached, using time travel. Time travel is triggered with the (quaternary) warp operator:

.  v  .
dx  @ dy
 . dt  .


This rolls back the history of the board by dt time steps, writes value v into the cell with coordinates (-dx, -dy) relative to the @ operator (note the negative signs!) on this past board, which means that the time coordinate of the target board is unchanged but the content is mutated. Then simulation then restarts from the modified point onward.

The minimal value of dt is 1, which means stepping back one time step.

2 > . .        . > 2 .        2 > . .
. 2 @ 0   ~>   . 2 @ 0   ~>   2 2 @ 0
. . 1 .        . . 1 .        . . 1 .


### Time travel principles

1. Time is discrete and starts with t=1 with the initial board.

2. Each tick, time t increases by 1, and the board is changed according to the action of all its operators.

3. The time warp operator rolls back time to any point in the past, up to and including t=1 (the initial board).

4. Time warping with dt=0 is not allowed.

4. After time warping to time t, the history before t is preserved but the future after t is discarded and its new version will be recomputed again.

5. If two different warp operators attempt to write different values into the same destination cell at the same destination time, the simulation will crash.

   Writing the same value into the same cell is fine, as is writing different values into different cells.

6. If two different warp operators attempt to travel to different times in the same tick, the simulation will crash.

7. As soon as the submit operator is overwritten, the entire simulation stops.

   A board can contain multiple submit operators but if more than one are overwritten at the same time, the simulation will crash.

8. If no operator on a board can reduce, the simulation terminates without submitting an answer.

# Example

As an example, the following program computes A * B by time-looping B times, adding A every time (for the sake of the example; of course there is also is the * operator):

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


When running this for A = 3 and B = 4, this program has a spacetime volume of 320 (vx * vy * vt = 8 * 10 * 4 = 320). You can see the execution trace at [3d-example].
*/

const assert: (condition: boolean, message?: string) => asserts condition = (condition: boolean, message?: string): asserts condition => {
  if (!condition) {
    throw new Error(message);
  }
};

type Cell = number | string | null;
type Board = Cell[][];
interface Point {
  x: number;
  y: number;
}

interface WarpOperation {
  dx: number;
  dy: number;
  dt: number;
  v: Cell;
}

interface Operation {
  op: string,
  x: number,
  y: number,
  inputs: number[],
  sources: Point[],
  targets: Point[],
}

interface HistoricalState {
  board: Board;
  t: number;
  tick: number;
  isBurnt: boolean;
}

const pointEquals = (a: Point, b: Point): boolean => a.x === b.x && a.y === b.y;

class Simulator {
  board: Board;
  t: number;
  tick: number;
  inputA: Cell;
  inputB: Cell;
  history: HistoricalState[];
  submitValue: number | null = null;

  constructor(board: string, inputA: Cell, inputB: Cell) {
    this.board = this.parseBoard(board);
    this.t = 1;
    this.tick = 1;
    this.inputA = inputA;
    this.inputB = inputB;
    this.history = [];
    this.replaceInputs();
  }

  private parseBoard(board: string): Board {
    return board.trim().split('\n').map(row =>
      row.split(/\s+/).map(cell => {
        if (cell === '.') return null;
        const intVal = parseInt(cell, 10);
        return isNaN(intVal) ? cell : intVal;
      })
    );
  }

  private replaceInputs(): void {
    this.board = this.board.map(row =>
      row.map(cell => {
        if (cell === 'A') return this.inputA;
        if (cell === 'B') return this.inputB;
        return cell;
      })
    );
  }

  private getCell(x: number, y: number): Cell {
    const cell = this.board[y]?.[x];
    if (cell === undefined) {
      throw new Error(`Access Violation: Cell (${x}, ${y}) is out of bounds`);
    }
    return cell;
  }

  private setCell(x: number, y: number, value: Cell): void {
    assert(this.board[y] !== undefined, `Access Violation: Cell (${x}, ${y}) is out of bounds`);
    assert(this.board[y][x] !== undefined, `Access Violation: Cell (${x}, ${y}) is out of bounds`);

    if (this.board[y][x] === 'S') {
      assert(typeof value === 'number', `Submission Error: Attempted to submit a non-number value`);
      assert(this.submitValue === null, `Submission Error: Attempted to submit conflicting values`);
      this.submitValue = value;
    }

    this.board[y][x] = value;
  }

  private simulateTick(): number | null {
    this.history.push({
      board: JSON.parse(JSON.stringify(this.board)),
      t: this.t,
      tick: this.tick,
      isBurnt: false,
    });

    const scheduledOps: Operation[] = [];
    const writeCells: Point[] = [];

    // Read operations and schedule them
    for (let y = 0; y < this.board.length; y++) {
      for (let x = 0; x < this.board[y].length; x++) {
        const cell = this.board[y][x];
        if (typeof cell === 'string') {
          const operation = this.readOperation(x, y);
          if (operation) {
            scheduledOps.push(operation);
            for (const point of operation.targets) {
              if (writeCells.some(p => pointEquals(p, point))) {
                throw new Error(`Conflicting writes at (${point.x}, ${point.y})`);
              }
              writeCells.push(point);
            }
          }
        }
      }
    }

    if (scheduledOps.length === 0) {
      throw new Error('No operations scheduled');
    }

    // Remove inputs
    for (const op of scheduledOps) {
      this.removeOperationInputs(op);
    }

    const willSubmitWritten = writeCells.some(point => this.getCell(point.x, point.y) === 'S');

    // Write operations
    const warpOperations = scheduledOps.filter(op => op.op === '@');
    if (!willSubmitWritten && warpOperations.length > 0) {
      const dtSet = new Set(warpOperations.map(op => op.inputs[2]));
      assert(dtSet.size === 1, `Multiple warp operations with different dt: (${[...dtSet]})`);

      const dt = dtSet.values().next().value;

      assert(dt > 0, `Invalid time warp: dt must be greater than 0, but found ${dt}`);

      for (const [i, op1] of warpOperations.entries()) {
        const targetX1 = op1.x - op1.inputs[0];
        const targetY1 = op1.y - op1.inputs[1];
        for (const j of Array(i).keys()) {
          const op2 = warpOperations[j];
          const targetX2 = op2.x - op2.inputs[0];
          const targetY2 = op2.y - op2.inputs[1];
          assert(
            (targetX1 !== targetX2 || targetY1 !== targetY2) ||
            (op1.inputs[3] === op2.inputs[3]),
            `Conflicting warp operations at (${targetX1}, ${targetY1})`
          );
        }
      }

      const warpTargetBoardIndex = this.history.findIndex(state => (
        state.t === this.t - dt && !state.isBurnt
      ));
      assert(warpTargetBoardIndex !== -1, `Invalid time warp: no valid target board found`);

      for (let t = warpTargetBoardIndex + 1; t < this.history.length; t++) {
        this.history[t].isBurnt = true;
      }

      this.board = JSON.parse(JSON.stringify(this.history[warpTargetBoardIndex].board));
      this.t -= dt;

      for (const op of warpOperations) {
        this.applyWarpOperation(op);
      }
    } else {
      for (const op of scheduledOps) {
        if (op.op === '@') {
          continue;
        }
        this.applyOperator(op);
      }

      this.t++;
    }

    this.tick++;

    if (this.submitValue !== null) {
      return this.submitValue;
    }

    return null;
  }

  private readOperation(x: number, y: number): Operation | null {
    const cell = this.getCell(x, y);
    if (typeof cell !== 'string') return null;

    const op = cell;

    switch (op) {
      case '>':
      case '<':
      case 'v':
      case '^':
        const source = {
          x: x + (op === '<' ? 1 : op === '>' ? -1 : 0),
          y: y + (op === '^' ? 1 : op === 'v' ? -1 : 0),
        };
        const target = {
          x: x + (op === '>' ? 1 : op === '<' ? -1 : 0),
          y: y + (op === 'v' ? 1 : op === '^' ? -1 : 0),
        };
        const input = this.getCell(source.x, source.y);
        if (typeof input === 'number') {
          return {
            op,
            x,
            y,
            inputs: [input],
            sources: [source],
            targets: [target],
          }
        }
        return null
      case '+':
      case '-':
      case '*':
      case '/':
      case '%': {
        const left = this.getCell(x - 1, y);
        const right = this.getCell(x, y - 1);
        if (typeof left === 'number' && typeof right === 'number') {
          if (op === '/' && right === 0) {
            return null;
          }
          if (op === '%' && right === 0) {
            return null;
          }
          return {
            op,
            x,
            y,
            inputs: [left, right],
            sources: [{ x: x - 1, y }, { x, y: y - 1 }],
            targets: [{ x: x + 1, y }, { x, y: y + 1 }],
          };
        }
        return null;
      }
      case '=': {
        const left = this.getCell(x - 1, y);
        const right = this.getCell(x, y - 1);
        if (typeof left === 'number' && typeof right === 'number' && left === right) {
          return {
            op,
            x,
            y,
            inputs: [left, left],
            sources: [{ x: x - 1, y }, { x, y: y - 1 }],
            targets: [{ x: x + 1, y }, { x, y: y + 1 }],
          };
        }
        return null;
      }
      case '#': {
        const left = this.getCell(x - 1, y);
        const right = this.getCell(x, y - 1);
        if (typeof left === 'number' && typeof right === 'number' && left !== right) {
          return {
            op,
            x,
            y,
            inputs: [right, left],
            sources: [{ x: x - 1, y }, { x, y: y - 1 }],
            targets: [{ x: x + 1, y }, { x, y: y + 1 }],
          };
        }
        return null
      }
      case '@': {
        const dx = this.getCell(x - 1, y);
        const dy = this.getCell(x + 1, y);
        const dt = this.getCell(x, y + 1);
        const v = this.getCell(x, y - 1);
        if (typeof dx === 'number' && typeof dy === 'number' && typeof dt === 'number' && typeof v === 'number') {
          return {
            op,
            x,
            y,
            inputs: [dx, dy, dt, v],
            sources: [{ x: x - 1, y }, { x: x + 1, y }, { x, y: y + 1 }, { x, y: y - 1 }],
            targets: [],
          };
        }
        return null
      }
      case 'S':
        return null
      default:
        throw new Error(`Unknown operator: ${op}`);
    }
  }

  private removeOperationInputs(op: Operation): void {
    for (let i = 0; i < op.sources.length; i++) {
      const { x, y } = op.sources[i];
      const cell = this.getCell(x, y);
      assert(typeof cell !== 'string', `Cell (${x}, ${y}) is expected to be a number, but found ${cell}`);
      this.setCell(x, y, null);
    }
  }

  private applyOperator(op: Operation): void {
    switch (op.op) {
      case '>':
      case '<':
      case 'v':
      case '^': {
        const {x, y} = op.targets[0];
        this.setCell(x, y, op.inputs[0]);
        break;
      }
      case '+':
      case '-':
      case '*':
      case '/':
      case '%': {
        const result = this.calculateBinaryOperator(op);
        for (const {x, y} of op.targets) {
          this.setCell(x, y, result);
        }
        break;
      }
      case '=':
      case '#': {
        for (const [i, {x, y}] of op.targets.entries()) {
          this.setCell(x, y, op.inputs[i]);
        }
        break;
      }
      default:
        throw new Error(`Unexpected operator: ${op.op}`);
    }
  }

  private calculateBinaryOperator(op: Operation): number | null {
    const [left, right] = op.inputs;
    switch (op.op) {
      case '+':
        return left + right;
      case '-':
        return left - right;
      case '*':
        return left * right;
      case '/':
        return Math.trunc(left / right);
      case '%':
        return left % right;
      default:
        throw new Error(`Unexpected operator: ${op.op}`);
    }
  }


  private scheduleWarpOperation(x: number, y: number, newBoard: Board, warpOps: WarpOperation[]): void {
    const dx = this.getCell(x - 1, y);
    const dy = this.getCell(x + 1, y);
    const dt = this.getCell(x, y + 1);
    const v = this.getCell(x, y - 1);
    if (typeof dx === 'number' && typeof dy === 'number' && typeof dt === 'number') {
      warpOps.push({ dx, dy, dt, v });
    }
  }

  private applyWarpOperation(op: Operation): void {
    const [dx, dy, dt, v] = op.inputs;
    const targetX = op.x - dx;
    const targetY = op.y - dy;
    this.setCell(targetX, targetY, v);
  }

  private submit(x: number, y: number, newBoard: Board): void {
    const value = this.getCell(x - 1, y);
    if (value !== null) {
      console.log(`Submit: ${value}`);
      process.exit(0);
    }
  }

  run(maxTicks: number = 1_000_000): number {
    console.log('Tick 1 (t = 1):');
    this.logBoard();
    
    while (this.tick <= maxTicks) {
      const submitValue = this.simulateTick();

      console.log(`Tick ${this.tick} (t = ${this.t}):`);
      this.logBoard();
      
      if (submitValue !== null) {
        return submitValue;
      }
    }
    throw new Error('Step limit exceeded without submitting a value');
  }

  formatBoard(): string {
    return this.board.map(row => row.map(cell => cell === null ? '.' : cell).join(' ')).join('\n');
  }

  logBoard(): void {
    console.log(this.formatBoard() + '\n');
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
const simulator = new Simulator(program, 10, 3);
const result = simulator.run();

console.log(`Result: ${result}`);
