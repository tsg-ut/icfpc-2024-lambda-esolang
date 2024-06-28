import { inspect } from "util";

type Token = BooleanToken | IntegerToken | StringToken | UnaryToken | BinaryToken | IfToken | LambdaToken | VariableToken;
type Environment = Map<number, [Environment, Token]>;

interface BooleanToken { type: 'boolean'; value: boolean; }
interface IntegerToken { type: 'integer'; value: number; }
interface StringToken { type: 'string'; value: string; }
interface UnaryToken { type: 'unary'; operator: string; operand: Token; }
interface BinaryToken { type: 'binary'; operator: string; left: Token; right: Token; }
interface IfToken { type: 'if'; condition: Token; thenBranch: Token; elseBranch: Token; }
interface LambdaToken { type: 'lambda'; variable: number; body: Token; }
interface VariableToken { type: 'variable'; variable: number; }

function parse(tokens: string[]): Token {
  if (tokens.length === 0) throw new Error("No tokens to parse");
  const token = tokens.shift()!;
  const indicator = token[0];
  const body = token.slice(1);

  switch (indicator) {
    case 'T':
      return { type: 'boolean', value: true };
    case 'F':
      return { type: 'boolean', value: false };
    case 'I':
      return { type: 'integer', value: parseBase94(body) };
    case 'S':
      return { type: 'string', value: parseCustomString(body) };
    case 'U':
      return { type: 'unary', operator: body, operand: parse(tokens) };
    case 'B':
      return { type: 'binary', operator: body, left: parse(tokens), right: parse(tokens) };
    case '?':
      return { type: 'if', condition: parse(tokens), thenBranch: parse(tokens), elseBranch: parse(tokens) };
    case 'L':
      return { type: 'lambda', variable: parseBase94(body), body: parse(tokens) };
    case 'v':
      return { type: 'variable', variable: parseBase94(body) };
    default:
      throw new Error(`Unknown token indicator: ${indicator}`);
  }
}

function evaluate(token: Token, env: Environment = new Map()): any {
  // console.log('evaluate', token, env);
  switch (token.type) {
    case 'boolean':
      return token.value;
    case 'integer':
      return token.value;
    case 'string':
      return token.value;
    case 'unary':
      const operand = evaluate(token.operand, env);
      switch (token.operator) {
        case '-': return -operand;
        case '!': return !operand;
        case '#': return parseBase94(operand);
        case '$': return encodeBase94(operand);
        default: throw new Error(`Unknown unary operator: ${token.operator}`);
      }
    case 'binary':
      const left = evaluate(token.left, env);
      const right = evaluate(token.right, env);
      switch (token.operator) {
        case '+': return left + right;
        case '-': return left - right;
        case '*': return left * right;
        case '/': return Math.trunc(left / right);
        case '%': return left % right;
        case '<': return left < right;
        case '>': return left > right;
        case '=': return left === right;
        case '|': return left || right;
        case '&': return left && right;
        case '.': return left + right;
        case 'T': return right.substring(0, left);
        case 'D': return right.substring(left);
        case '$': {
          // console.log('apply', token.left, token.right, env);
          const leftToken = evaluate(token.left, env);
          // console.log('leftToken', leftToken);
          return leftToken(token.right, env);
        }
        default: throw new Error(`Unknown binary operator: ${token.operator}`);
      }
    case 'if':
      const condition = evaluate(token.condition, env);
      return evaluate(condition ? token.thenBranch : token.elseBranch, env);
    case 'lambda':
      return (arg: Token, lambdaEnv: Environment) => { 
        const tokenBody2 = alphaconvert(token.body);
        return evaluate(tokenBody2, new Map(env).set(token.variable, [lambdaEnv, arg]));
      };
    case 'variable':
      const tokenEnv = env.get(token.variable);
      if (!tokenEnv) {
        throw new Error(`Variable ${token.variable} not found in environment`);
      }
      const [lambdaEnv, arg] = tokenEnv;
      return evaluate(arg, lambdaEnv);
    default:
      console.log(token);
      throw new Error(`Unknown token type: ${(token as any).type}`);
  }
}

let idCounter = 1_000_000;
function id() {
  return idCounter++;
}

function alphaconvert(lambda: Token): Token {
  const variables = new Map<number, number>();
  function convert(token: Token, variables: Map<number, number>): Token {
    switch (token.type) {
      case 'unary':
        return { ...token, operand: convert(token.operand, variables) };
      case 'binary':
        return { ...token, left: convert(token.left, variables), right: convert(token.right, variables) };
      case 'if':
        return {
          ...token,
          condition: convert(token.condition, variables),
          thenBranch: convert(token.thenBranch, variables),
          elseBranch: convert(token.elseBranch, variables),
        };
      case 'lambda':
        const variable = token.variable;
        const newVariable = id();
        return {
          ...token,
          variable: newVariable,
          body: convert(token.body, new Map(variables).set(variable, newVariable)),
        };
      case 'variable':
        if (variables.has(token.variable)) {
          return { ...token, variable: variables.get(token.variable)! };
        }
        return token;
      case 'string':
      case 'boolean':
      case 'integer':
        return token;
      default:
        throw new Error(`Unknown token type: ${(token as any).type}`);
    }
  }
  return convert(lambda, variables);
}

function substitute(lambda: Token, arg: Token): Token {
  if (lambda.type === 'variable' && lambda.variable === arg.variable) {
    return arg;
  }
  switch (lambda.type) {
    case 'unary':
      return { ...lambda, operand: substitute(lambda.operand, arg) };
    case 'binary':
      return { ...lambda, left: substitute(lambda.left, arg), right: substitute(lambda.right, arg) };
    case 'if':
      return { ...lambda, condition: substitute(lambda.condition, arg), thenBranch: substitute(lambda.thenBranch, arg), elseBranch: substitute(lambda.elseBranch, arg) };
    case 'lambda':
      if (lambda.variable === arg.variable) {
        return lambda;
      } else {
        return { ...lambda, body: substitute(lambda.body, arg) };
      }
    case 'variable':
    case 'string':
    case 'boolean':
    case 'integer':
      return lambda;
    default:
      throw new Error(`Unknown token type: ${(lambda as any).type}`);
  }
}

function parseBase94(input: string): number {
  return [...input].reduce((acc, char) => acc * 94 + (char.charCodeAt(0) - 33), 0);
}

function encodeBase94(number: number): string {
  let result = '';
  while (number > 0) {
    result = String.fromCharCode((number % 94) + 33) + result;
    number = Math.trunc(number / 94);
  }
  return result || '!';
}

function parseCustomString(input: string): string {
  const customOrder = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!"#$%&\'()*+,-./:;<=>?@[\\]^_`|~ ';
  return [...input].map(char => customOrder[char.charCodeAt(0) - 33]).join('');
}

// Example usage:
const tokens = 'B$ L+ B. B. SF B$ B$ v+ Sl IR B$ B$ v+ B. S~ B$ B$ v+ Sl IS IR L" B$ L" B$ L# B$ v" B$ v# v# L# B$ v" B$ v# v# L$ L# ? B= v# I" v" B. v" B$ v$ B- v# I"'.split(" ");
// const tokens = "B$ B$ L# L$ v# B. SB%,,/ S}Q/2,$_ IK".split(" ");
const ast = parse(tokens);
console.log(inspect(ast, false, null, true));
console.log(evaluate(ast)); // Output: 5

/*
Y(f) = f(Y(f))
f(h, n) = n == 0 ? 1 : h(n - 1) * n
Y(f)(2)
= f(Y(f))(2)
= f(Y(f), 2)
= Y(f)(1) * 2
= f(Y(f))(1) * 2
= f(Y(f), 1) * 2
= Y(f)(0) * 1 * 2
= f(Y(f))(0) * 1 * 2
= f(Y(f), 0) * 1 * 2
= 1 * 1 * 2
*/
