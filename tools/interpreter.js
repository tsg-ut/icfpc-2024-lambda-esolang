// Run: npx tsx ./interpreter.ts
// import { inspect } from "util";
// import assert from 'assert';
const assert = (condition, message) => {
    if (!condition) {
        throw new Error(message);
    }
};
function parse(tokens) {
    if (tokens.length === 0)
        throw new Error("No tokens to parse");
    const token = tokens.shift();
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
            return { type: 'lambda', variable: Number(parseBase94(body)), body: parse(tokens) };
        case 'v':
            return { type: 'variable', variable: Number(parseBase94(body)) };
        default:
            throw new Error(`Unknown token indicator: ${indicator}`);
    }
}
function inspectToken(token) {
    switch (token.type) {
        case 'boolean':
            return token.value ? 'true' : 'false';
        case 'integer':
            return token.value.toString();
        case 'string':
            return JSON.stringify(token.value);
        case 'unary':
            return `(${token.operator} ${inspectToken(token.operand)})`;
        case 'binary':
            return `(${token.operator} ${inspectToken(token.left)} ${inspectToken(token.right)})`;
        case 'if':
            return `(if ${inspectToken(token.condition)} then ${inspectToken(token.thenBranch)} else ${inspectToken(token.elseBranch)})`;
        case 'lambda':
            return `(λ${token.variable} ${inspectToken(token.body)})`;
        case 'variable':
            return `v${token.variable}`;
        default:
            throw new Error(`Unknown token type: ${token.type}`);
    }
}
let evacn = 0;
function evaluateToken(token, env = new Map()) {
    if (evacn > 100) {
        process.exit(1);
    }
    // evacn += 1;
    // console.log('evaluate', inspectToken(token), env);
    switch (token.type) {
        case 'boolean':
            return token.value;
        case 'integer':
            return token.value;
        case 'string':
            return token.value;
        case 'unary':
            const operand = evaluateToken(token.operand, env);
            switch (token.operator) {
                case '-': {
                    assert(typeof operand === 'bigint', `Expected bigint operand for -, got ${typeof operand}`);
                    return -operand;
                }
                case '!': return !operand;
                case '#': {
                    assert(typeof operand === 'string', `Expected string operand for #, got ${typeof operand}`);
                    return parseBase94(operand);
                }
                case '$': {
                    assert(typeof operand === 'bigint', `Expected bigint operand for $, got ${typeof operand}`);
                    return encodeBase94(operand);
                }
                default: throw new Error(`Unknown unary operator: ${token.operator}`);
            }
        case 'binary':
            const left = evaluateToken(token.left, env);
            if (token.operator === "$") {
                // console.log('apply', inspectToken(token.left), inspectToken(token.right), env);
                const leftToken = evaluateToken(token.left, env);
                assert(typeof leftToken === 'function', `Expected function left operand for $, got ${typeof leftToken}`);
                // console.log('leftToken', leftToken);
                return leftToken(token.right, env);
            }
            const right = evaluateToken(token.right, env);
            switch (token.operator) {
                case '+': {
                    assert(typeof left === 'bigint' && typeof right === 'bigint', `Expected bigint operands for +, got ${typeof left} and ${typeof right}`);
                    return left + right;
                }
                case '-': {
                    assert(typeof left === 'bigint' && typeof right === 'bigint', `Expected bigint operands for -, got ${typeof left} and ${typeof right}`);
                    return left - right;
                }
                case '*': {
                    assert(typeof left === 'bigint' && typeof right === 'bigint', `Expected bigint operands for *, got ${typeof left} and ${typeof right}`);
                    return left * right;
                }
                case '/': {
                    assert(typeof left === 'bigint' && typeof right === 'bigint', `Expected bigint operands for /, got ${typeof left} and ${typeof right}`);
                    return left / right;
                }
                case '%': {
                    assert(typeof left === 'bigint' && typeof right === 'bigint', `Expected bigint operands for %, got ${typeof left} and ${typeof right}`);
                    return left % right;
                }
                case '<': return left < right;
                case '>': return left > right;
                case '=': return left === right;
                case '|': return left || right;
                case '&': return left && right;
                case '.': {
                    assert(typeof left === 'string' && typeof right === 'string', `Expected string operands for ., got ${typeof left} and ${typeof right}`);
                    return left + right;
                }
                case 'T': {
                    assert(typeof left === 'bigint', `Expected bigint left operand for T, got ${typeof left}`);
                    assert(typeof right === 'string', `Expected string right operand for T, got ${typeof right}`);
                    return right.substring(0, Number(left));
                }
                case 'D': {
                    assert(typeof left === 'bigint', `Expected bigint left operand for D, got ${typeof left}`);
                    assert(typeof right === 'string', `Expected string right operand for D, got ${typeof right}`);
                    return right.substring(Number(left));
                }
                default: throw new Error(`Unknown binary operator: ${token.operator}`);
            }
        case 'if':
            const condition = evaluateToken(token.condition, env);
            return evaluateToken(condition ? token.thenBranch : token.elseBranch, env);
        case 'lambda':
            return (arg, lambdaEnv) => {
                // console.log("evaluating lambda body",inspectToken(token));
                const token2 = alphaconvert({ ...token });
                assert(token2.type === "lambda");
                return evaluateToken(token2.body, new Map(env).set(token2.variable, [lambdaEnv, arg]));
            };
        case 'variable':
            const tokenEnv = env.get(token.variable);
            if (!tokenEnv) {
                throw new Error(`Variable ${token.variable} not found in environment`);
            }
            const [lambdaEnv, arg] = tokenEnv;
            return evaluateToken(arg, lambdaEnv);
        default:
            // console.log(token);
            throw new Error(`Unknown token type: ${token.type}`);
    }
}
let idCounter = 1_000_000;
function id() {
    return idCounter++;
}
function alphaconvert(lambda) {
    const variables = new Map();
    function convert(token, variables) {
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
                    return { ...token, variable: variables.get(token.variable) };
                }
                return token;
            case 'string':
            case 'boolean':
            case 'integer':
                return token;
            default:
                throw new Error(`Unknown token type: ${token.type}`);
        }
    }
    return convert(lambda, variables);
}
function substitute(lambda, arg) {
    if (lambda.type === 'variable') {
        assert(arg.type === 'variable');
        if (lambda.variable === arg.variable) {
            return arg;
        }
    }
    switch (lambda.type) {
        case 'unary':
            return { ...lambda, operand: substitute(lambda.operand, arg) };
        case 'binary':
            return { ...lambda, left: substitute(lambda.left, arg), right: substitute(lambda.right, arg) };
        case 'if':
            return { ...lambda, condition: substitute(lambda.condition, arg), thenBranch: substitute(lambda.thenBranch, arg), elseBranch: substitute(lambda.elseBranch, arg) };
        case 'lambda':
            assert(arg.type === 'variable');
            if (lambda.variable === arg.variable) {
                return lambda;
            }
            else {
                return { ...lambda, body: substitute(lambda.body, arg) };
            }
        case 'variable':
        case 'string':
        case 'boolean':
        case 'integer':
            return lambda;
        default:
            throw new Error(`Unknown token type: ${lambda.type}`);
    }
}
function parseBase94(input) {
    return [...input].reduce((acc, char) => acc * 94n + (BigInt(char.charCodeAt(0)) - 33n), 0n);
}
function encodeBase94(number) {
    let result = '';
    while (number > 0n) {
        result = String.fromCharCode(Number(number % 94n) + 33) + result;
        number = number / 94n;
    }
    return result || '!';
}
function parseCustomString(input) {
    const customOrder = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!"#$%&\'()*+,-./:;<=>?@[\\]^_`|~ ';
    return [...input].map(char => customOrder[char.charCodeAt(0) - 33]).join('');
}
function evaluate(input) {
    const tokens = input.split(/\s+/).filter((token) => token !== "");
    const ast = parse(tokens);
    return evaluateToken(ast);
}
export { evaluate, inspectToken, parse, evaluateToken, alphaconvert, substitute, parseBase94, encodeBase94, parseCustomString, };
// Example usage:
// const tokens = 'B$ L+ B. B. SF B$ B$ v+ Sl IR B$ B$ v+ B. S~ B$ B$ v+ Sl IS IR L" B$ L" B$ L# B$ v" B$ v# v# L# B$ v" B$ v# v# L$ L# ? B= v# I" v" B. v" B$ v$ B- v# I"'.split(" ");
// const tokens = 'B$ B$ Lf B$ Lx B$ vf B$ vx vx Lx B$ vf B$ vx vx Lh Ln ? B= vn I! I" B* B$ vh B- vn I" vn I&'.split(" ");
// const tokens = 'B$ Lf B$ Lx B$ vf B$ vx vx Lx B$ vf B$ vx vx Lh Ln ? B= vn I! I" B* B$ vh B- vn I" vn'.split(" ");
// const ast = parse(tokens);
// console.log(evaluateToken(ast)); // Output: 5
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
