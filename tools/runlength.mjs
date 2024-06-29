import {evaluate} from './interpreter.js';

const inputEl = document.getElementById('input');
const inputSizeEl = document.getElementById('input-size');
const encodedEl = document.getElementById('encoded');
const encodedSizeEl = document.getElementById('encoded-size');

const runLengthDecoder = 'B$ B$ Lf B$ Lx B$ vf B$ vx vx Lx B$ vf B$ vx vx Lh La Ln ? B< vn I" va Lc ? B= vn I" B$ vh B. va vc B$ B$ B$ vh B. va vc B- vn I" vc S';
const permutation = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!"#$%&\'()*+,-./:;<=>?@[\\]^_`|~ \n';

function encodeString(input) {
  return 'S' + Array.from(input).map((char) => {
    const index = permutation.indexOf(char);
    return index === -1 ? '✕' : String.fromCodePoint(index + 33);
  }).join('');
};

function parseBase94(input) {
  return [...input].reduce((acc, char) => acc * 94n + (BigInt(char.charCodeAt(0)) - 33n), 0n);
}

function encodeBase94(number) {
  let result = '';
  while (number > 0) {
    result = String.fromCharCode(Number(number % 94n) + 33) + result;
    number = number / 94n;
  }
  return result === '' ? 'I!' : 'I' + result;
}

function runLengthEncode(str) {
  let encoded = [];
  let count = 1;
  for (let i = 0; i < str.length; i++) {
    if (str[i] === str[i + 1]) {
      count++;
    } else {
      encoded.push({ char: str[i], count });
      count = 1;
    }
  }
  return encoded;
}

// Combine succeeding characters with one repetition into one
function optimize(encoded) {
  let optimized = [];
  let cnt = '';
  for (const [i, { char, count }] of encoded.entries()) {
    if (count < 10) {
      cnt += char.repeat(count);
    } else {
      if (cnt.length > 0) {
        optimized.push({ char: cnt, count: 1 });
        cnt = '';
      }
      optimized.push({ char, count });
    }
  }
  if (cnt.length > 0) {
    optimized.push({ char: cnt, count: 1 });
  }
  return optimized;
}

function encode(input) {
  const encoded = runLengthEncode(input);
  const optimized = optimize(encoded);

  const tokens = [];
  for (const i of optimized.keys()) {
    tokens.push('B$');
    tokens.push('B$');
  }
  tokens.push('B$');
  tokens.push(runLengthDecoder);

  for (const {char, count} of optimized) {
    tokens.push(encodeBase94(BigInt(count)));
    tokens.push(encodeString(char));
  }

  tokens.push('I!');

  return tokens.join(' ');
}

function decode(input) {
  if (!input.startsWith('S')) {
    try {
      const result = evaluate(input);
      return result.toString();
    } catch (e) {
      return e.message;
    }
  }

  return Array.from(input.slice(1)).map((char) => {
    const index = char.codePointAt(0) - 33;
    return permutation[index] || '✕';
  }).join('');
};

inputEl.addEventListener('input', () => {
  const input = inputEl.value;
  inputSizeEl.textContent = input.length;
  encodedEl.value = encode(input);
  encodedSizeEl.textContent = encodedEl.value.length;
});

encodedEl.addEventListener('input', () => {
  const input = encodedEl.value;
  encodedSizeEl.textContent = input.length;
  inputEl.value = decode(input);
  inputSizeEl.textContent = inputEl.value.length;
});