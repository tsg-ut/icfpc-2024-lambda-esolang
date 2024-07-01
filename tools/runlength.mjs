import {evaluate} from './interpreter.js';

const runLengthDecoder = 'B$ B$ Lf B$ Lx B$ vf B$ vx vx Lx B$ vf B$ vx vx Lh La Ln ? B< vn I" va Lc ? B= vn I" B$ vh B. va vc B$ B$ B$ vh B. va vc B- vn I" vc S';
const rludDecoder = 'B$ B$ L2 L3 B$ B$ v2 v2 v3 L0 L1 ? B> v1 I! B. BT I" BD B% v1 I% SFLO> B$ B$ v0 v0 B/ v1 I% S';
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

function runlengthTokensEncode(str) {
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

function runlengthEncode(input) {
  const encoded = runlengthTokensEncode(input);
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

function rludBodyEncode(input) {
  const reversedInput = Array.from(input).reverse();

  if (reversedInput[0] === 'L') {
    return 'Input cannot end with `L`';
  }

  let acc = 0n;
  for (const char of reversedInput) {
    const n = 'LRUD'.indexOf(char);
    if (n === -1) {
      return `Invalid character: ${char}`;
    }
    acc = acc * 4n + BigInt(n);
  }

  return rludDecoder + ' ' + encodeBase94(acc);
}

function rludEncode(input) {
  const tokens = [];

  let acc = '';
  let mode = 'LRUD'.includes(input[0]) ? 'lrud' : 'raw';

  for (const char of Array.from(input)) {
    if (mode === 'lrud') {
      if ('LRUD'.includes(char)) {
        acc += char;
      } else {
        const trailingLs = acc.match(/(L*)$/)[0];
        tokens.push(rludBodyEncode(acc.slice(0, acc.length - trailingLs.length)));
        mode = 'raw';
        acc = trailingLs + char;
      }
    } else {
      if ('LRUD'.includes(char)) {
        tokens.push(encodeString(acc));
        acc = char;
        mode = 'lrud';
      } else {
        acc += char;
      }
    }
  }

  if (mode === 'lrud' && acc !== '') {
    const trailingLs = acc.match(/(L*)$/)[0];
    tokens.push(rludBodyEncode(acc.slice(0, acc.length - trailingLs.length)));
    console.log(acc.slice(0, -trailingLs.length));
    mode = 'raw';
    acc = trailingLs;
  }
  if (mode === 'raw' && acc !== '') {
    tokens.push(encodeString(acc));
  }

  return Array(tokens.length - 1).fill('B.').join(' ') + ' ' + tokens.join(' ');
}

{
  const form = document.getElementById('runlength');

  const inputEl = form.getElementsByClassName('input')[0];
  const inputSizeEl = form.getElementsByClassName('input-size')[0];
  const encodedEl = form.getElementsByClassName('encoded')[0];
  const encodedSizeEl = form.getElementsByClassName('encoded-size')[0];

  inputEl.addEventListener('input', () => {
    const input = inputEl.value;
    inputSizeEl.textContent = input.length;
    encodedEl.value = runlengthEncode(input);
    encodedSizeEl.textContent = encodedEl.value.length;
  });

  encodedEl.addEventListener('input', () => {
    const input = encodedEl.value;
    encodedSizeEl.textContent = input.length;
    inputEl.value = decode(input);
    inputSizeEl.textContent = inputEl.value.length;
  });
}

{
  const form = document.getElementById('rlud');

  const inputEl = form.getElementsByClassName('input')[0];
  const inputSizeEl = form.getElementsByClassName('input-size')[0];
  const encodedEl = form.getElementsByClassName('encoded')[0];
  const encodedSizeEl = form.getElementsByClassName('encoded-size')[0];

  inputEl.addEventListener('input', () => {
    const input = inputEl.value;
    inputSizeEl.textContent = input.length;
    encodedEl.value = rludEncode(input);
    encodedSizeEl.textContent = encodedEl.value.length;
  });

  encodedEl.addEventListener('input', () => {
    const input = encodedEl.value;
    encodedSizeEl.textContent = input.length;
    inputEl.value = decode(input);
    inputSizeEl.textContent = inputEl.value.length;
  });
}
