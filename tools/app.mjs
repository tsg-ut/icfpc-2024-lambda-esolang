import {evaluate} from './interpreter.js';

const inputEl = document.getElementById('input');
const inputSizeEl = document.getElementById('input-size');
const encodedEl = document.getElementById('encoded');
const encodedSizeEl = document.getElementById('encoded-size');
const sendToUniverseEl = document.getElementById('send-to-universe');
const resultEl = document.getElementById('result');
const decodedEl = document.getElementById('decoded');

const permutation = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!"#$%&\'()*+,-./:;<=>?@[\\]^_`|~ \n';

const encode = (input) => {
  return 'S' + Array.from(input).map((char) => {
    const index = permutation.indexOf(char);
    return index === -1 ? '✕' : String.fromCodePoint(index + 33);
  }).join('');
};

const decode = (input) => {
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

resultEl.addEventListener('input', () => {
  const input = resultEl.value;
  decodedEl.value = decode(input);
});

decodedEl.addEventListener('input', () => {
  const input = decodedEl.value;
  resultEl.value = encode(input);
});

sendToUniverseEl.addEventListener('click', async () => {
  sendToUniverseEl.disabled = true;

  const encoded = encodedEl.value;

  const req = await fetch('/send', {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
    },
    body: JSON.stringify({message: encoded}),
  });

  const response = await req.json();

  console.log(response);

  resultEl.value = response.result;

  decodedEl.value = decode(response.result);

  sendToUniverseEl.disabled = false;
});

