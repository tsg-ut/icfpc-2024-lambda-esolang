const inputEl = document.getElementById('input');
const encodedEl = document.getElementById('encoded');

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

inputEl.addEventListener('input', () => {
  const input = inputEl.value;
  try {
    const inputNumber = BigInt(input);
    encodedEl.value = encodeBase94(inputNumber);
  } catch (e) {
    encodedEl.value = e.message;
  }
});

encodedEl.addEventListener('input', () => {
  const encoded = encodedEl.value;
  const encodedBody = encoded.startsWith('I') ? encoded.slice(1) : encoded;
  const number = parseBase94(encodedBody);
  inputEl.value = number.toString();
});