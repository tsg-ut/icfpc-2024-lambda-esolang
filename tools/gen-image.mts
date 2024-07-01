import sharp from 'sharp';
import fs from 'fs/promises';

const pixels = (await fs.readFile('../testcases/lambdaman20', 'utf-8')).split('\n').map((row) => (
  row.split('').map((cell) => (
    cell === '.' ? 0 : 1
  ))
));

const width = pixels[0].length;
const height = pixels.length;

const data = new Uint8Array(width * height * 3);
for (let y = 0; y < height; y++) {
  for (let x = 0; x < width; x++) {
    const i = (y * width + x) * 3;
    data[i] = data[i + 1] = data[i + 2] = pixels[y][x] * 255;
  }
}

await sharp(Buffer.from(data), { raw: { width, height, channels: 3 } }).toFile('image.png');
