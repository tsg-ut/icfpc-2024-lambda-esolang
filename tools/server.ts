// Run: npx tsx watch server.ts

import Fastify from 'fastify';
import fastifyStatic from '@fastify/static';
import storage from 'node-persist';
import {Mutex} from 'async-mutex';
import fastifyView from '@fastify/view';
import ejs from 'ejs';
import {evaluate} from './interpreter.ts';
import fs from 'fs/promises';
import path from 'path';

import 'dotenv/config';

const permutation = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!"#$%&\'()*+,-./:;<=>?@[\\]^_`|~ \n';

const mutex = new Mutex();

storage.init();

const baseUrl = 'https://boundvariable.space/communicate';

const testcaseConfig: [string, number][] = [
  ['lambdaman', 21],
  ['spaceship', 25],
];

const testcases = testcaseConfig.flatMap(([name, length]) => (
  Array.from({ length }, (_, i) => ( 
    `${name}${i + 1}`
  ))
));

const logResult = async (
  {
    payload,
    response,
    decodedPayload,
    decodedResponse,
  }: {
    payload: string,
    response: string,
    decodedPayload: string,
    decodedResponse: string,
  }
) => {
  mutex.runExclusive(async () => {
    const logs = await storage.getItem('logs') || [];
    logs.push({
      payload,
      response,
      decodedPayload,
      decodedResponse,
      timestamp: new Date().toISOString(),
    });
    await storage.setItem('logs', logs);
  });
};

const decode = (input: string) => {
  if (!input.startsWith('S')) {
    try {
      const result = evaluate(input);
      return result.toString();
    } catch (e) {
      return e.message;
    }
  }

  return Array.from(input.slice(1)).map((char) => {
    const index = char.codePointAt(0)! - 33;
    return permutation[index] || '✕';
  }).join('');
};

const server = Fastify({
  logger: {
    transport: {
      target: 'pino-pretty',
      options: {
        translateTime: 'HH:MM:ss Z',
        ignore: 'pid,hostname',
      },
    },
  },
});

server.register(fastifyStatic, {
  root: __dirname,
});

server.register(fastifyView, {
  engine: { ejs },
})

server.get('/', async (request, reply) => {
  const logs = await storage.getItem('logs') || [];
  logs.sort((a, b) => new Date(b.timestamp).getTime() - new Date(a.timestamp).getTime());
  return reply.view('index.ejs', { logs });
});

server.get('/testcases.html', async (request, reply) => {
  if (typeof (request.query as any).case === 'string') {
    const caseName = (request.query as any).case;
    const index = testcases.indexOf(caseName);
    if (index === -1) {
      reply.code(404);
      return 'Not found';
    }
    const logs = await storage.getItem('logs') || [];
    const filteredLogs = logs.filter(({decodedResponse}) => (
      decodedResponse?.includes(`solved ${caseName}`)
    ));

    const content = await fs.readFile(path.join(__dirname, '..', 'testcases', caseName), 'utf-8');

    return reply.view('testcases.ejs', { logs: filteredLogs, caseName, testcases, content });
  } else {
    return reply.view('testcases.ejs', { logs: [], caseName: null, testcases });
  }
});

server.post('/send', async (request, reply) => {
  const { message } = request.body as any;
  server.log.info(`Message received: ${message}`);

  const response = await fetch(baseUrl, {
    method: 'POST',
    headers: {
      'Content-Type': 'text/plain',
      Authorization: `Bearer ${process.env.TOKEN}`,
    },
    body: message,
  });

  const text = await response.text();

  try {
    const decodedPayload = decode(message);
    const decodedResponse = decode(text);
    logResult({
      payload: message,
      response: text,
      decodedPayload,
      decodedResponse,
    });
  } catch (e) {
    server.log.error(e);
  }

  return { result: text };
});

server.listen({ port: 36728 }, (err, address) => {
  if (err) {
    console.error(err);
    process.exit(1);
  }
  console.log(`Server listening at ${address}`);
});
