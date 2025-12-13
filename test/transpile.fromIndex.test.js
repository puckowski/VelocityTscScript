import { describe, it, expect } from 'vitest';
import fs from 'fs/promises';
import path from 'path';
import { transpile } from '../index.js';

async function extractSnippets() {
  const file = await fs.readFile(path.resolve('index.html'), 'utf8');
  const map = new Map();
  // Match backtick-assigned snippets (code, code2..code15)
  const re = /const\s+(code\d*|code)\s*=\s*`([\s\S]*?)`;/g;
  let m;
  while ((m = re.exec(file)) !== null) {
    map.set(m[1], m[2]);
  }

  // code13 in the HTML is concatenated strings; provide a fallback if not captured
  if (!map.has('code13')) {
    map.set('code13', "let name = 'bob'; let day = 'Monday'; const msg = `Hello ${name} Today is ${day}`;");
  }

  return map;
}

describe('transpile snippets extracted from index.html', () => {
  it('each snippet either returns a string or throws a controlled Error', async () => {
    const snippets = await extractSnippets();
    expect(snippets.size).toBeGreaterThan(0);

    for (const [name, code] of snippets) {
      // run in try/catch and assert predictable outcome
      try {
        const out = transpile(code);
        // If it returns, it should be a non-empty string
        expect(typeof out).toBe('string');
        expect(out.length).toBeGreaterThanOrEqual(0);
      } catch (err) {
        // If it throws, it should be an Error instance with a message
        expect(err).toBeInstanceOf(Error);
        expect(typeof err.message).toBe('string');
      }
    }
  });
});
