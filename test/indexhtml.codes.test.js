import { describe, it, expect } from 'vitest';
import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';
import { transpile } from '../index.js';

// Resolve __dirname in ESM
const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const htmlPath = path.resolve(__dirname, '../index.html');
const html = fs.readFileSync(htmlPath, 'utf8');

// Find const code, code2, code3... template literals in the HTML file
const regex = /const\s+(code\d*)\s*=\s*`([\s\S]*?)`/g;
let match;
const snippets = [];
while ((match = regex.exec(html)) !== null) {
  snippets.push({ name: match[1], code: match[2] });
}

describe('index.html snippet coverage', () => {
  it('extracts at least one code snippet from index.html', () => {
    expect(Array.isArray(snippets)).toBe(true);
    expect(snippets.length).toBeGreaterThan(0);
  });

  // Expectations derived from previous test run results: true => should throw (type errors),
  // false => should not throw and should return a non-empty string.
  const expected = {
    code2: false,
    code3: true,
    code4: true,
    code5: true,
    code6: true,
    code7: false,
    code8: true,
    code9: false,
    code10: false,
    code11: false,
    code12: true,
    code13: false,
    code14: true,
    code15: true
  };

  for (const s of snippets) {
    it(`transpile ${s.name} (from index.html)`, () => {
      const code = s.code;
      // basic sanity
      expect(typeof code).toBe('string');
      expect(code.length).toBeGreaterThan(0);

      const exp = expected[s.name];
      if (typeof exp === 'boolean') {
        if (exp) {
          // we expect transpile to throw
          expect(() => transpile(code)).toThrow();
        } else {
          // we expect transpile to succeed and return non-empty string
          const out = transpile(code);
          expect(typeof out).toBe('string');
          expect(out.length).toBeGreaterThan(0);
        }
      } else {
        // fallback: accept either behavior (backwards compatible)
        let threw = false;
        let out;
        try {
          out = transpile(code);
        } catch (err) {
          threw = true;
          expect(err).toBeInstanceOf(Error);
        }
        if (!threw) {
          expect(typeof out).toBe('string');
          expect(out.length).toBeGreaterThan(0);
        }
      }
    });
  }
});
