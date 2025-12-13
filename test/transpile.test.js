import { describe, it, expect } from 'vitest';
import { transpile } from '../index.js';

describe('transpile (basic snippets from index.html)', () => {
  it('returns a string for a simple arrow function', () => {
    const code = `const double = x => x * 2;`;
    const out = transpile(code);
    expect(typeof out).toBe('string');
    expect(out.length).toBeGreaterThan(0);
    // output should mention the function/variable name at least
    expect(out).toContain('double');
  });

  it('handles a simple class with method', () => {
    const code = `class Dog { bark() { console.log("Woof!"); } }`;
    const out = transpile(code);
    expect(typeof out).toBe('string');
    // ensure transpilation didn't remove the console.log call text
    expect(out).toContain('Woof');
    expect(out).toContain('console');
  });

  it('removes TypeScript-only type declarations', () => {
    const code = `type T = number; let x: T = 1;`;
    const out = transpile(code);
    expect(typeof out).toBe('string');
    // type declarations should not be present literally in the output
    expect(out).not.toContain('type T');
    // variable should still exist
    expect(out).toContain('let x');
  });

  it('transpiles larger snippet without throwing', () => {
    const code = `const arr = [1,2,3]; let arr2 = arr.map(x => x + 1); const double = x => x*2;`;
    expect(() => transpile(code)).not.toThrow();
    const out = transpile(code);
    expect(out).toContain('arr');
  });
});
