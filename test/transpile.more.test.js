import { describe, it, expect } from 'vitest';
import { transpile } from '../index.js';

describe('additional transpile tests from index.html snippets', () => {
  it('transpiles exported function and consts (code2-like)', () => {
    const code = `export function add(x: number, y: number): number { return x + y; }
export const PI = 3.14;
let arr: number[] = [1,2,3]; arr = arr.map(x => x + 1);`;
    expect(() => transpile(code)).not.toThrow();
    const out = transpile(code);
    expect(out).toContain('add');
    expect(out).toContain('PI');
    expect(out).toContain('arr');
  });

  it('reports a type error when incompatible classes are assigned (code5-like)', () => {
    const code = `export class Dog { bark(): void { console.log("Woof!"); } }
export class Lab extends Dog { fetch(): void { console.log("Fetching..."); } }
let foo = new Lab(); foo.bark(); class Cat { meow(): void { console.log("Meow!"); } } foo = new Cat();`;
    // This intentionally causes a type-check failure: Cat is not compatible with Lab
    expect(() => transpile(code)).toThrow();
  });

  it('supports static methods and plain functions (code10-like)', () => {
    const code = `class Dog { static sleep(duration = 5): void { console.log('Sleeping'); } }
function meow(count = 1): void { console.log('meow x' + String(count)); }`;
    expect(() => transpile(code)).not.toThrow();
    const out = transpile(code);
    expect(out).toContain('Sleeping');
    expect(out).toContain('meow');
  });

  it('accepts union-of-arrays assignment (code11-like)', () => {
    const code = `type List = number[] | string[]; let l: List = [1,2]; l = ['s'];`;
    expect(() => transpile(code)).not.toThrow();
    const out = transpile(code);
    expect(out).toContain('let l');
  });

  it('transpiles template literals (code13-like)', () => {
    const code = "let name = 'bob'; let day = 'Monday'; const msg = `Hello ${name} Today is ${day}`;";
    expect(() => transpile(code)).not.toThrow();
    const out = transpile(code);
    expect(out).toContain('Hello');
    expect(out).toContain('Today');
  });

  it('throws for type-mismatch return (code15-like)', () => {
    const code = `function num(): number { return 's'; } let n: number = num();`;
    expect(() => transpile(code)).toThrow();
  });
});
