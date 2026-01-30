import { describe, it, expect } from 'vitest';
import { transpile } from '../index.js';

describe('Pick/Omit types', () => {
  it('allows Pick with string literal union keys', () => {
    const code = `
      type User = { id: number; name: string; active: boolean };
      type UserPreview = Pick<User, "id" | "name">;
      let u: UserPreview = { id: 1, name: "Ada" };
    `;
    expect(() => transpile(code)).not.toThrow();
    const out = transpile(code);
    expect(out).toContain('let u');
  });

  it('errors when accessing omitted property', () => {
    const code = `
      type User = { id: number; name: string; active: boolean };
      type UserNoActive = Omit<User, "active">;
      let u: UserNoActive = { id: 1, name: "Ada" };
      u.active = true;
    `;
    expect(() => transpile(code)).toThrow();
  });
});
