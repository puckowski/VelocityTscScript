const ts = require("typescript");

const start = performance.now();
const program = ts.createProgram(["index2.ts"], { outDir: "dist" });
program.emit();
const end = performance.now();

console.log(`Pure TS compile time: ${(end - start).toFixed(2)} ms`);