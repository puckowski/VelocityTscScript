// minify.js
const { spawnSync } = require("child_process");

const input = "index.js";
const output = "index.min.js";

const args = [
  input,
  "--compress",
  "--mangle",
  "--output",
  output
];

console.log(`🔧 Minifying ${input} -> ${output} ...`);

const result = spawnSync("npx", ["terser", ...args], {
  stdio: "inherit",
  shell: true
});

if (result.status !== 0) {
  console.error("❌ Minification failed");
  process.exit(result.status || 1);
}

console.log("✅ Minification complete:", output);
