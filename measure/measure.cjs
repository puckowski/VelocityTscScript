const { transpile } = require("./index"); // or "./index.js" if you prefer

// assume `code` is a string defined somewhere above:
// const code = "...your TS-like source...";

const code = `
// Primitive and object types
type UserId = number;
type Point = { x: number, y: number };
type Point3D = { x: number, y: number, z: number };

// Function using object type and property access
function getX(p: Point): number {
  return p.x;
}

// Structural compatibility: Point3D is assignable to Point
function len2D(p: Point): number {
  // pretend we use p.x, p.y here
  return 0;
}

let p3: Point3D = get3D(); // get3D is untyped JS; ignore for now
let x: number = getX(p3); // OK: Point3D has x,y

// Type error: accessing missing property
function bad(p: Point): number {
  return p.z; // error: z does not exist
}

// Type error: wrong return type
function bad2(p: Point): string {
  return p.x; // error: number not assignable to string
}

const p4: Point3D = get3D();
const x2: number = getX(p4);
if (x2 == 5) {
    console.log("x2 is 5");
}
while (x2 < 10) {
    x2 = x2 + 1;
    if (x2 < 7) {
        console.log("x2 is less than 7");
    }
}
let arr: number[] = [1, 2, 3];
let arr2: string[] = [ "a", "b", "c" ];
arr[0] = 's';
arr = arr2; // error: string[] not assignable to number[]
arr = arr.map(x => x + 1);
for (let i = 0; i < arr.length; i = i + 1) {
    console.log(arr[i]);
}
for(let i of arr) {
    console.log(i);
}`;

const start = performance.now();
try {
transpile(code);
} catch (err) {
    console.error("Error during transpilation:", err);
}
const end = performance.now();

console.log(`Pure transpile time: ${(end - start).toFixed(2)} ms`);