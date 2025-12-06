// Function using object type and property access
function getX(p) {
    return p.x;
}
// Structural compatibility: Point3D is assignable to Point
function len2D(p) {
    // pretend we use p.x, p.y here
    return 0;
}
var p3 = get3D(); // get3D is untyped JS; ignore for now
var x = getX(p3); // OK: Point3D has x,y
// Type error: accessing missing property
function bad(p) {
    return p.z; // error: z does not exist
}
// Type error: wrong return type
function bad2(p) {
    return p.x; // error: number not assignable to string
}
var p4 = get3D();
var x2 = getX(p4);
if (x2 == 5) {
    console.log("x2 is 5");
}
while (x2 < 10) {
    x2 = x2 + 1;
    if (x2 < 7) {
        console.log("x2 is less than 7");
    }
}
var arr = [1, 2, 3];
var arr2 = ["a", "b", "c"];
arr[0] = 's';
arr = arr2; // error: string[] not assignable to number[]
arr = arr.map(function (x) { return x + 1; });
for (var i = 0; i < arr.length; i = i + 1) {
    console.log(arr[i]);
}
for (var _i = 0, arr_1 = arr; _i < arr_1.length; _i++) {
    var i = arr_1[_i];
    console.log(i);
}
