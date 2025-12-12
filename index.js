"use strict";

/**
 * Tiny TS-like compiler with:
 * - type Alias = number;
 * - type Point = { x: number, y: number };
 * - let x: Alias = 1;
 * - function f(a: number, b: number): number { ... }
 * - Property access: p.x checked against object type { x: ... }
 *
 * Now also supports:
 * - Object literals: { x: 1, y: 2 } with structural typing.
 *
 * Now supports:
 * - Classic for loops: for (init; cond; update) { ... }
 *   - init can be: let / const decl (with or without type), or an expression
 *   - Any of init / cond / update may be omitted: for (;;), for (; cond;), etc.
 * - for-of loops: for (let x of arr) { ... } (optional type annotation)
 * - for-in loops: for (let k in obj) { ... } (very loose typing for keys)
 * - Postfix update expressions: x++, x--
 *
 * And a more JS-friendly type checker:
 * - Unknown values (e.g. console, get3D) are treated as `any`
 * - Property access and calls on `any` are allowed
 * - Special-case: array.length is `number`
 */

/* =============== LEXER =============== */

function tokenize(input) {
    const tokens = [];
    let i = 0;
    const isAlpha = c => /[a-zA-Z_]/.test(c);
    const isDigit = c => /[0-9]/.test(c);
    const isAlphaNum = c => isAlpha(c) || isDigit(c);

    while (i < input.length) {
        let c = input[i];

        if (c === " " || c === "\t" || c === "\n" || c === "\r") {
            i++;
            continue;
        }

        // Comments: // ...
        if (c === "/" && input[i + 1] === "/") {
            while (i < input.length && input[i] !== "\n") i++;
            continue;
        }

        // String literals (" or ')
        if (c === '"' || c === "'") {
            const quote = c;
            let start = i++;
            let value = "";
            while (i < input.length && input[i] !== quote) {
                if (input[i] === "\\") {
                    if (i + 1 >= input.length) break; // unterminated
                    const esc = input[i + 1];
                    // handle common escapes
                    if (esc === 'n') value += '\n';
                    else if (esc === 'r') value += '\r';
                    else if (esc === 't') value += '\t';
                    else if (esc === 'b') value += '\b';
                    else if (esc === 'f') value += '\f';
                    else if (esc === 'v') value += '\v';
                    else if (esc === '\\') value += '\\';
                    else if (esc === '"') value += '"';
                    else if (esc === "'") value += "'";
                    else value += esc; // unknown escape: keep char
                    i += 2;
                } else {
                    value += input[i++];
                }
            }
            if (input[i] !== quote) {
                throw new Error("Unterminated string literal at " + start);
            }
            i++; // skip closing quote
            tokens.push({ type: "string", value, pos: start });
            continue;
        }

        // Numbers (support integer, float, and BigInt suffix `n`)
        if (isDigit(c)) {
            let start = i;
            let value = "";
            let hasDot = false;
            while (i < input.length && (isDigit(input[i]) || input[i] === ".")) {
                if (input[i] === ".") {
                    if (hasDot) break;
                    hasDot = true;
                }
                value += input[i++];
            }
            // BigInt literal: integer followed by 'n' (no decimal point allowed)
            if (!hasDot && input[i] === "n") {
                i++; // consume 'n'
                tokens.push({ type: "bigint", value: value + "n", pos: start });
            } else {
                tokens.push({ type: "number", value, pos: start });
            }
            continue;
        }

        // Identifiers / keywords
        if (isAlpha(c)) {
            let start = i;
            let value = "";
            while (i < input.length && isAlphaNum(input[i])) {
                value += input[i++];
            }
            const keywords = new Set([
                "type",
                "function",
                "export",
                "let",
                "const",
                "return",
                "if",
                "else",
                "while",
                "for",
                "switch",
                "case",
                "default",
                "break",
                "try",
                "catch",
                "finally",
                "class",
                    "extends",
                "new",
                "async",
                "await"
            ]);
            // recognize 'static' as a keyword so class parsing detects it
            keywords.add("static");
            tokens.push({
                type: keywords.has(value) ? "keyword" : "identifier",
                value,
                pos: start
            });
            continue;
        }

        // Multi-char operators first (handle 3-char then 2-char operators)
        const threeChar = input.substr(i, 3);
        const threeOps = new Set(["===", "!=="]);
        if (threeOps.has(threeChar)) {
            tokens.push({ type: "punct", value: threeChar, pos: i });
            i += 3;
            continue;
        }

        const twoChar = input.substr(i, 2);
        const twoOps = new Set(["==", "!=", "<=", ">=", "&&", "||", "=>", "++", "--"]);
        if (twoOps.has(twoChar)) {
            tokens.push({ type: "punct", value: twoChar, pos: i });
            i += 2;
            continue;
        }

        // Single-char tokens (includes '.')
        const singleChars = [
            "{", "}", "(", ")", ";",
            "+", "-", "*", "/", "=",
            ",", ":", ".", "<", ">",
            "[", "]",
            "?"
        ];
        if (singleChars.includes(c)) {
            tokens.push({ type: "punct", value: c, pos: i });
            i++;
            continue;
        }

        throw new Error("Unexpected character: '" + c + "' at position " + i);
    }

    return tokens;
}

// Use this tokenizer as the TS lexer
const tokenizeTS = tokenize;

/* =============== AST HELPERS (with positions) =============== */

// AST node constructors
function Program(body) { return { kind: "Program", body }; }
function TypeDecl(name, type, pos) { return { kind: "TypeDecl", name, type, pos }; }
function VarDecl(name, type, init, pos) { return { kind: "VarDecl", name, type, init, pos }; }
function ConstDecl(name, type, init, pos) { return { kind: "ConstDecl", name, type, init, pos }; }
// NEW/UPDATED: Function decl / expr with isAsync flag
function FuncDecl(name, typeParams, params, returnType, body, pos, isAsync = false) {
    return { kind: "FuncDecl", name, typeParams, params, returnType, body, pos, isAsync };
}
function Param(name, type, pos) { return { kind: "Param", name, type, pos }; }
function Block(statements, pos) { return { kind: "Block", statements, pos }; }
function ReturnStmt(expr, pos) { return { kind: "ReturnStmt", expr, pos }; }
function ExprStmt(expr, pos) { return { kind: "ExprStmt", expr, pos }; }
function ForStmt(init, test, update, body, pos) {
    return { kind: "ForStmt", init, test, update, body, pos };
}
function ForOfStmt(left, right, body, pos) {
    return { kind: "ForOfStmt", left, right, body, pos };
}
function ForInStmt(left, right, body, pos) {
    return { kind: "ForInStmt", left, right, body, pos };
}
function SwitchStmt(discriminant, cases, pos) { return { kind: "SwitchStmt", discriminant, cases, pos }; }
function CaseClause(test, consequent, pos) { return { kind: "CaseClause", test, consequent, pos }; }
function BreakStmt(pos) { return { kind: "BreakStmt", pos }; }

// Type AST
function TypeRef(name, pos) { return { kind: "TypeRef", name, pos }; }
function ObjectType(properties, pos) { return { kind: "ObjectType", properties, pos }; }
function TypeProperty(name, type, pos) { return { kind: "TypeProperty", name, type, pos }; }
function ArrayType(element, pos) { return { kind: "ArrayType", element, pos }; }

// Export wrapper AST node: export <decl>
function ExportDecl(decl, pos) { return { kind: "ExportDecl", decl, pos }; }

// Expr AST
function Identifier(name, pos) { return { kind: "Identifier", name, pos }; }
function NumberLiteral(value, pos) { return { kind: "NumberLiteral", value, pos }; }
function BigIntLiteral(value, pos) { return { kind: "BigIntLiteral", value, pos }; }
function StringLiteral(value, pos) { return { kind: "StringLiteral", value, pos }; }
function BoolLiteral(value, pos) { return { kind: "BoolLiteral", value, pos }; }
function BinaryExpr(op, left, right, pos) { return { kind: "BinaryExpr", op, left, right, pos }; }
function ConditionalExpr(cond, thenExpr, elseExpr, pos) { return { kind: "ConditionalExpr", cond, thenExpr, elseExpr, pos }; }
function CallExpr(callee, args, pos) { return { kind: "CallExpr", callee, args, pos }; }
function MemberExpr(object, property, pos) { return { kind: "MemberExpr", object, property, pos }; }
function ArrayLiteral(elements, pos) { return { kind: "ArrayLiteral", elements, pos }; }
function IndexExpr(object, index, pos) { return { kind: "IndexExpr", object, index, pos }; }
// UPDATED: add isAsync flag
function ArrowFunction(params, body, pos, isAsync = false) {
    return { kind: "ArrowFunction", params, body, pos, isAsync };
}
// NEW: Await expression
function AwaitExpr(argument, pos) {
    return { kind: "AwaitExpr", argument, pos };
}
function UpdateExpr(argument, operator, prefix, pos) {
    return { kind: "UpdateExpr", argument, operator, prefix, pos };
}
// NEW: Object literal AST nodes
function ObjectLiteral(properties, pos) {
    return { kind: "ObjectLiteral", properties, pos };
}
function ObjectLiteralProperty(name, value, pos) {
    return { kind: "ObjectLiteralProperty", name, value, pos };
}
// Function expression (anonymous or named): function [name] (params) [: Type]? { body }
function FunctionExpr(name, typeParams, params, returnType, body, pos, isAsync = false) {
    return { kind: "FunctionExpr", name, typeParams, params, returnType, body, pos, isAsync };
}
// ClassDecl now holds both methods and fields (public instance fields)
function FieldDecl(name, type, init, pos, isStatic = false) { return { kind: "FieldDecl", name, type, init, pos, isStatic }; }
// ClassDecl now optionally holds a `superClass` name (string) when the class extends another
function ClassDecl(name, methods, fields, pos, superClass = null) { return { kind: "ClassDecl", name, methods, fields, pos, superClass }; }
function MethodDecl(name, params, body, pos, isConstructor, returnType = null, isStatic = false) { return { kind: "MethodDecl", name, params, body, pos, isConstructor, returnType, isStatic }; }
function NewExpr(callee, args, pos) { return { kind: "NewExpr", callee, args, pos }; }

/* =============== PARSER =============== */

function parse(tokens) {
    let i = 0;

    function peek(offset = 0) {
        return tokens[i + offset] || { type: "eof", value: "", pos: tokens.length };
    }

    function consume(type, value) {
        const tok = peek();
        if (type && tok.type !== type) {
            throw new Error(`Expected token type ${type}, got ${tok.type} at ${tok.pos}`);
        }
        if (value && tok.value !== value) {
            throw new Error(`Expected '${value}', got '${tok.value}' at ${tok.pos}`);
        }
        i++;
        return tok;
    }

    function match(type, value) {
        const tok = peek();
        if (tok.type === type && (!value || tok.value === value)) {
            i++;
            return true;
        }
        return false;
    }

    // helper: consume an identifier or a keyword (useful for property names like `.catch`)
    function consumeIdentAllowKeyword() {
        const t = peek();
        if (t.type === "identifier" || t.type === "keyword") {
            i++;
            return t;
        }
        throw new Error(`Expected token type identifier, got ${t.type} at ${t.pos}`);
    }

    // ---- Type parsing ----
    function parseTypeExpr() {
        const tok = peek();
        if (tok.type === "punct" && tok.value === "{") {
            return parseObjectType();
        }
        return parseSimpleTypeRef();
    }

    function parseSimpleTypeRef() {
        const tok = peek();
        if (tok.type === "identifier") {
            consume("identifier");
            let base = TypeRef(tok.value, tok.pos);
            // support array type syntax: T[]
            while (peek().type === "punct" && peek().value === "[") {
                const bracketTok = consume("punct", "[");
                consume("punct", "]");
                base = ArrayType(base, bracketTok.pos);
            }
            return base;
        }
        throw new Error("Expected type name at " + tok.pos);
    }

    function parseObjectType() {
        const braceTok = consume("punct", "{");
        const properties = [];
        while (!(peek().type === "punct" && peek().value === "}")) {
            const nameTok = consume("identifier");
            consume("punct", ":");
            const propType = parseTypeExpr();
            properties.push(TypeProperty(nameTok.value, propType, nameTok.pos));
            // allow either comma or semicolon as property separators in type/object
            const sep = peek();
            if (sep.type === "punct" && (sep.value === "," || sep.value === ";")) {
                // consume whichever separator is present
                consume("punct", sep.value);
                // continue parsing next property
            } else {
                break;
            }
        }
        consume("punct", "}");
        return ObjectType(properties, braceTok.pos);
    }

    // ---- Statements ----
    function parseStatement() {
        const tok = peek();

        // support: export <declaration>
        if (tok.type === "keyword" && tok.value === "export") {
            const expTok = consume("keyword", "export");
            // The next statement should be a declaration (const/let/function/type/class)
            const inner = parseStatement();
            return ExportDecl(inner, expTok.pos);
        }

        if (tok.type === "keyword" && tok.value === "type") {
            return parseTypeDecl();
        }

        if (tok.type === "keyword" && (
            tok.value === "function" ||
            (tok.value === "async" &&
                peek(1).type === "keyword" &&
                peek(1).value === "function")
        )) {
            return parseFuncDecl();
        }

        if (tok.type === "keyword" && tok.value === "let") {
            return parseVarDecl();
        }

        if (tok.type === "keyword" && tok.value === "const") {
            return parseConstDecl();
        }

        if (tok.type === "keyword" && tok.value === "if") {
            return parseIfStmt();
        }

        if (tok.type === "keyword" && tok.value === "while") {
            return parseWhileStmt();
        }

        if (tok.type === "keyword" && tok.value === "for") {
            return parseForStmt();
        }

        if (tok.type === "keyword" && tok.value === "class") {
            return parseClassDecl();
        }

        if (tok.type === "keyword" && tok.value === "try") {
            return parseTryStmt();
        }

        if (tok.type === "keyword" && tok.value === "switch") {
            return parseSwitchStmt();
        }

        if (tok.type === "keyword" && tok.value === "break") {
            const bk = consume("keyword", "break");
            const semi = consume("punct", ";");
            return BreakStmt(bk.pos);
        }

        if (tok.type === "keyword" && tok.value === "return") {
            return parseReturnStmt();
        }

        const expr = parseExpression();
        const semiTok = consume("punct", ";");
        return ExprStmt(expr, semiTok.pos);
    }

    function parseTypeDecl() {
        consume("keyword", "type");
        const nameTok = consume("identifier");
        consume("punct", "=");
        const typeExpr = parseTypeExpr();
        consume("punct", ";");
        return TypeDecl(nameTok.value, typeExpr, nameTok.pos);
    }

    // Top-level var/const declarations support optional type annotations or omitted type (JS-style)
    function parseVarDecl() {
        consume("keyword", "let");
        const nameTok = consume("identifier");
        let type = null;
        // optional type annotation
        if (peek().type === "punct" && peek().value === ":") {
            consume("punct", ":");
            type = parseSimpleTypeRef();
        }
        // optional initializer
        let init = null;
        if (peek().type === "punct" && peek().value === "=") {
            consume("punct", "=");
            init = parseExpression();
        }
        consume("punct", ";");
        return VarDecl(nameTok.value, type, init, nameTok.pos);
    }

    function parseConstDecl() {
        consume("keyword", "const");
        const nameTok = consume("identifier");
        let type = null;
        if (peek().type === "punct" && peek().value === ":") {
            consume("punct", ":");
            type = parseSimpleTypeRef();
        }
        // const must have initializer
        if (peek().type === "punct" && peek().value === "=") {
            consume("punct", "=");
            const init = parseExpression();
            consume("punct", ";");
            return ConstDecl(nameTok.value, type, init, nameTok.pos);
        }
        throw new Error(`Const '${nameTok.value}' must have an initializer at ${nameTok.pos}`);
    }
    // parse optional generic type parameter list: <T, U>
    function parseTypeParams() {
        if (!(peek().type === "punct" && peek().value === "<")) return null;
        consume("punct", "<");
        const tparams = [];
        if (!(peek().type === "punct" && peek().value === ">")) {
            do {
                const id = consume("identifier");
                tparams.push(id.value);
            } while (match("punct", ","));
        }
        consume("punct", ">");
        return tparams;
    }

    function parseFuncDecl() {
        let isAsync = false;
        if (peek().type === "keyword" && peek().value === "async") {
            consume("keyword", "async");
            isAsync = true;
        }

        consume("keyword", "function");
        const nameTok = consume("identifier");

        const typeParams = parseTypeParams();

        consume("punct", "(");
        const params = [];

        if (!match("punct", ")")) {
            do {
                const pName = consume("identifier");

                // optional : Type
                let pType = null;
                if (peek().type === "punct" && peek().value === ":") {
                    consume("punct", ":");
                    pType = parseSimpleTypeRef();
                }

                params.push(Param(pName.value, pType, pName.pos));
            } while (match("punct", ","));
            consume("punct", ")");
        }

        // optional return type: function f(): number {}
        // store a *type AST node* here; if omitted, the checker will treat it as `any`
        let retType = null;
        if (peek().type === "punct" && peek().value === ":") {
            consume("punct", ":");
            retType = parseSimpleTypeRef();
        }

        const body = parseBlock();
        return FuncDecl(nameTok.value, typeParams, params, retType, body, nameTok.pos, isAsync);
    }


    function parseBlock() {
        const braceTok = consume("punct", "{");
        const statements = [];
        while (!(peek().type === "punct" && peek().value === "}")) {
            statements.push(parseStatement());
        }
        consume("punct", "}");
        return Block(statements, braceTok.pos);
    }

    function parseIfStmt() {
        const ifTok = consume("keyword", "if");
        consume("punct", "(");
        const cond = parseExpression();
        consume("punct", ")");
        const thenBlock = parseBlock();
        let elseBlock = null;
        if (peek().type === "keyword" && peek().value === "else") {
            consume("keyword", "else");
            elseBlock = parseBlock();
        }
        return { kind: "IfStmt", cond, thenBlock, elseBlock, pos: ifTok.pos };
    }

    function parseWhileStmt() {
        const whileTok = consume("keyword", "while");
        consume("punct", "(");
        const cond = parseExpression();
        consume("punct", ")");
        const body = parseBlock();
        return { kind: "WhileStmt", cond, body, pos: whileTok.pos };
    }

    function parseSwitchStmt() {
        const swTok = consume("keyword", "switch");
        consume("punct", "(");
        const disc = parseExpression();
        consume("punct", ")");
        const cases = [];
        const open = consume("punct", "{");
        while (!(peek().type === "punct" && peek().value === "}")) {
            if (peek().type === "keyword" && peek().value === "case") {
                const caseTok = consume("keyword", "case");
                const testExpr = parseExpression();
                consume("punct", ":");
                const consequents = [];
                while (!(peek().type === "keyword" && (peek().value === "case" || peek().value === "default")) && !(peek().type === "punct" && peek().value === "}")) {
                    consequents.push(parseStatement());
                }
                cases.push(CaseClause(testExpr, consequents, caseTok.pos));
                continue;
            }
            if (peek().type === "keyword" && peek().value === "default") {
                const defTok = consume("keyword", "default");
                consume("punct", ":");
                const consequents = [];
                while (!(peek().type === "keyword" && (peek().value === "case" || peek().value === "default")) && !(peek().type === "punct" && peek().value === "}")) {
                    consequents.push(parseStatement());
                }
                cases.push(CaseClause(null, consequents, defTok.pos));
                continue;
            }
            throw new Error("Unexpected token in switch body: " + JSON.stringify(peek()));
        }
        consume("punct", "}");
        return SwitchStmt(disc, cases, swTok.pos);
    }

    // Full JS-style for loops: classic, for-of, for-in
    function parseForStmt() {
        const forTok = consume("keyword", "for");
        consume("punct", "(");

        // 1) Declaration-based head (let / const)
        if (peek().type === "keyword" && (peek().value === "let" || peek().value === "const")) {
            const declKind = peek().value; // 'let' or 'const'
            consume("keyword");
            const idTok = consume("identifier");

            // Optional type annotation: let x: T ...
            let typeAnn = null;
            if (peek().type === "punct" && peek().value === ":") {
                consume("punct", ":");
                typeAnn = parseSimpleTypeRef();
            }

            // Check for for-of / for-in
            if (
                peek().type === "identifier" &&
                (peek().value === "of" || peek().value === "in")
            ) {
                const modeTok = peek();
                const mode = modeTok.value; // 'of' or 'in'
                consume("identifier");
                const right = parseExpression();
                consume("punct", ")");
                const body = parseBlock();
                const left = {
                    declKind,
                    name: idTok.value,
                    typeAnn,
                    pos: idTok.pos
                };
                if (mode === "of") {
                    return ForOfStmt(left, right, body, forTok.pos);
                } else {
                    return ForInStmt(left, right, body, forTok.pos);
                }
            }

            // Otherwise: classic for-loop with declaration init
            let initExpr = null;
            if (peek().type === "punct" && peek().value === "=") {
                consume("punct", "=");
                initExpr = parseExpression();
            }
            const initDecl = declKind === "let"
                ? VarDecl(idTok.value, typeAnn, initExpr, idTok.pos)
                : ConstDecl(idTok.value, typeAnn, initExpr, idTok.pos);
            consume("punct", ";");

            const test = (peek().type === "punct" && peek().value === ";")
                ? null
                : parseExpression();
            consume("punct", ";");

            const update = (peek().type === "punct" && peek().value === ")")
                ? null
                : parseExpression();
            consume("punct", ")");

            const body = parseBlock();
            return ForStmt(initDecl, test, update, body, forTok.pos);
        }

        // 2) Non-declaration init: for (expr; cond; update)
        //    or missing init: for (; cond; update)
        let init = null;
        if (!(peek().type === "punct" && peek().value === ";")) {
            const initExpr = parseExpression();
            init = ExprStmt(initExpr, initExpr.pos);
        }
        consume("punct", ";");

        const test = (peek().type === "punct" && peek().value === ";")
            ? null
            : parseExpression();
        consume("punct", ";");

        const update = (peek().type === "punct" && peek().value === ")")
            ? null
            : parseExpression();
        consume("punct", ")");

        const body = parseBlock();
        return ForStmt(init, test, update, body, forTok.pos);
    }

    function parseReturnStmt() {
        const retTok = consume("keyword", "return");
        if (peek().type === "punct" && peek().value === ";") {
            consume("punct", ";");
            return ReturnStmt(null, retTok.pos);
        }
        const expr = parseExpression();
        consume("punct", ";");
        return ReturnStmt(expr, retTok.pos);
    }

    function parseClassDecl() {
        const classTok = consume("keyword", "class");
        const nameTok = consume("identifier");
        const methods = [];
        const fields = [];
        // optional `extends` clause
        let superName = null;
        if (peek().type === "keyword" && peek().value === "extends") {
            consume("keyword", "extends");
            const sup = consume("identifier");
            superName = sup.value;
        }
        consume("punct", "{");
        while (!(peek().type === "punct" && peek().value === "}")) {
            // optional 'static' modifier for class methods
            let isStatic = false;
            if (peek().type === "keyword" && peek().value === "static") {
                consume("keyword", "static");
                isStatic = true;
            }
            // member name (identifier). could be a method or a public field
            const memberNameTok = consume("identifier");
            // method if followed by '('
            if (peek().type === "punct" && peek().value === "(") {
                const mNameTok = memberNameTok;
                const isCtor = mNameTok.value === "constructor";
                consume("punct", "(");
                const params = [];
                if (!(peek().type === "punct" && peek().value === ")")) {
                    do {
                        const pName = consume("identifier");
                        let pType = null;
                        if (peek().type === "punct" && peek().value === ":") {
                            consume("punct", ":");
                            pType = parseSimpleTypeRef();
                        }
                        params.push(Param(pName.value, pType, pName.pos));
                    } while (match("punct", ",") && true);
                }
                consume("punct", ")");
                // optional return type annotation on method: foo(...): T { ... }
                let mReturnType = null;
                if (peek().type === "punct" && peek().value === ":") {
                    consume("punct", ":");
                    mReturnType = parseSimpleTypeRef();
                }
                const body = parseBlock();
                methods.push(MethodDecl(mNameTok.value, params, body, mNameTok.pos, isCtor, mReturnType, isStatic));
            } else {
                // treat as a public instance field (optional type, optional initializer)
                const fNameTok = memberNameTok;
                let fType = null;
                if (peek().type === "punct" && peek().value === ":") {
                    consume("punct", ":");
                    fType = parseSimpleTypeRef();
                }
                let fInit = null;
                if (peek().type === "punct" && peek().value === "=") {
                    consume("punct", "=");
                    fInit = parseExpression();
                }
                // optional semicolon after field
                if (peek().type === "punct" && peek().value === ";") {
                    consume("punct", ";");
                }
                fields.push(FieldDecl(fNameTok.value, fType, fInit, fNameTok.pos, isStatic));
            }
        }
        consume("punct", "}");
        return ClassDecl(nameTok.value, methods, fields, classTok.pos, superName);
    }

    function parseTryStmt() {
        const tryTok = consume("keyword", "try");
        const tryBlock = parseBlock();
        let catchClause = null;
        let finallyBlock = null;
        if (peek().type === "keyword" && peek().value === "catch") {
            consume("keyword", "catch");
            consume("punct", "(");
            const errName = consume("identifier");
            consume("punct", ")");
            const catchBlock = parseBlock();
            catchClause = { name: errName.value, block: catchBlock, pos: errName.pos };
        }
        if (peek().type === "keyword" && peek().value === "finally") {
            consume("keyword", "finally");
            finallyBlock = parseBlock();
        }
        return { kind: "TryStmt", tryBlock, catchClause, finallyBlock, pos: tryTok.pos };
    }

    // ---- Expressions ----
    function parseExpression() {
        return parseAssignment();
    }

    // assignment -> logicalOr ( '=' assignment )?
    // conditional -> logicalOr ( '?' assignment ':' assignment )?
    function parseConditional() {
        let expr = parseLogicalOr();
        const tok = peek();
        if (tok.type === "punct" && tok.value === "?") {
            // ternary conditional
            i++; // consume '?'
            const thenExpr = parseAssignment();
            consume("punct", ":");
            const elseExpr = parseAssignment();
            return ConditionalExpr(expr, thenExpr, elseExpr, tok.pos);
        }
        return expr;
    }

    function parseAssignment() {
        let expr = parseConditional();
        const tok = peek();
        if (tok.type === "punct" && tok.value === "=") {
            // assignment is right-associative
            i++;
            const right = parseAssignment();
            expr = BinaryExpr("=", expr, right, tok.pos);
        }
        return expr;
    }

    // logicalOr -> logicalAnd ( '||' logicalAnd )*
    function parseLogicalOr() {
        let expr = parseLogicalAnd();
        while (true) {
            const tok = peek();
            if (tok.type === "punct" && tok.value === "||") {
                i++;
                const right = parseLogicalAnd();
                expr = BinaryExpr(tok.value, expr, right, tok.pos);
            } else break;
        }
        return expr;
    }

    // logicalAnd -> equality ( '&&' equality )*
    function parseLogicalAnd() {
        let expr = parseEquality();
        while (true) {
            const tok = peek();
            if (tok.type === "punct" && tok.value === "&&") {
                i++;
                const right = parseEquality();
                expr = BinaryExpr(tok.value, expr, right, tok.pos);
            } else break;
        }
        return expr;
    }

    // equality -> relational ( (== | != | === | !==) relational )*
    function parseEquality() {
        let expr = parseRelational();
        while (true) {
            const tok = peek();
            if (tok.type === "punct" && (["==", "!=", "===", "!=="].includes(tok.value))) {
                i++;
                const right = parseRelational();
                expr = BinaryExpr(tok.value, expr, right, tok.pos);
            } else {
                break;
            }
        }
        return expr;
    }

    // relational -> additive ( (< | > | <= | >=) additive )*
    function parseRelational() {
        let expr = parseAdditive();
        while (true) {
            const tok = peek();
            if (tok.type === "punct" && (tok.value === "<" || tok.value === ">" || tok.value === "<=" || tok.value === ">=")) {
                i++;
                const right = parseAdditive();
                expr = BinaryExpr(tok.value, expr, right, tok.pos);
            } else {
                break;
            }
        }
        return expr;
    }

    // additive -> multiplicative ( (+ | -) multiplicative )*
    function parseAdditive() {
        let expr = parseMultiplicative();
        while (true) {
            const tok = peek();
            if (tok.type === "punct" && (tok.value === "+" || tok.value === "-")) {
                i++;
                const right = parseMultiplicative();
                expr = BinaryExpr(tok.value, expr, right, tok.pos);
            } else {
                break;
            }
        }
        return expr;
    }

    // multiplicative -> postfix ( (* | /) postfix )*
    function parseMultiplicative() {
        let expr = parsePostfix();
        while (true) {
            const tok = peek();
            if (tok.type === "punct" && (tok.value === "*" || tok.value === "/")) {
                i++;
                const right = parsePostfix();
                expr = BinaryExpr(tok.value, expr, right, tok.pos);
            } else {
                break;
            }
        }
        return expr;
    }

    // Postfix: property access + call + index + postfix ++/--
    function parsePostfix() {

        // support `new` expressions as a prefix that then receive argument list
        if (peek().type === "keyword" && peek().value === "new") {
            const newTok = consume("keyword", "new");
            // callee: accept identifier or a member expression starting with identifier
            let callee = null;
            if (peek().type === "identifier") {
                const id = consume("identifier");
                callee = Identifier(id.value, id.pos);
            } else {
                throw new Error("Expected identifier after 'new' at " + newTok.pos);
            }
            // argument list
            consume("punct", "(");
            const args = [];
            if (!(peek().type === "punct" && peek().value === ")")) {
                do {
                    args.push(parseExpression());
                } while (match("punct", ",") && true);
            }
            consume("punct", ")");
            let expr = NewExpr(callee, args, newTok.pos);
            // allow chaining postfixes on the result
            while (true) {
                const tok = peek();
                if (tok.type === "punct" && tok.value === ".") {
                    consume("punct", ".");
                    // property name after '.' may be an identifier or (JS) keyword like `catch`
                    const nameTok = consumeIdentAllowKeyword();
                    expr = MemberExpr(expr, nameTok.value, nameTok.pos);
                } else if (tok.type === "punct" && tok.value === "[") {
                    const bracketTok = consume("punct", "[");
                    const idx = parseExpression();
                    consume("punct", "]");
                    expr = IndexExpr(expr, idx, bracketTok.pos);
                } else if (tok.type === "punct" && tok.value === "(") {
                    const parenTok = consume("punct", "(");
                    const args2 = [];
                    if (!(peek().type === "punct" && peek().value === ")")) {
                        do {
                            args2.push(parseExpression());
                        } while (match("punct", ",") && true);
                    }
                    consume("punct", ")");
                    expr = CallExpr(expr, args2, parenTok.pos);
                } else if (tok.type === "punct" && (tok.value === "++" || tok.value === "--")) {
                    consume("punct", tok.value);
                    expr = UpdateExpr(expr, tok.value, false, tok.pos);
                } else {
                    break;
                }
            }
            return expr;
        }

        let expr = parsePrimary();
        while (true) {
            const tok = peek();
            if (tok.type === "punct" && tok.value === ".") {
                consume("punct", ".");
                // allow keyword property names (e.g. `.catch` or `.finally`) after '.'
                const nameTok = (peek().type === "identifier" || peek().type === "keyword") ? consumeIdentAllowKeyword() : consume("identifier");
                expr = MemberExpr(expr, nameTok.value, nameTok.pos);
            } else if (tok.type === "punct" && tok.value === "[") {
                const bracketTok = consume("punct", "[");
                const idx = parseExpression();
                consume("punct", "]");
                expr = IndexExpr(expr, idx, bracketTok.pos);
            } else if (tok.type === "punct" && tok.value === "(") {
                const parenTok = consume("punct", "(");
                const args = [];
                if (!(peek().type === "punct" && peek().value === ")")) {
                    do {
                        args.push(parseExpression());
                    } while (match("punct", ",") && true);
                }
                consume("punct", ")");
                expr = CallExpr(expr, args, parenTok.pos);
            } else if (tok.type === "punct" && (tok.value === "++" || tok.value === "--")) {
                consume("punct", tok.value);
                expr = UpdateExpr(expr, tok.value, false, tok.pos);
            } else {
                break;
            }
        }
        return expr;
    }

    function parsePrimary() {
        const tok = peek();
        // NEW: await expression
        if (tok.type === "keyword" && tok.value === "await") {
            const awaitTok = consume("keyword", "await");
            const arg = parseExpression();
            return AwaitExpr(arg, awaitTok.pos);
        }

        // NEW: async arrow functions and async function expressions
        if (tok.type === "keyword" && tok.value === "async") {
            // async x => ...
            if (peek(1).type === "identifier" &&
                peek(2).type === "punct" && peek(2).value === "=>") {
                const asyncTok = consume("keyword", "async");
                const nameTok = consume("identifier");
                consume("punct", "=>");
                let body;
                if (peek().type === "punct" && peek().value === "{") {
                    body = parseBlock();
                } else {
                    body = parseExpression();
                }
                return ArrowFunction([nameTok.value], body, asyncTok.pos, true);
            }

            // async (x, y) => ...
            if (peek(1).type === "punct" && peek(1).value === "(") {
                function isArrowParamListFrom(offset) {
                    let j = offset + 1; // token after '('
                    const tOpen = tokens[offset + 1];
                    if (!tOpen || tOpen.type !== "punct" || tOpen.value !== "(") return false;

                    if (tokens[j] && tokens[j].type === "punct" && tokens[j].value === ")") {
                        return tokens[j + 1] &&
                            tokens[j + 1].type === "punct" &&
                            tokens[j + 1].value === "=>";
                    }
                    while (true) {
                        const t = tokens[j];
                        if (!t) return false;
                        if (t.type === "identifier") {
                            j++;
                            if (tokens[j] && tokens[j].type === "punct" && tokens[j].value === ",") {
                                j++;
                                continue;
                            }
                            if (tokens[j] && tokens[j].type === "punct" && tokens[j].value === ")") {
                                return tokens[j + 1] &&
                                    tokens[j + 1].type === "punct" &&
                                    tokens[j + 1].value === "=>";
                            }
                            return false;
                        }
                        return false;
                    }
                }

                if (isArrowParamListFrom(i)) {
                    const asyncTok = consume("keyword", "async");
                    const openTok = consume("punct", "(");
                    const params = [];
                    if (!(peek().type === "punct" && peek().value === ")")) {
                        do {
                            const p = consume("identifier");
                            params.push(p.value);
                        } while (match("punct", ",") && true);
                    }
                    consume("punct", ")");
                    consume("punct", "=>");
                    let body;
                    if (peek().type === "punct" && peek().value === "{") {
                        body = parseBlock();
                    } else {
                        body = parseExpression();
                    }
                    return ArrowFunction(params, body, asyncTok.pos, true);
                }
            }

            // async function expression: async function [name]? (params) [: Type]? { body }
            if (tok.type === "keyword" && tok.value === "async" &&
                peek(1).type === "keyword" && peek(1).value === "function") {

                const asyncTok = consume("keyword", "async");
                consume("keyword", "function");

                // optional name
                let name = null;
                if (peek().type === "identifier") {
                    const n = consume("identifier");
                    name = n.value;
                }

                // optional generic params on function expression
                const typeParams = parseTypeParams();

                consume("punct", "(");
                const params = [];
                if (!(peek().type === "punct" && peek().value === ")")) {
                    do {
                        const pName = consume("identifier");
                        let pType = null;

                        // OPTIONAL : Type
                        if (peek().type === "punct" && peek().value === ":") {
                            consume("punct", ":");
                            pType = parseSimpleTypeRef();
                        }

                        params.push(Param(pName.value, pType, pName.pos));
                    } while (match("punct", ","));
                }
                consume("punct", ")");

                // OPTIONAL return type
                // OPTIONAL return type (null means "any")
                let returnType = null;
                if (peek().type === "punct" && peek().value === ":") {
                    consume("punct", ":");
                    returnType = parseSimpleTypeRef();
                }

                const body = parseBlock();
                return FunctionExpr(name, typeParams, params, returnType, body, asyncTok.pos, true);
            }

            // async function () { ... } expression
            if (peek(1).type === "keyword" && peek(1).value === "function") {
                const asyncTok = consume("keyword", "async");
                const funcTok = consume("keyword", "function");
                let name = null;
                if (peek().type === "identifier") {
                    const n = consume("identifier");
                    name = n.value;
                }
                const typeParams = parseTypeParams();
                consume("punct", "(");
                const params = [];
                if (!(peek().type === "punct" && peek().value === ")")) {
                    do {
                        const pName = consume("identifier");
                        let pType = null;
                        if (peek().type === "punct" && peek().value === ":") {
                            consume("punct", ":");
                            pType = parseSimpleTypeRef();
                        }
                        params.push(Param(pName.value, pType, pName.pos));
                    } while (match("punct", ",") && true);
                }
                consume("punct", ")");
                let returnType = null;
                if (peek().type === "punct" && peek().value === ":") {
                    consume("punct", ":");
                    returnType = parseSimpleTypeRef();
                }
                const body = parseBlock();
                return FunctionExpr(name, typeParams, params, returnType, body, funcTok.pos, true);
            }
        }

        // existing function expression handling stays, just uses default isAsync=false
        if (tok.type === "keyword" && tok.value === "function") {
            const funcTok = consume("keyword", "function");

            // optional name
            let name = null;
            if (peek().type === "identifier") {
                const n = consume("identifier");
                name = n.value;
            }

            const typeParams = parseTypeParams();

            consume("punct", "(");
            const params = [];
            if (!match("punct", ")")) {
                do {
                    const pName = consume("identifier");
                    let pType = null;

                    // optional : Type
                    if (peek().type === "punct" && peek().value === ":") {
                        consume("punct", ":");
                        pType = parseSimpleTypeRef();
                    }

                    params.push(Param(pName.value, pType, pName.pos));
                } while (match("punct", ","));
                consume("punct", ")");
            }

            // optional return type
            // optional return type (null means "any")
            let returnType = null;
            if (peek().type === "punct" && peek().value === ":") {
                consume("punct", ":");
                returnType = parseSimpleTypeRef();
            }

            const body = parseBlock();
            return FunctionExpr(name, typeParams, params, returnType, body, funcTok.pos, false);
        }
        if (tok.type === "bigint") {
            consume("bigint");
            // keep the literal text (including trailing 'n') so emitter can reproduce it
            return BigIntLiteral(tok.value, tok.pos);
        }
        if (tok.type === "number") {
            consume("number");
            return NumberLiteral(Number(tok.value), tok.pos);
        }
        if (tok.type === "string") {
            consume("string");
            return StringLiteral(tok.value, tok.pos);
        }
        if (tok.type === "punct" && tok.value === "[") {
            const bracketTok = consume("punct", "[");
            const elems = [];
            if (!(peek().type === "punct" && peek().value === "]")) {
                do {
                    elems.push(parseExpression());
                } while (match("punct", ",") && true);
            }
            consume("punct", "]");
            return ArrayLiteral(elems, bracketTok.pos);
        }
        // NEW: object literal expression
        if (tok.type === "punct" && tok.value === "{") {
            const braceTok = consume("punct", "{");
            const props = [];
            if (!(peek().type === "punct" && peek().value === "}")) {
                while (true) {
                    // object literal property names can be keywords in JS, allow that here
                    const nameTok = (peek().type === "identifier" || peek().type === "keyword") ? consumeIdentAllowKeyword() : consume("identifier");
                    consume("punct", ":");
                    const valueExpr = parseExpression();
                    props.push(ObjectLiteralProperty(nameTok.value, valueExpr, nameTok.pos));
                    // accept both comma and semicolon as separators in object literals
                    const sep2 = peek();
                    if (sep2.type === "punct" && (sep2.value === "," || sep2.value === ";")) {
                        consume("punct", sep2.value);
                        continue;
                    }
                    break;
                }
            }
            consume("punct", "}");
            return ObjectLiteral(props, braceTok.pos);
        }
        // Arrow function: identifier => expr
        if (tok.type === "identifier" && peek(1).type === "punct" && peek(1).value === "=>") {
            const nameTok = consume("identifier");
            consume("punct", "=>");
            let body;
            if (peek().type === "punct" && peek().value === "{") {
                body = parseBlock();
            } else {
                body = parseExpression();
            }
            return ArrowFunction([nameTok.value], body, nameTok.pos);
        }
        if (tok.type === "identifier") {
            if (tok.value === "true" || tok.value === "false") {
                consume("identifier");
                return BoolLiteral(tok.value === "true", tok.pos);
            }
            consume("identifier");
            return Identifier(tok.value, tok.pos);
        }
        if (tok.type === "punct" && tok.value === "(") {
            // could be grouping or arrow-params: lookahead to see if this is a parameter list for an arrow
            function isArrowParamList() {
                let j = i + 1; // token after '('
                // allow empty param list
                if (peek(1).type === "punct" && peek(1).value === ")") {
                    return peek(2).type === "punct" && peek(2).value === "=>";
                }
                // expect identifier (, identifier)* ) =>
                while (true) {
                    const t = tokens[j];
                    if (!t) return false;
                    if (t.type === "identifier") {
                        j++;
                        if (tokens[j] && tokens[j].type === "punct" && tokens[j].value === ",") {
                            j++;
                            continue;
                        }
                        if (tokens[j] && tokens[j].type === "punct" && tokens[j].value === ")") {
                            return tokens[j + 1] && tokens[j + 1].type === "punct" && tokens[j + 1].value === "=>";
                        }
                        return false;
                    }
                    return false;
                }
            }

            if (isArrowParamList()) {
                const openTok = consume("punct", "(");
                const params = [];
                if (!(peek().type === "punct" && peek().value === ")")) {
                    do {
                        const p = consume("identifier");
                        params.push(p.value);
                    } while (match("punct", ",") && true);
                }
                consume("punct", ")");
                consume("punct", "=>");
                let body;
                if (peek().type === "punct" && peek().value === "{") {
                    body = parseBlock();
                } else {
                    body = parseExpression();
                }
                return ArrowFunction(params, body, openTok.pos);
            }

            // grouping
            consume("punct", "(");
            const expr = parseExpression();
            consume("punct", ")");
            return expr;
        }

        throw new Error("Unexpected token in expression: " + JSON.stringify(tok));
    }

    const body = [];
    while (peek().type !== "eof") {
        body.push(parseStatement());
    }
    return Program(body);
}

/* =============== TYPE SYSTEM =============== */

function anyType() {
    return { kind: "any" };
}

function primitiveType(name) {
    if (name === "any") return anyType();
    // include additional JS/TS primitive kinds: null, undefined, symbol
    if (!["number", "string", "boolean", "void", "bigint", "null", "undefined", "symbol"].includes(name)) return null;
    return { kind: name };
}

function functionType(paramTypes, returnType, typeParams = null) {
    return { kind: "function", params: paramTypes, returnType, typeParams };
}

function objectTypeFromAst(node, env, resolveTypeExpr) {
    const props = new Map();
    for (const p of node.properties) {
        const t = resolveTypeExpr(p.type, env);
        props.set(p.name, t);
    }
    return { kind: "object", properties: props };
}

function typeToString(t) {
    // Guard against recursive types by tracking visited objects
    function _typeToString(t, visited) {
        if (!t) return "unknown";
        if (t.kind === "any") return "any";
        if (t.kind === "function") {
            if (visited.has(t)) return "(...) => ...";
            visited.add(t);
            const params = t.params.map(p => _typeToString(p, visited)).join(", ");
            const ret = _typeToString(t.returnType, visited);
            visited.delete(t);
            return `(${params}) => ${ret}`;
        }
        if (t.kind === "object") {
            if (visited.has(t)) return "{...}";
            visited.add(t);
            const parts = [];
            for (const [name, tp] of t.properties.entries()) {
                parts.push(`${name}: ${_typeToString(tp, visited)}`);
            }
            visited.delete(t);
            return `{ ${parts.join("; ")} }`;
        }
        if (t.kind === "array") {
            return `${_typeToString(t.element, visited)}[]`;
        }
        return t.kind;
    }
    return _typeToString(t, new WeakSet());
}

/* =============== SCOPE / ENVIRONMENT =============== */

class Env {
    constructor(parent = null) {
        this.parent = parent;
        this.types = new Map();   // name -> Type
        this.values = new Map();  // name -> Type
    }

    defineType(name, type) {
        this.types.set(name, type);
    }

    lookupType(name) {
        if (this.types.has(name)) return this.types.get(name);
        if (this.parent) return this.parent.lookupType(name);
        return null;
    }

    defineValue(name, type) {
        this.values.set(name, type);
    }

    lookupValue(name) {
        if (this.values.has(name)) return this.values.get(name);
        if (this.parent) return this.parent.lookupValue(name);
        return null;
    }
}

/* =============== TYPE CHECKER =============== */

function buildPosToLineMap(source) {
    const lineStarts = [0];
    for (let i = 0; i < source.length; i++) {
        if (source[i] === "\n") {
            lineStarts.push(i + 1);
        }
    }
    return function posToLine(pos) {
        if (typeof pos !== "number" || pos < 0) return null;
        let line = 1;
        for (let i = 0; i < lineStarts.length; i++) {
            if (lineStarts[i] <= pos) {
                line = i + 1;
            } else {
                break;
            }
        }
        return line;
    };
}

function typeCheck(ast, source) {
    const errors = [];
    const globalEnv = new Env();
    const posToLine = buildPosToLineMap(source);

    // register built-in primitive names (including the newly-supported ones)
    ["number", "string", "boolean", "void", "bigint", "null", "undefined", "symbol", "any"].forEach(p => {
        globalEnv.defineType(p, primitiveType(p));
    });

    function error(msg, node) {
        errors.push({
            msg: msg,
            pos: node && typeof node.pos === "number" ? node.pos : 0
        });
    }

    function resolveTypeExpr(node, env) {
        switch (node.kind) {
            case "TypeRef": {
                const prim = primitiveType(node.name);
                if (prim) return prim;
                const t = env.lookupType(node.name);
                if (!t) {
                    error(`Unknown type '${node.name}'`, node);
                    return primitiveType("void");
                }
                return t;
            }
            case "ArrayType": {
                const el = resolveTypeExpr(node.element, env);
                return { kind: "array", element: el };
            }
            case "ObjectType": {
                return objectTypeFromAst(node, env, resolveTypeExpr);
            }
            default:
                error("Unknown type expression kind: " + node.kind, node);
                return primitiveType("void");
        }
    }

    // Infer type parameter mappings by comparing a parameter type (which may
    // reference type parameters) against an argument type. `mapping` is a Map
    // from type parameter name -> concrete Type.
    function inferTypeParams(paramType, argType, mapping) {
        if (!paramType || !argType) return;
        if (paramType.kind === "typeParam") {
            // map type parameter to the argument type (first mapping wins)
            if (!mapping.has(paramType.name)) mapping.set(paramType.name, argType);
            return;
        }
        if (paramType.kind === "array" && argType.kind === "array") {
            inferTypeParams(paramType.element, argType.element, mapping);
            return;
        }
        if (paramType.kind === "object" && argType.kind === "object") {
            // try to infer from common properties (shallow)
            for (const [name, pType] of paramType.properties.entries()) {
                const aProp = argType.properties.get(name);
                if (aProp) inferTypeParams(pType, aProp, mapping);
            }
            return;
        }
        // other shapes: no inference
    }

    function substituteType(t, mapping) {
        if (!t) return t;
        if (t.kind === "typeParam") {
            return mapping.has(t.name) ? mapping.get(t.name) : anyType();
        }
        if (t.kind === "array") {
            return { kind: "array", element: substituteType(t.element, mapping) };
        }
        if (t.kind === "object") {
            const props = new Map();
            for (const [name, pt] of t.properties.entries()) {
                props.set(name, substituteType(pt, mapping));
            }
            return { kind: "object", properties: props };
        }
        if (t.kind === "function") {
            const params = t.params.map(p => substituteType(p, mapping));
            const ret = substituteType(t.returnType, mapping);
            return functionType(params, ret, null);
        }
        return t;
    }

    function checkProgram(node, env) {
        for (const stmt of node.body) {
            checkStatement(stmt, env, null);
        }
    }

    function checkStatement(node, env, currentFunctionType) {
        switch (node.kind) {
            case "ExportDecl": {
                // type-check inner declaration (export doesn't change checking semantics)
                checkStatement(node.decl, env, currentFunctionType);
                break;
            }
            case "TypeDecl": {
                const t = resolveTypeExpr(node.type, env);
                env.defineType(node.name, t);
                break;
            }
            case "VarDecl": {
                let declaredType = node.type ? resolveTypeExpr(node.type, env) : null;
                let initType = null;

                if (node.init) {
                    initType = checkExpr(node.init, env);
                }

                if (declaredType && initType && !isAssignable(initType, declaredType)) {
                    error(
                        `Cannot assign type '${typeToString(initType)}' to '${typeToString(declaredType)}' (variable '${node.name}')`,
                        node
                    );
                }

                if (!declaredType && initType) {
                    declaredType = initType;
                }

                if (!declaredType) {
                    declaredType = anyType();
                }

                env.defineValue(node.name, declaredType);
                break;
            }
            case "ConstDecl": {
                let declaredType = node.type ? resolveTypeExpr(node.type, env) : null;

                if (!node.init) {
                    error(`Const '${node.name}' must have an initializer`, node);
                    declaredType = declaredType || anyType();
                    env.defineValue(node.name, declaredType);
                    break;
                }

                const initType = checkExpr(node.init, env);

                if (declaredType && !isAssignable(initType, declaredType)) {
                    error(
                        `Cannot assign type '${typeToString(initType)}' to '${typeToString(declaredType)}' (const '${node.name}')`,
                        node
                    );
                }

                if (!declaredType) {
                    declaredType = initType || anyType();
                }

                env.defineValue(node.name, declaredType);
                break;
            }
            case "FuncDecl": {
                // If the function has generic type parameters, make them available
                // while resolving parameter and return type annotations so that
                // TypeRef nodes referencing those names resolve to the same
                // type parameter placeholders.
                const resolvedTypeEnv = new Env(env);
                if (node.typeParams) {
                    node.typeParams.forEach(tp => resolvedTypeEnv.defineType(tp, { kind: "typeParam", name: tp }));
                }
                const paramTypes = node.params.map(
                    p => (p.type ? resolveTypeExpr(p.type, resolvedTypeEnv) : anyType())
                );
                const returnType = node.returnType
                    ? resolveTypeExpr(node.returnType, resolvedTypeEnv)
                    : anyType();
                const fnType = functionType(paramTypes, returnType, node.typeParams || null);
                env.defineValue(node.name, fnType);

                const fnEnv = new Env(env);
                // make type parameters available inside the function body as types
                if (node.typeParams) {
                    node.typeParams.forEach(tp => fnEnv.defineType(tp, { kind: "typeParam", name: tp }));
                }
                node.params.forEach((p, idx) => {
                    fnEnv.defineValue(p.name, paramTypes[idx]);
                });

                // If the function is generic (has type parameters) we shouldn't
                // enforce the declared return type against type-parameter-based
                // return expressions at declaration time. Those return checks
                // are performed effectively at call sites when type parameters
                // are instantiated. To avoid spurious errors like returning a
                // `typeParam` where a concrete type was annotated, use a
                // relaxed function type (with `any` return) while checking the
                // body of a generic function.
                if (node.typeParams && node.typeParams.length > 0) {
                    const relaxed = functionType(paramTypes, anyType(), null);
                    checkBlock(node.body, fnEnv, relaxed);
                } else {
                    checkBlock(node.body, fnEnv, fnType);
                }
                break;
            }
            case "Block":
                checkBlock(node, env, currentFunctionType);
                break;
            case "ReturnStmt": {
                if (!currentFunctionType) {
                    error("Return statement not inside function", node);
                    break;
                }
                const expected = currentFunctionType.returnType;
                if (node.expr === null) {
                    if (expected.kind !== "void") {
                        error(
                            `Return type mismatch: expected '${typeToString(expected)}' but got 'void'`,
                            node
                        );
                    }
                } else {
                    const actual = checkExpr(node.expr, env);
                    if (!isAssignable(actual, expected)) {
                        error(
                            `Return type mismatch: expected '${typeToString(expected)}' but got '${typeToString(actual)}'`,
                            node
                        );
                    }
                }
                break;
            }
            case "ExprStmt":
                checkExpr(node.expr, env);
                break;
            case "IfStmt": {
                const condType = checkExpr(node.cond, env);
                if (condType.kind !== "boolean" && condType.kind !== "any") {
                    error(
                        `Condition in if must be boolean, got '${typeToString(condType)}'`,
                        node.cond
                    );
                }
                checkStatement(node.thenBlock, env, currentFunctionType);
                if (node.elseBlock) checkStatement(node.elseBlock, env, currentFunctionType);
                break;
            }
            case "WhileStmt": {
                const condType = checkExpr(node.cond, env);
                if (condType.kind !== "boolean" && condType.kind !== "any") {
                    error(
                        `Condition in while must be boolean, got '${typeToString(condType)}'`,
                        node.cond
                    );
                }
                checkStatement(node.body, env, currentFunctionType);
                break;
            }
            case "ForStmt": {
                const loopEnv = new Env(env);

                if (node.init) {
                    // init is either VarDecl/ConstDecl or ExprStmt
                    checkStatement(node.init, loopEnv, currentFunctionType);
                }

                if (node.test) {
                    const condType = checkExpr(node.test, loopEnv);
                    if (condType.kind !== "boolean" && condType.kind !== "any") {
                        error(
                            `Condition in for must be boolean, got '${typeToString(condType)}'`,
                            node.test
                        );
                    }
                }

                if (node.update) {
                    checkExpr(node.update, loopEnv);
                }

                checkStatement(node.body, loopEnv, currentFunctionType);
                break;
            }
            case "ForOfStmt": {
                const arrType = checkExpr(node.right, env);
                let elemType = anyType();
                if (!arrType || (arrType.kind !== "array" && arrType.kind !== "any")) {
                    error(
                        `Right-hand side of for-of must be an array, got '${typeToString(arrType)}'`,
                        node.right
                    );
                } else if (arrType.kind === "array") {
                    elemType = arrType.element;
                }

                const loopEnv = new Env(env);
                let varType = elemType;

                if (node.left.typeAnn) {
                    const declared = resolveTypeExpr(node.left.typeAnn, env);
                    varType = declared;
                    if (!isAssignable(elemType, declared)) {
                        error(
                            `for-of variable type '${typeToString(declared)}' is not assignable from element type '${typeToString(elemType)}'`,
                            node
                        );
                    }
                }

                loopEnv.defineValue(node.left.name, varType);
                checkStatement(node.body, loopEnv, currentFunctionType);
                break;
            }
            case "ForInStmt": {
                // Very loose: keys are treated as strings (or any)
                const objType = checkExpr(node.right, env);
                const keyType = primitiveType("string") || anyType();

                const loopEnv = new Env(env);
                let varType = keyType;

                if (node.left.typeAnn) {
                    const declared = resolveTypeExpr(node.left.typeAnn, env);
                    varType = declared;
                    if (!isAssignable(keyType, declared)) {
                        error(
                            `for-in variable type '${typeToString(declared)}' is not assignable from key type '${typeToString(keyType)}'`,
                            node
                        );
                    }
                }

                loopEnv.defineValue(node.left.name, varType);
                checkStatement(node.body, loopEnv, currentFunctionType);
                break;
            }
            case "ClassDecl": {
                // register class in current environment with a simple constructor/method map
                const staticMethodsMap = new Map();
                const instanceProps = new Map();

                // If this class extends another, try to inherit static and instance members
                if (node.superClass) {
                    const superType = env.lookupValue(node.superClass);
                    if (superType && superType.kind === "class") {
                        // copy static methods from super
                        for (const [k, v] of superType.methods.entries()) {
                            staticMethodsMap.set(k, v);
                        }
                        // copy instance properties/methods from super
                        if (superType.instance && superType.instance.properties) {
                            for (const [k, v] of superType.instance.properties.entries()) {
                                instanceProps.set(k, v);
                            }
                        }
                    }
                }

                // instance type for `new Person(...)`
                const instanceType = { kind: "object", properties: instanceProps };

                // default ctor: () => instance
                let ctorType = functionType([], instanceType);

                // First pass: collect parameter types and create placeholder method types
                // (use any for return types initially). We resolve declared return types
                // in a second pass after registering the class type so that methods can
                // refer to the class name in their return annotations.
                const methodParamMap = new Map();
                for (const m of node.methods) {
                    const paramTypes = m.params.map(
                        p => (p.type ? resolveTypeExpr(p.type, env) : anyType())
                    );
                    methodParamMap.set(m, paramTypes);

                    if (m.isConstructor) {
                        // constructor returns the instance type
                        ctorType = functionType(paramTypes, instanceType);
                    } else {
                        const mt = functionType(paramTypes, anyType());
                        if (m.isStatic) {
                            staticMethodsMap.set(m.name, mt);
                        } else {
                            // methods are available on the instance
                            instanceProps.set(m.name, mt);
                        }
                    }
                }

                // Also collect declared fields (typed or inferred from initializer)
                if (node.fields && node.fields.length > 0) {
                    for (const f of node.fields) {
                        // if field has an explicit type annotation, use it; otherwise try to infer from initializer
                        let fType = null;
                        if (f.type) {
                            fType = resolveTypeExpr(f.type, env);
                        } else if (f.init) {
                            fType = checkExpr(f.init, env);
                        } else {
                            fType = anyType();
                        }
                        if (f.isStatic) {
                            staticMethodsMap.set(f.name, fType);
                        } else {
                            instanceProps.set(f.name, fType);
                        }
                    }
                }

                const classType = {
                    kind: "class",
                    name: node.name,
                    ctor: ctorType,
                    methods: staticMethodsMap,
                    instance: instanceType,
                    super: node.superClass || null
                };

                // make the class available as a value and as a type (the type being
                // the instance shape) so that method return annotations can reference
                // the class name (e.g. `clone(): Vec3`).
                env.defineValue(node.name, classType);
                env.defineType(node.name, instanceType);

                // Second pass: resolve declared return types now that the class name
                // is registered as a type; update method function types accordingly.
                for (const m of node.methods) {
                    if (!m.isConstructor && m.returnType) {
                        const paramTypes = methodParamMap.get(m) || [];
                        const retT = resolveTypeExpr(m.returnType, env);
                        const mt = functionType(paramTypes, retT);
                        if (m.isStatic) {
                            staticMethodsMap.set(m.name, mt);
                        } else {
                            instanceProps.set(m.name, mt);
                        }
                    }
                }

                // type-check method bodies
                for (const m of node.methods) {
                    const fnType = m.isConstructor
                        ? ctorType
                        : instanceProps.get(m.name) || functionType([], anyType());
                    const methodEnv = new Env(env);
                    m.params.forEach((p, idx) =>
                        methodEnv.defineValue(p.name, fnType.params[idx] || anyType())
                    );
                    checkBlock(m.body, methodEnv, fnType);
                }
                break;
            }

            case "TryStmt": {
                // type-check try/catch/finally by checking contained blocks
                checkBlock(node.tryBlock, env, currentFunctionType);
                if (node.catchClause) {
                    const catchEnv = new Env(env);
                    // bind the catch exception name as 'any'
                    catchEnv.defineValue(node.catchClause.name, anyType());
                    checkBlock(node.catchClause.block, catchEnv, currentFunctionType);
                }
                if (node.finallyBlock) {
                    checkBlock(node.finallyBlock, env, currentFunctionType);
                }
                break;
            }
            case "SwitchStmt": {
                const discType = checkExpr(node.discriminant, env);
                // check each case
                for (const c of node.cases) {
                    if (c.test) checkExpr(c.test, env);
                    for (const stmt of c.consequent) {
                        checkStatement(stmt, env, currentFunctionType);
                    }
                }
                break;
            }
            case "BreakStmt": {
                // nothing to check for break here
                break;
            }
            default:
                error("Unknown statement kind: " + node.kind, node);
        }
    }

    function checkBlock(node, env, currentFunctionType) {
        const blockEnv = new Env(env);
        for (const stmt of node.statements) {
            checkStatement(stmt, blockEnv, currentFunctionType);
        }
    }

    function checkExpr(node, env) {
        switch (node.kind) {
            case "NumberLiteral":
                return primitiveType("number");
            case "BigIntLiteral":
                return primitiveType("bigint");
            case "StringLiteral":
                return primitiveType("string");
            case "BoolLiteral":
                return primitiveType("boolean");
            case "Identifier": {
                const t = env.lookupValue(node.name);
                if (!t) {
                    // JS-style: unknown variables are `any` instead of hard error
                    return anyType();
                }
                return t;
            }
            case "AwaitExpr": {
                const inner = checkExpr(node.argument, env);
                // No special Promise handling; just propagate the inner type.
                return inner;
            }
            case "BinaryExpr": {
                // handle many binary operators: =, ==, !=, <, >, <=, >=, &&, ||, +, -, *, /
                if (node.op === "=") {
                    // assignment: left must be identifier or member expression or index
                    if (
                        !(
                            node.left.kind === "Identifier" ||
                            node.left.kind === "MemberExpr" ||
                            node.left.kind === "IndexExpr"
                        )
                    ) {
                        error("Invalid assignment target", node.left);
                        // still check right to collect more errors
                        checkExpr(node.right, env);
                        return anyType();
                    }
                    const rightType = checkExpr(node.right, env);
                    let leftType = null;
                    if (node.left.kind === "Identifier") {
                        leftType = env.lookupValue(node.left.name);
                        if (!leftType) {
                            // undeclared LHS is any
                            leftType = anyType();
                        }
                    } else if (node.left.kind === "MemberExpr") {
                        const objType = checkExpr(node.left.object, env);
                        if (!objType || (objType.kind !== "object" && objType.kind !== "array" && objType.kind !== "any")) {
                            error(
                                `Property access '${node.left.property}' on non-object type '${typeToString(objType)}'`,
                                node.left
                            );
                            leftType = anyType();
                        } else if (objType.kind === "object") {
                            leftType = objType.properties.get(node.left.property);
                            if (!leftType) {
                                error(
                                    `Property '${node.left.property}' does not exist on type '${typeToString(objType)}'`,
                                    node.left
                                );
                                leftType = anyType();
                            }
                        } else if (objType.kind === "array" && node.left.property === "length") {
                            // arr.length is number, but you probably don't assign to it anyway
                            leftType = primitiveType("number");
                        } else {
                            // objType is any
                            leftType = anyType();
                        }
                    } else { // IndexExpr
                        const objType = checkExpr(node.left.object, env);
                        if (!objType || (objType.kind !== "array" && objType.kind !== "any")) {
                            error(
                                `Index access on non-array type '${typeToString(objType)}'`,
                                node.left
                            );
                            leftType = anyType();
                        } else if (objType.kind === "array") {
                            const idxType = checkExpr(node.left.index, env);
                            if (idxType.kind !== "number" && idxType.kind !== "any") {
                                error(
                                    `Array index must be a number, got '${typeToString(idxType)}'`,
                                    node.left.index
                                );
                            }
                            leftType = objType.element;
                        } else {
                            leftType = anyType();
                        }
                    }
                    if (!isAssignable(rightType, leftType)) {
                        error(
                            `Cannot assign type '${typeToString(rightType)}' to '${typeToString(leftType)}'`,
                            node
                        );
                    }
                    return leftType;
                }

                const left = checkExpr(node.left, env);
                const right = checkExpr(node.right, env);

                if (left.kind === "any" || right.kind === "any") {
                    // Be permissive when any is involved
                    if (["==", "!=", "===", "!==", "<", ">", "<=", ">="].includes(node.op)) {
                        return primitiveType("boolean");
                    }
                    if (["&&", "||"].includes(node.op)) {
                        return primitiveType("boolean");
                    }
                    if (["+", "-", "*", "/"].includes(node.op)) {
                        return anyType();
                    }
                    return anyType();
                }

                if (node.op === "+") {
                    if (left.kind === "number" && right.kind === "number") {
                        return primitiveType("number");
                    }
                    if (left.kind === "string" && right.kind === "string") {
                        return primitiveType("string");
                    }
                    error(
                        `Operator '+' not supported for '${typeToString(left)}' and '${typeToString(right)}'`,
                        node
                    );
                    return anyType();
                }
                if (["-", "*", "/"].includes(node.op)) {
                    if (left.kind === "number" && right.kind === "number") {
                        return primitiveType("number");
                    }
                    error(
                        `Operator '${node.op}' not supported for '${typeToString(left)}' and '${typeToString(right)}'`,
                        node
                    );
                    return anyType();
                }

                // equality (support both double and triple-equals)
                if (["==", "!=", "===", "!=="].includes(node.op)) {
                    // allow equality for standard primitive kinds (now includes null/undefined/symbol)
                    if (left.kind === right.kind && ["number", "string", "boolean", "null", "undefined", "symbol"].includes(left.kind)) {
                        return primitiveType("boolean");
                    }
                    error(
                        `Operator '${node.op}' not supported for '${typeToString(left)}' and '${typeToString(right)}'`,
                        node
                    );
                    return anyType();
                }

                // relational
                if (["<", ">", "<=", ">="].includes(node.op)) {
                    if (left.kind === "number" && right.kind === "number") {
                        return primitiveType("boolean");
                    }
                    error(
                        `Operator '${node.op}' requires numeric operands, got '${typeToString(left)}' and '${typeToString(right)}'`,
                        node
                    );
                    return anyType();
                }

                // logical
                if (node.op === "&&" || node.op === "||") {
                    if (left.kind === "boolean" && right.kind === "boolean") {
                        return primitiveType("boolean");
                    }
                    error(
                        `Logical operator '${node.op}' requires boolean operands, got '${typeToString(left)}' and '${typeToString(right)}'`,
                        node
                    );
                    return anyType();
                }

                // fall-through
                return anyType();
            }
            case "ConditionalExpr": {
                const condType = checkExpr(node.cond, env);
                if (condType.kind !== "boolean" && condType.kind !== "any") {
                    error(`Condition in ternary must be boolean, got '${typeToString(condType)}'`, node);
                }
                const t1 = checkExpr(node.thenExpr, env);
                const t2 = checkExpr(node.elseExpr, env);
                if (t1.kind === "any" || t2.kind === "any") return anyType();
                // if identical
                if (JSON.stringify(t1) === JSON.stringify(t2)) return t1;
                if (isAssignable(t1, t2)) return t2;
                if (isAssignable(t2, t1)) return t1;
                error(`Ternary branches have incompatible types: '${typeToString(t1)}' and '${typeToString(t2)}'`, node);
                return anyType();
            }
            case "CallExpr": {
                // Special-case: array.map(callback)
                if (node.callee.kind === "MemberExpr" && node.callee.property === "map") {
                    const objType = checkExpr(node.callee.object, env);
                    if (!objType || (objType.kind !== "array" && objType.kind !== "any")) {
                        error(
                            `Attempting to call 'map' on non-array type '${typeToString(objType)}'`,
                            node.callee
                        );
                        return anyType();
                    }
                    if (node.args.length !== 1) {
                        error(
                            `Array.map expects 1 argument but got ${node.args.length}`,
                            node
                        );
                        return { kind: "array", element: anyType() };
                    }
                    const cb = node.args[0];
                    // callback can be ArrowFunction or other function value; handle ArrowFunction specially
                    if (cb.kind === "ArrowFunction") {
                        // bind parameter to element type
                        const fnEnv = new Env(env);
                        const elementType = objType.kind === "array" ? objType.element : anyType();
                        const paramName = cb.params[0] || null;
                        if (paramName) fnEnv.defineValue(paramName, elementType);
                        let retType;
                        if (cb.body.kind === "Block") {
                            // create a fake function type to check returns inside block
                            checkBlock(cb.body, fnEnv, {
                                kind: "function",
                                params: [elementType],
                                returnType: anyType()
                            });
                            // we don't infer return type from block easily; default to any
                            retType = anyType();
                        } else {
                            retType = checkExpr(cb.body, fnEnv);
                        }
                        return { kind: "array", element: retType };
                    } else {
                        // other callable: type-check normally
                        const cbType = checkExpr(cb, env);
                        if (!cbType || (cbType.kind !== "function" && cbType.kind !== "any")) {
                            error(
                                `map callback is not a function (got '${typeToString(cbType)}')`,
                                node
                            );
                            return { kind: "array", element: anyType() };
                        }
                        if (cbType.kind === "any") {
                            return { kind: "array", element: anyType() };
                        }
                        // ensure callback first param accepts element type
                        const paramType = cbType.params[0] || anyType();
                        const elementType = objType.kind === "array" ? objType.element : anyType();
                        if (!isAssignable(elementType, paramType)) {
                            error(
                                `map callback parameter type mismatch: expected '${typeToString(paramType)}' but got '${typeToString(elementType)}'`,
                                node
                            );
                        }
                        return { kind: "array", element: cbType.returnType };
                    }
                }

                const calleeType = checkExpr(node.callee, env);
                if (!calleeType || (calleeType.kind !== "function" && calleeType.kind !== "any")) {
                    error(
                        `Attempting to call non-function of type '${typeToString(calleeType)}'`,
                        node
                    );
                    return anyType();
                }

                if (calleeType.kind === "any") {
                    // don't type-check arguments when callee is any
                    node.args.forEach(arg => checkExpr(arg, env));
                    return anyType();
                }

                // If callee is a generic function (has typeParams), try to infer
                // concrete type arguments from the provided call arguments.
                if (calleeType.typeParams && Array.isArray(calleeType.typeParams) && calleeType.typeParams.length > 0) {
                    const mapping = new Map();
                    // first collect arg types
                    const argTypes = node.args.map(a => checkExpr(a, env));
                    // try to infer mapping from each parameter
                    for (let i = 0; i < calleeType.params.length && i < argTypes.length; i++) {
                        inferTypeParams(calleeType.params[i], argTypes[i], mapping);
                    }
                    // substitute parameter and return types
                    const instantiatedParams = calleeType.params.map(p => substituteType(p, mapping));
                    const instantiatedReturn = substituteType(calleeType.returnType, mapping);

                    if (node.args.length !== instantiatedParams.length) {
                        error(
                            `Function expects ${instantiatedParams.length} arguments but got ${node.args.length}`,
                            node
                        );
                    }
                    node.args.forEach((arg, idx) => {
                        const argType = argTypes[idx];
                        const paramType = instantiatedParams[idx] || anyType();
                        if (!isAssignable(argType, paramType)) {
                            error(
                                `Argument ${idx + 1} type mismatch: expected '${typeToString(paramType)}' but got '${typeToString(argType)}'`,
                                node
                            );
                        }
                    });
                    return instantiatedReturn;
                }

                if (node.args.length !== calleeType.params.length) {
                    error(
                        `Function expects ${calleeType.params.length} arguments but got ${node.args.length}`,
                        node
                    );
                }
                node.args.forEach((arg, idx) => {
                    const argType = checkExpr(arg, env);
                    const paramType = calleeType.params[idx] || anyType();
                    if (!isAssignable(argType, paramType)) {
                        error(
                            `Argument ${idx + 1} type mismatch: expected '${typeToString(paramType)}' but got '${typeToString(argType)}'`,
                            node
                        );
                    }
                });
                return calleeType.returnType;
            }
            case "MemberExpr": {
                const objType = checkExpr(node.object, env);
                if (!objType) {
                    return anyType();
                }

                // Unknown / any: be completely permissive
                if (objType.kind === "any") {
                    return anyType();
                }

                // Static class members
                if (objType.kind === "class") {
                    const propType = objType.methods.get(node.property);
                    if (!propType) {
                        error(
                            `Static property '${node.property}' does not exist on class '${objType.name}'`,
                            node
                        );
                        return anyType();
                    }
                    return propType;
                }

                // Arrays: keep `length` precise, treat all other properties/methods as `any`
                if (objType.kind === "array") {
                    if (node.property === "length") {
                        return primitiveType("number");
                    }
                    // pop, push, map, filter, etc. — all treated as `any`,
                    // and CallExpr will handle special cases like map.
                    return anyType();
                }

                // Structural object types: still strict (good for your TS-like types)
                if (objType.kind === "object") {
                    const propType = objType.properties.get(node.property);
                    if (!propType) {
                        error(
                            `Property '${node.property}' does not exist on type '${typeToString(objType)}'`,
                            node
                        );
                        return anyType();
                    }
                    return propType;
                }

                // Primitives / everything else:
                // (number, string, boolean, function, class, etc.)
                // Treat property as `any` so built-ins like toString, trim, toFixed
                // don't produce hard errors.
                return anyType();
            }
            case "IndexExpr": {
                const objType = checkExpr(node.object, env);
                if (!objType) return anyType();
                if (objType.kind === "any") {
                    // any[index] is any
                    checkExpr(node.index, env);
                    return anyType();
                }
                if (objType.kind !== "array") {
                    error(`Index access on non-array type '${typeToString(objType)}'`, node);
                    return anyType();
                }
                // index expression should be numeric
                const idxType = checkExpr(node.index, env);
                if (idxType.kind !== "number" && idxType.kind !== "any") {
                    error(
                        `Array index must be a number, got '${typeToString(idxType)}'`,
                        node.index
                    );
                }
                return objType.element;
            }
            case "NewExpr": {
                // evaluate callee type
                const calleeType = checkExpr(node.callee, env);
                // if class-style constructor
                if (calleeType && calleeType.kind === "class") {
                    const ctor = calleeType.ctor;
                    // simple arg count check
                    if (node.args.length !== ctor.params.length) {
                        error(
                            `Constructor expects ${ctor.params.length} arguments but got ${node.args.length}`,
                            node
                        );
                    }
                    node.args.forEach((a, idx) => {
                        const at = checkExpr(a, env);
                        const pt = ctor.params[idx] || anyType();
                        if (!isAssignable(at, pt)) {
                            error(
                                `Constructor argument ${idx + 1} type mismatch: expected '${typeToString(pt)}' but got '${typeToString(at)}'`,
                                a
                            );
                        }
                    });
                    // instances have methods as properties
                    return calleeType.instance || ctor.returnType || anyType();
                }
                if (!calleeType || calleeType.kind === "any") {
                    // still check args
                    node.args.forEach(a => checkExpr(a, env));
                    return anyType();
                }
                if (calleeType.kind === "function") {
                    // treat new on function as returning the function's returnType
                    node.args.forEach((a, idx) => checkExpr(a, env));
                    return calleeType.returnType || anyType();
                }
                error("Cannot use 'new' on non-constructor type", node);
                node.args.forEach(a => checkExpr(a, env));
                return anyType();
            }
            case "ArrowFunction": {
                // produce a function type with unknown param/return types (may be checked in call-site)
                const paramTypes = node.params.map(_ => anyType());
                return functionType(paramTypes, anyType());
            }
            case "ArrayLiteral": {
                const elems = node.elements;
                if (elems.length === 0) return { kind: "array", element: anyType() };
                const first = checkExpr(elems[0], env);
                for (let i = 1; i < elems.length; i++) {
                    const t = checkExpr(elems[i], env);
                    if (!isAssignable(t, first)) {
                        error(
                            `Array literal elements have incompatible types: '${typeToString(first)}' and '${typeToString(t)}'`,
                            node
                        );
                    }
                }
                return { kind: "array", element: first };
            }
            case "FunctionExpr": {
                // If function expression has generic type params, make them available
                // when resolving its parameter/return annotations.
                const typeParams = node.typeParams || null;
                const resolvedTypeEnv = new Env(env);
                if (typeParams) {
                    typeParams.forEach(tp => resolvedTypeEnv.defineType(tp, { kind: "typeParam", name: tp }));
                }

                // build function type from declared param types (or any) and declared return type (or any)
                const paramTypes = node.params.map(p => (p.type ? resolveTypeExpr(p.type, resolvedTypeEnv) : anyType()));
                const retType = node.returnType ? resolveTypeExpr(node.returnType, resolvedTypeEnv) : anyType();
                const fnType = functionType(paramTypes, retType, typeParams);

                // check body with new env and type params visible
                const fnEnv = new Env(env);
                if (typeParams) {
                    typeParams.forEach(tp => fnEnv.defineType(tp, { kind: "typeParam", name: tp }));
                }
                node.params.forEach((p, idx) => fnEnv.defineValue(p.name, paramTypes[idx]));
                // For generic function expressions, relax return-type checking
                // inside the body (use any return) so that returns of type
                // parameters don't produce an error at declaration time.
                if (typeParams && typeParams.length > 0) {
                    const relaxed = functionType(paramTypes, anyType(), null);
                    checkBlock(node.body, fnEnv, relaxed);
                } else {
                    checkBlock(node.body, fnEnv, fnType);
                }
                return fnType;
            }
            case "UpdateExpr": {
                const t = checkExpr(node.argument, env);
                if (t.kind === "any") return anyType();
                if (t.kind !== "number") {
                    error(
                        `Operator '${node.operator}' requires a numeric operand, got '${typeToString(t)}'`,
                        node
                    );
                    return anyType();
                }
                // In JS, x++ returns number; we keep it number
                return primitiveType("number");
            }
            case "ObjectLiteral": {
                const props = new Map();
                for (const p of node.properties) {
                    const valueType = checkExpr(p.value, env);
                    props.set(p.name, valueType);
                }
                return { kind: "object", properties: props };
            }
            default:
                error("Unknown expression kind: " + node.kind, node);
                return anyType();
        }
    }

    function isAssignable(from, to, _visited) {
        // Use a WeakMap of seen pairs to handle recursive/cyclic types
        const visited = _visited || new WeakMap();
        if (!from || !to) return false;
        if (from === to) return true;
        if (from.kind === "any" || to.kind === "any") return true;

        // short-circuit for primitives
        if (from.kind !== "object" && from.kind !== "function" && from.kind !== "array") {
            return from.kind === to.kind;
        }

        // detect cycles: if we've compared this pair before, assume true to break recursion
        let seenForFrom = visited.get(from);
        if (!seenForFrom) {
            seenForFrom = new Set();
            visited.set(from, seenForFrom);
        }
        if (seenForFrom.has(to)) return true;
        seenForFrom.add(to);

        // arrays: element-wise assignable
        if (from.kind === "array" && to.kind === "array") {
            return isAssignable(from.element, to.element, visited);
        }

        if (from.kind === to.kind) {
            if (from.kind === "function") {
                if (from.params.length !== to.params.length) return false;
                for (let i = 0; i < from.params.length; i++) {
                    if (!isAssignable(from.params[i], to.params[i], visited)) return false;
                }
                return isAssignable(from.returnType, to.returnType, visited);
            }
            if (from.kind === "object") {
                for (const [name, tTo] of to.properties.entries()) {
                    const tFrom = from.properties.get(name);
                    if (!tFrom || !isAssignable(tFrom, tTo, visited)) return false;
                }
                return true;
            }
            return true;
        }
        return false;
    }

    checkProgram(ast, globalEnv);

    return { errors };
}

/* =============== JS EMITTER (types stripped) =============== */

function emitJS(ast) {
    function emitProgram(node) {
        return node.body
            .map(emitStmt)
            .filter(Boolean)
            .join("\n");
    }

    function emitStmt(node) {
        switch (node.kind) {
            case "ExportDecl": {
                // Emit 'export ' followed by the inner declaration. If the inner
                // declaration is a type alias, there's nothing to emit at runtime.
                const inner = node.decl;
                if (inner.kind === "TypeDecl") return "";
                return `export ${emitStmt(inner)}`;
            }
            case "TypeDecl":
                return ""; // no runtime type aliases
            case "VarDecl":
                return `let ${node.name}` + (node.init ? ` = ${emitExpr(node.init)}` : "") + `;`;
            case "ConstDecl":
                return `const ${node.name}` + (node.init ? ` = ${emitExpr(node.init)}` : "") + `;`;
            case "FuncDecl": {
                const params = node.params.map(p => p.name).join(", ");
                const body = emitBlock(node.body);
                const asyncPrefix = node.isAsync ? "async " : "";
                return `${asyncPrefix}function ${node.name}(${params}) ${body}`;
            }
            case "IfStmt": {
                const elsePart = node.elseBlock ? ` else ${emitStmt(node.elseBlock)}` : "";
                return `if (${emitExpr(node.cond)}) ${emitStmt(node.thenBlock)}${elsePart}`;
            }
            case "WhileStmt": {
                return `while (${emitExpr(node.cond)}) ${emitStmt(node.body)}`;
            }
            case "ForStmt": {
                const initStr = node.init
                    ? (node.init.kind === "VarDecl" || node.init.kind === "ConstDecl"
                        ? emitStmt(node.init).replace(/;$/, "")
                        : emitStmt(node.init).replace(/;$/, ""))
                    : "";
                const testStr = node.test ? emitExpr(node.test) : "";
                const updateStr = node.update ? emitExpr(node.update) : "";
                return `for (${initStr}; ${testStr}; ${updateStr}) ${emitStmt(node.body)}`;
            }
            case "ForOfStmt": {
                const left = `${node.left.declKind} ${node.left.name}`;
                return `for (${left} of ${emitExpr(node.right)}) ${emitStmt(node.body)}`;
            }
            case "ForInStmt": {
                const left = `${node.left.declKind} ${node.left.name}`;
                return `for (${left} in ${emitExpr(node.right)}) ${emitStmt(node.body)}`;
            }
            case "SwitchStmt": {
                const disc = emitExpr(node.discriminant);
                const parts = node.cases.map(c => {
                    if (c.test) {
                        const stmts = c.consequent.map(s => emitStmt(s)).join("\n");
                        return `  case ${emitExpr(c.test)}:\n${stmts.replace(/^/gm, '    ')}`;
                    } else {
                        const stmts = c.consequent.map(s => emitStmt(s)).join("\n");
                        return `  default:\n${stmts.replace(/^/gm, '    ')}`;
                    }
                }).join("\n");
                return `switch (${disc}) {\n${parts}\n}`;
            }
            case "BreakStmt": {
                return `break;`;
            }
            case "ClassDecl": {
                const fields = node.fields || [];

                // instance field initializers (non-static) that have an initializer
                const instanceInits = fields.filter(f => !f.isStatic && f.init);
                // static field initializers
                const staticInits = fields.filter(f => f.isStatic && f.init);

                // find constructor method if present
                const ctor = node.methods.find(m => m.isConstructor);

                const otherMethods = node.methods.filter(m => !m.isConstructor);

                // emit constructor: if one exists, prepend field initializers into its body;
                // otherwise, if we have instance field initializers, create a default ctor
                let ctorStr = "";
                if (ctor) {
                    const params = ctor.params.map(p => p.name).join(", ");
                    const innerLines = [];
                    for (const f of instanceInits) {
                        innerLines.push(`this.${f.name} = ${emitExpr(f.init)};`);
                    }
                    // append original constructor body statements
                    const bodyStmts = ctor.body.statements.map(s => emitStmt(s)).filter(Boolean);
                    if (bodyStmts.length > 0) innerLines.push(...bodyStmts);
                    const inner = innerLines.map(l => `    ${l}`).join("\n");
                    ctorStr = `  constructor(${params}) {\n${inner}\n  }`;
                } else if (instanceInits.length > 0) {
                    const innerLines = instanceInits.map(f => `    this.${f.name} = ${emitExpr(f.init)};`);
                    ctorStr = `  constructor() {\n${innerLines.join("\n")}\n  }`;
                }

                const methodParts = otherMethods.map(m => {
                    const params = m.params.map(p => p.name).join(", ");
                    const staticPrefix = m.isStatic ? "static " : "";
                    return `  ${staticPrefix}${m.name}(${params}) ${emitBlock(m.body)}`;
                }).join("\n\n");

                const classBodyParts = [ctorStr, methodParts].filter(Boolean).join("\n\n");
                const extendsPart = node.superClass ? ` extends ${node.superClass}` : "";
                let classCode = `class ${node.name}${extendsPart} {\n${classBodyParts.replace(/^/gm, '  ')}\n}`;

                // emit static field initializers after the class if present
                if (staticInits.length > 0) {
                    const staticLines = staticInits.map(f => `${node.name}.${f.name} = ${emitExpr(f.init)};`);
                    classCode += "\n" + staticLines.join("\n");
                }

                return classCode;
            }
            case "Block":
                return emitBlock(node);
            case "TryStmt": {
                const tryPart = `try ${emitStmt(node.tryBlock)}`;
                const catchPart = node.catchClause ? ` catch (${node.catchClause.name}) ${emitStmt(node.catchClause.block)}` : "";
                const finallyPart = node.finallyBlock ? ` finally ${emitStmt(node.finallyBlock)}` : "";
                return `${tryPart}${catchPart}${finallyPart}`;
            }
            case "ReturnStmt":
                if (node.expr === null) return "return;";
                return `return ${emitExpr(node.expr)};`;
            case "ExprStmt":
                return `${emitExpr(node.expr)};`;
            default:
                throw new Error("Unknown stmt kind: " + node.kind);
        }
    }

    function emitBlock(node) {
        const inner = node.statements
            .map(s => emitStmt(s))
            .filter(Boolean)
            .map(s => "  " + s)
            .join("\n");
        return "{\n" + inner + "\n}";
    }

    function emitExpr(node) {
        switch (node.kind) {
            case "NumberLiteral":
                return String(node.value);
            case "BigIntLiteral":
                // value already contains the trailing 'n' (e.g. "123n")
                return String(node.value);
            case "StringLiteral":
                return `"${node.value}"`;
            case "BoolLiteral":
                return node.value ? "true" : "false";
            case "Identifier":
                return node.name;
            case "BinaryExpr":
                return `${emitExpr(node.left)} ${node.op} ${emitExpr(node.right)}`;
            case "CallExpr":
                return `${emitExpr(node.callee)}(${node.args.map(emitExpr).join(", ")})`;
            case "MemberExpr":
                return `${emitExpr(node.object)}.${node.property}`;
            case "IndexExpr":
                return `${emitExpr(node.object)}[${emitExpr(node.index)}]`;
            case "FunctionExpr": {
                const params = node.params.map(p => p.name).join(", ");
                const namePart = node.name ? ` ${node.name}` : "";
                const asyncPrefix = node.isAsync ? "async " : "";
                return `${asyncPrefix}function${namePart}(${params}) ${emitBlock(node.body)}`;
            }
            case "ConditionalExpr":
                return `${emitExpr(node.cond)} ? ${emitExpr(node.thenExpr)} : ${emitExpr(node.elseExpr)}`;
            case "ArrowFunction": {
                const params =
                    node.params.length === 1
                        ? node.params[0]
                        : `(${node.params.join(", ")})`;
                const asyncPrefix = node.isAsync ? "async " : "";
                if (node.body.kind === "Block") {
                    return `${asyncPrefix}${params} => ${emitBlock(node.body)}`;
                }
                return `${asyncPrefix}${params} => ${emitExpr(node.body)}`;
            }
            case "AwaitExpr":
                return `await ${emitExpr(node.argument)}`;
            case "ArrayLiteral":
                return `[` + node.elements.map(emitExpr).join(", ") + `]`;
            case "UpdateExpr":
                if (node.prefix) {
                    return `${node.operator}${emitExpr(node.argument)}`;
                }
                return `${emitExpr(node.argument)}${node.operator}`;
            case "ObjectLiteral": {
                const parts = node.properties.map(
                    p => `${p.name}: ${emitExpr(p.value)}`
                );
                return `{ ${parts.join(", ")} }`;
            }
            case "NewExpr": {
                return `new ${emitExpr(node.callee)}(${node.args.map(emitExpr).join(", ")})`;
            }
            default:
                throw new Error("Unknown expr kind: " + node.kind);
        }
    }

    return emitProgram(ast);
}

export function transpile(source) {
    // Extract top-level ES module import statements and preserve them.
    // Simple approach: match import ...; on a single line (covers common cases like
    // `import { a, b } from "./mod.js";`). We remove them from the source
    // before tokenizing/parsing so the existing parser (which doesn't handle
    // 'import' as a keyword) won't choke. After emitting JS, we'll prepend the
    // imports back to the output.
    // Match import statements that end on the same line (don't accidentally
    // consume subsequent statements on the same line). This requires the
    // terminating semicolon to be followed by a line break or end-of-input.
    const importRegex = /^\s*import\b[^\n]*?;(?=\s*(?:\r?\n|$))/gim;
    const imports = [];
    let cleanedSource = source.replace(importRegex, (m) => {
        imports.push(m.trim());
        return "";
    });

    const destructureRegex = /\{\s*(?:[^{}]|\{[^{}]*\})*?\}\s*(?==)/gm;

    if (destructureRegex.test(source)) {
        source.match(destructureRegex).forEach(match => {
            console.log("Destructuring assignment found:", match);
        });
    }

    const spreadRegex = /\.\.\.\s*\w+/g;

    if (spreadRegex.test(source)) {
        source.match(spreadRegex).forEach(match => {
            console.log("Spread syntax found:", match);
        });
    }

    const interfaceRegex = /\binterface\s+\w+\s*{[^}]*}/gm;

    if (interfaceRegex.test(source)) {
        source.match(interfaceRegex).forEach(match => {
            console.log("Interface declaration found:", match);
        });
    }

    const tokens = tokenizeTS(cleanedSource);
    const ast = parse(tokens);

    // Now typeCheck returns { errors: [{ msg, pos }] }
    const { errors } = typeCheck(ast, source);

    if (errors.length > 0) {
        console.log("Type errors:");

        // Pre-split the input into lines
        const lines = source.split(/\r?\n/);

        for (const err of errors) {
            const { msg, pos } = err;

            // Convert absolute pos -> line + column
            let line = 1, col = 1;
            {
                for (let i = 0; i < source.length; i++) {
                    if (i === pos) break;
                    if (source[i] === '\n') {
                        line++;
                        col = 1;
                    } else {
                        col++;
                    }
                }
            }

            const codeLine = lines[line - 1] || "";
            const prefix = `   ${line} | `;

            console.log(`\n• ${msg}`);
            console.log(prefix + codeLine);
            console.log(" ".repeat(prefix.length) + " ".repeat(col - 1) + "^");
        }

        throw new Error("Type checking failed");
    } else {
        console.log("No type errors.");
    }

    const js = emitJS(ast);
    // Reattach preserved imports at the top of the emitted JS, separated by a blank line.
    if (imports.length > 0) {
        return imports.join("\n") + "\n\n" + js;
    }
    return js;
}
