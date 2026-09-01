# cpp-mython

`cpp-mython` is a Yandex.Practicum C++ course project implementing a small Python-like language through lexical analysis, parsing, an AST-based runtime, and component tests.

---

## Features

- **Lexer**: Converts raw Mython source code into a stream of tokens.
- **Parser**: Builds an Abstract Syntax Tree (AST) from the token stream.
- **Runtime**: Executes the AST, handling variables, expressions, and control structures.
- **Unit Tests**: Extensive testing for each component of the interpreter.

## Structure and build notes

- Lexer, parser, runtime, statements, and their test helpers are under `mython/`.
- Requires a C++17-compatible compiler.
- The repository does not include a portable project-level build configuration.
