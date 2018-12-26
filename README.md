# block_of_ocaml
Print AST of OCaml code as XML

## How to use

To compile, just execute: ```make```
```bash
$ ./block_of_ocaml "2 + 1"
<block type="int_arithmetic_typed"><field name="OP_INT">ADD_INT</field><value name="A"><block type="int_typed"><field name="INT">2</field></block></value><value name="B"><block type="int_typed"><field name="INT">1</field></block></value></block>
```

## Running tests

```bash
$ python test.py
```

## Compiling to JavaScript

You might need to run js_of_ocaml with options such as +weak.js, +toplevel.js, and +dynlink.js to provide missing primitives.

```bash
$ make bcl
$ make -C web
$ js_of_ocaml web/converter
```

Function named ```blockOfOCaml``` is exposed to JavaScript.

```javascript
> blockOfOCaml("2 + 1")
"<block type="int_arithmetic_typed"><field name="OP_INT">ADD_INT</field><value name="A"><block type="int_typed"><field name="INT">2</field></block></value><value name="B"><block type="int_typed"><field name="INT">1</field></block></value></block>"
```

# References

* [OCaml Blockly](https://github.com/harukamm/typed-blockly)
