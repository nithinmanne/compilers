# Compilers [![Build Status](https://travis-ci.com/nithinmanne/compilers.svg?token=qkM8xJGEEgqsq7NrzJ3U&branch=master)](https://travis-ci.com/nithinmanne/compilers)
Compilers Assignment Repository

## Usage
```
python3 ekcc.py [-h|-?] [-v] [-O] [-emit-ast|-emit-llvm] [-jit] [-o <output-file>] <input-file> [args]

The Extended-Kaleidoscope Language Compiler

positional arguments:
  input-file      Input .ek File to Compile
  args            Optional arguments when performing JIT and execution

optional arguments:
  -h, -?          Show this help message and exit
  -v              Enable Verbose mode
  -O              Enable Optimization
  -emit-ast       Dump AST in YAML Format to the Output File
  -emit-llvm      Dump LLVM IR to the Output File
  -jit            Run the compiled Code
  -o output-file  Output File to emit AST or LLVM IR
```

## Prerequisites
1.  [Python 3](https://www.python.org/) (version >= 3.5)
2.  [PLY](https://www.dabeaz.com/ply/) used for Lexing/Parsing (version >= 3.11)
3.  [PyYAML](https://github.com/yaml/pyyaml/) used for generation YAML output (version >= 5.1.2)
4.  [llvmlite](https://github.com/numba/llvmlite) used to interface with LLVM (version == 0.29.0)

Use requirements.txt to install all dependencies
```
python3 -m pip install --user -r requirements.txt
```

## Authors

* **Naga Nithin Manne** - [nithinmanne](https://github.com/nithinmanne)
* **Dipti Sengupta**
