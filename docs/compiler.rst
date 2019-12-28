The compiler
============

The compiler implements the GECKO-AS and GECKO-ITS programming languages
and lowers them to the GECKO-C internal language.

Lowering GECKO-AS and GECKO-ITS
-------------------------------

The translation unit consists of interface files and source files.
Interface files and source files have the same format,
but value definition implementations are only considered when they come from source files.

A translation unit is parsed to an AST.
The AST is then lowered to GECKO-C.

Compiling GECKO-C
-----------------

Compiling a GECKO-C translation unit is a seven-phase process.
Each phase receives the output of the previous phase as its input.
For each phase there is a unique Haskell module in the source folder.

Phase I: Find type definitions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Find all type definitions and record their names and kinds.

Phase II: Analyze type definitions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Find all type definitions and record their fields and constructors.

Phase III: Find value definitions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Find all value definitions and record their names and types.

Phase IV: Analyze value definitions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Find all value definitions and type check their implementations.

Phase V: Generate LLVM IR
~~~~~~~~~~~~~~~~~~~~~~~~~

Find all value definitions and generate LLVM IR for their implementations.

Phase VI: Generate code in the target language
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Instruct LLVM to generate code in the target langauge.

Phase VII: Write out an object file
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Instruct LLVM to write out an object file.
