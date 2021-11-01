# Minimal Homebrew Language (MHL)
MHL is a simple imperative language primarily targeting low-performance microprocessors.  
It is compiled into a simple intermediate bytecode that can then be cross-compiled to your target instruction set.

## Examples
```
const SOME_CONST: int = 0xF;

fn some_function(arg: word): long {
    var val: long = arg + SOME_CONST;
    return val + 1;
}

struct SomeStruct {
    field1: uword,
    field2: char*,
    field3: int[10],
}
```

## Grammar

### Identifier
```
[a-zA-Z_][a-zA-Z0-9_]*
```

### IntegerLiteral
```
  "0x" [0-9a-fA-F_]+
| "0o" [0-7_]+
| "0b" [01_]+
| [0-9_]+
```

### CharLiteral
```
"'" (   "\n"
      | "\r"
      | "\t"
      | "\'"
      | <unicode char>
) "'"
```

### StringLiteral
```
"\"" (   "\n"
       | "\r"
       | "\t"
       | "\""
       | <unicode char>
)* "\""
```

### Type
```
  "word"
| "uword"
| "int"
| "uint"
| "long"
| "ulong"
| "char"
| Identifier
| Type "*"
| Type "[" IntegerLiteral "]"
```

### UnaryOperator
```
  "+"
| "-"
| "!"
| "&"
| "*"
```

### BinaryOperator
```
  "+"
| "-"
| "*"
| "/"
| "%"
| "<<"
| ">>"
| "&&"
| "||"
| "&"
| "|"
| "^"
| "=="
| "!="
| "<"
| "<="
| ">"
| ">="
```

### AssignmentOperator
```
  "="
| "+="
| "-="
| "*="
| "/="
| "%="
| "<<="
| ">>="
| "&="
| "|="
| "^="
```

### Expression
```
  IntegerLiteral
| CharLiteral
| StringLiteral
| Identifier
| "sizeof(" Type ")"
| Identifier "(" ( Expression "," )* ")"
| Expression "[" Expression "]"
| Expression "." Identifier
| "<" Type ">" Expression
| UnaryOperator Expression
| Expression BinaryOperator Expression
| "(" Expression ")"
```

### ConstantDeclaration
```
"const" Identifier ":" Type "=" Expression ";"
```

### VariableDeclaration
```
  "var" Identifier ":" Type "=" Expression ";"
| "var" Identifier ":" Type ";"
```

### Assignment
```
Expression AssignmentOperator Expression ";"
```

### IfStatement
```
  "if" Expression "{" Statement* "}" "else" "{" Statement* "}"
| "if" Expression "{" Statement* "}"
```

### WhileLoop
```
"while" Expression "{" Statement* "}"
```

### BreakStatement
```
  "break" IntegerLiteral ";"
| "break" ";"
```

### ContinueStatement
```
  "continue" IntegerLiteral ";"
| "continue" ";"
```

### ReturnStatement
```
  "return" Expression ";"
| "return" ";"
```

### Statement
```
  ConstantDeclaration
| VariableDeclaration
| Assignment
| IfStatement
| WhileLoop
| BreakStatement
| ContinueStatement
| ReturnStatement
```

### Argument
```
Identifier ":" Type
```

### FunctionDefinition
```
  "fn" Identifier "(" ( Argument "," )* ")" ":" Type "{" Statement* "}"
| "fn" Identifier "(" ( Argument "," )* ")" "{" Statement* "}"
```

### Field
```
Identifier ":" Type
```

### StructDefinition
```
"struct" Identifier "{" ( Field "," )* "}"
```

### UnionDefinition
```
"union" Identifier "{" ( Field "," )* "}"
```

### TopLevelStatement
```
  ConstantDefinition
| FunctionDefinition
| StructDefinition
| UnionDefinition
```

### Program
```
TopLevelStatement*
```
