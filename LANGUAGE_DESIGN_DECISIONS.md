# Language Design Decisions

## Implicit type conversions

### Range
- Type of expressions should be `integer`, otherwise we expect compile-time exception.

### Expressions
- No implicit type conversion inside expression.
  - Invalid code: `var a: integer is 1 + false;`
  - Valid code: `var a: integer is false;`

## RoutineCall
Expression, not statement

## Separating statements 
Semicolon instead of new line (?)

## Naming convention

### Routines
PascalCase (e.g. `RoutineName`)

### Variables
camelCase (e.g. `variableName`)

## Predefined types

### Boolean
Internal representation:

Range:

### Integer
Internal representation:

Range:

### Real 
Internal representation: 

Range: 