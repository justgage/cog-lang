val ast : (int * token) of list

type token =
  | STRING of string
  | FLOAT of float
  | EXPRESSION of expression
  | BOOL of boolean
  | WHITESPACE
  | LIST of list
  | STATEMENT of statement

type statement =
  | DISPLAY
  | BOX
  | ASSIGNMENT
  | REPEAT_TILL
  | 

type list

type expression =
  | FUNCTION
  | ADDITION
  | DIVISION

type boolean =
  | TRUE
  | FALSE


parse : 
