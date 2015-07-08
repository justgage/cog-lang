type primitive

type expression = expression | primitive

type if_ex = {
  condition : bool;
  if_true : expression;
  if_false : expression 
}



type statement =
  | If of (Expresion * Expression )

type 'a token =
  | String of 'a
  | Expression of 'a
  | Statement of statement 'a
