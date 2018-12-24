type expression =
  | Digital(string)
  | Add(expression, expression)
  | Minus(expression, expression);

let rec evaluate = (expr: expression) => {
  switch (expr) {
  | Digital(d) => int_of_string(d)
  | Add(d1, d2) => evaluate(d1) + evaluate(d2)
  | Minus(d1, d2) => evaluate(d1) - evaluate(d2)
  };
};

let rec print = (expr: expression) => {
  let res = "";
  switch (expr) {
  | Digital(d) => res ++ d
  | Add(d1, d2) =>
    res ++ "  " ++ "Add(" ++ print(d1) ++ "," ++ print(d2) ++ ")"
  | Minus(d1, d2) =>
    res ++ "  " ++ "Minus(" ++ print(d1) ++ "," ++ print(d2) ++ ")"
  };
};
/* let expr = Minus(Add(Digital("3"), Digital("5")), Digital("3")); */

/* evaluate(expr) |> Js.log; */