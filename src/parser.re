open Evaluate;
type result('a) =
  | Ok('a, string)
  | Error(string);

type parser = (expression, string) => result(expression);
let (@|) = (p1, p2, input: string) => {
  switch (p1(input)) {
  | Ok(exp, remain) => Ok(exp, remain)
  | Error(_) => p2(input)
  };
};

let (@<<) = (onOk, onError, res) => {
  switch (res) {
  | Ok(r, remain) => onOk(r, remain)
  | Error(err) => onError(err)
  };
};

let isChar = (isChar_: string => bool, input: string) => {
  let c = input |> Js.String.charAt(0);
  if (isChar_(c)) {
    Ok(c, input |> Js.String.slice(~from=1, ~to_=Js.String.length(input)));
  } else {
    Error(input);
  };
};

let isDigitalChar = isChar(c => "0123456789" |> Js.String.includes(c));
let isAddChar = isChar(c => c == "+");
let isMinusChar = isChar(c => c == "-");
let isEmptyChar = isChar(c => c == "");

let digitalParser_ = (input: string) => {
  input
  |> isDigitalChar
  |> ((c, remain) => Ok(Digital(c), remain))
  @<< (err => Error(err));
};

let emptyParser = (input: string) => {
  input |> isEmptyChar |> ((_, _) => Ok("", input)) @<< (_ => Error(input));
};

let rec many = (res, pChar, input: string) => {
  input
  |> emptyParser
  |> ((_, _) => Error(input))
  @<< (
    e => {
      e
      |> pChar
      |> (
        (char_, remain) => {
          remain
          |> emptyParser
          |> ((_, _) => Ok(res ++ char_, ""))
          @<< (_ => many(res ++ char_, pChar, remain));
        }
      )
      @<< (err => Ok(res, err));
    }
  );
};

let digitalParser = (input: string) => {
  input
  |> many("", isDigitalChar)
  |> (
    (d, remain) => {
      d
      |> emptyParser
      |> ((_, _) => Error(input))
      @<< (_ => Ok(Digital(d), remain));
    }
  )
  @<< (_ => Error(input));
};

let parserGenerator =
    (
      opSplitter,
      onOk: (expression, expression) => expression,
      exp1: expression,
      input: string,
    ) => {
  input
  |> opSplitter
  |> (
    (_, opRemain) => {
      opRemain
      |> digitalParser
      |> ((exp2, remain2) => Ok(onOk(exp1, exp2), remain2))
      @<< (err => Error(err));
    }
  )
  @<< (err => Error(err));
};

let addParser = parserGenerator(isAddChar, (exp1, exp2) => Add(exp1, exp2));
let minusParser =
  parserGenerator(isMinusChar, (exp1, exp2) => Minus(exp1, exp2));

let rec parser_ = (exp: expression, input: string): result(expression) => {
  let expressionParser = addParser(exp) @| minusParser(exp);
  input
  |> emptyParser
  |> ((_, _) => Ok(exp, input))
  @<< (
    input_ => {
      input_ |> expressionParser |> parser_ @<< (err => Error(err));
    }
  );
};

let negativeDigitalParser = (input: string) => {
  input
  |> isMinusChar
  |> (
    (_, remain) => {
      remain
      |> digitalParser
      |> ((d, remain2) => Ok(Minus(Digital("0"), d), remain2))
      @<< (_ => Error(input));
    }
  )
  @<< (_ => digitalParser(input));
};
let parser = (input: string) =>
  input
  |> emptyParser
  |> ((_, _) => Error(input))
  @<< negativeDigitalParser
  |> parser_
  @<< (_ => Error(input))
  |> ((exp, _) => string_of_int(evaluate(exp)))
  @<< (_ => "failed: " ++ input);

let logParse = (input: string) => input |> parser |> Js.log;

logParse("1e23e");
logParse("123+1");