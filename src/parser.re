open Js.String;
open Utils;
open Evaluate;
open Combinators;

/**
parsers
 */
let pChar = (charToMatch: string) => {
  let innerFn = input => {
    let firstChar = charAt(0, input);
    if (charToMatch == firstChar) {
      Ok((firstChar, slice(~from=1, ~to_=length(input), input)));
    } else {
      Error
        ("expected: " ++ charToMatch ++ "; but got: " ++ input);
        /* Error(
             input,
           ); */
    };
  };
  Parser(innerFn);
};

let addCharParser = pChar("+");
let minusCharParser = pChar("-");
let emptyParser = pChar("");
let digitalCharParser =
  [|"0", "1", "2", "3", "4", "5", "6", "7", "8", "9"|]
  |> Js.Array.map(pChar)
  |> anyOf;

let mapFn = (s1, s2) => s1 ++ s2;
let addThen_ = (p1, p2) => lift2(mapFn, p1, p2);
let (&&=) = addThen_;

let rec many = parser =>
  parser
  &&= {
    let innerFn = input => {
      input |> run(many(parser)) &!= (_ => Ok(("", input)));
    };
    Parser(innerFn);
  }
  &| emptyParser;

/* let input = "13456a";
   let p = many(digitalCharParser);
   switch (run(p, input)) {
   | Ok((c, remain)) =>
     "current:  " ++ c |> Js.log;
     "remain:  " ++ remain |> Js.log;
   | Error(_) => "failed" ++ input |> Js.log
   }; */

/**
  expression evaluate
 */
let positiveDigitalParser = many(digitalCharParser) &= (res => Digital(res));
let digitalParser =
  minusCharParser
  &> positiveDigitalParser
  &= (((_op, d)) => Minus(Digital("0"), d))
  &| positiveDigitalParser;

let addParser =
  digitalParser
  &> addCharParser
  &> positiveDigitalParser
  &= ((((d1, _op), d2)) => Add(d1, d2));

let minusParser =
  digitalParser
  &> minusCharParser
  &> positiveDigitalParser
  &= ((((d1, _op), d2)) => Minus(d1, d2));

let expressionParser = addParser &| minusParser &| digitalParser;
let rec parse = input => {
  switch (input |> run(expressionParser)) {
  | Ok((exp, remain)) =>
    if (remain == "") {
      string_of_int(evaluate(exp));
    } else {
      parse(string_of_int(evaluate(exp)) ++ remain);
    }
  | Error(e) => "failed: " ++ e
  };
};

let logParse = input => input |> parse |> Js.log;

/* logParse("12+3-4"); */