open Evaluate;
type result =
  | Ok(string, string)
  | Error(string);

type parser = string => result;

let digitalParser = (input: string) => {
  let c = input |> Js.String.charAt(0);
  if ("0123456789" |> Js.String.includes(c)) {
    Ok(c, input |> Js.String.slice(~from=1, ~to_=Js.String.length(input)));
  } else {
    Error("not digital");
  };
};

let operatorParser = (input: string) => {
  let c = input |> Js.String.charAt(0);
  if (c == "+" || c == "-") {
    Ok(c, input |> Js.String.slice(~from=1, ~to_=Js.String.length(input)));
  } else {
    Error("not known operator");
  };
};

let validInputParser = (input: string) => {
  switch (input) {
  | "" => Error("empty string")
  | _ => Ok(input, "")
  };
};

let (@>) = (p1: parser, p2: parser, input: string) => {
  switch (p1(input)) {
  | Ok(current, _) => p2(current)
  | Error(e) => Error(e)
  };
};

let (@|) = (p1: parser, p2: parser, input: string) => {
  switch (p1(input)) {
  | Ok(current, _) => p2(current)
  | Error(_) => Ok(input, "")
  };
};

let rec many = (p: parser, input: string) => {
  switch (input |> validInputParser @> p) {
  | Ok(current, remaining) =>
    switch (many(p, remaining)) {
    | Ok(nextCurrent, nextRemaining) =>
      Ok(current ++ nextCurrent, nextRemaining)
    | Error(_) => Ok(current, remaining)
    }
  | Error(e) => Error(e)
  };
};

let rec parser = (~input: string, ~expr: option(expression)=?, ()) => {
  input |> Js.log;
  switch (input |> many(digitalParser)) {
  | Ok(current, remaining) =>
    switch (remaining |> validInputParser @| operatorParser) {
    | Ok(current1, remaining1) =>
      (
        switch (expr) {
        | Some(expr_) => (expr_, remaining1)
        | None =>
          switch (remaining1 |> many(digitalParser)) {
          | Ok(current_, remaining_) => (Digital(current_), remaining_)
          | Error(_) => Js.Exn.raiseError("next expr error")
          }
        }
      )
      |> (
        ((expr_, remaining_)) => {
          switch (current1) {
          | "+" =>
            parser(
              ~input=remaining_,
              ~expr=Add(expr_, Digital(current)),
              (),
            )
          | "-" =>
            parser(
              ~input=remaining_,
              ~expr=Minus(expr_, Digital(current)),
              (),
            )
          | "" => expr_
          | _ => Js.Exn.raiseError("parse error")
          };
        }
      )

    | Error(e) => Js.Exn.raiseError(e)
    }
  | Error(e) => Js.Exn.raiseError(e)
  };
};

parser(~input="1+2-3+4", ()) |> print |> Js.log;
/* "0123456789" |> Js.String.includes("") |> Js.log; */