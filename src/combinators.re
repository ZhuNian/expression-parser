open Utils;

let run = (p, input) => {
  let Parser(fn) = p;
  fn(input);
};

let bind = (p, f) => {
  let innerFn = input => {
    switch (run(p, input)) {
    | Ok((res, remain)) => remain |> run(f(res))
    | Error(e) => Error(e)
    };
  };
  Parser(innerFn);
};
let (==>) = bind;

let andThen = (p1, p2) => {
  let innerFn = input =>
    run(p1, input)
    --> (
      ((current1, remain1)) => {
        run(p2, remain1)
        --> (((current2, remain2)) => Ok(((current1, current2), remain2)));
      }
    );
  Parser(innerFn);
};
let (&>) = andThen;

let orElse = (p1, p2) => {
  let innerFn = input => {
    switch (run(p1, input)) {
    | Ok(result) => Ok(result)
    | Error(_) => run(p2, input)
    };
  };
  Parser(innerFn);
};
let (&|) = orElse;

let map = (p, mapFunc) => {
  let innerFn = input => {
    switch (run(p, input)) {
    | Ok((result, remain)) => Ok((mapFunc(result), remain))
    | Error(e) => Error(e)
    };
  };
  Parser(innerFn);
};
let (&=) = map;

let returnP = res => {
  let innerFn = input => Ok((res, input));
  Parser(innerFn);
};

let applyP = (p1: parser('a => 'b), p2: parser('a)): parser('b) =>
  p1 ==> map(p2);
let (<*>) = applyP;

let lift2 = (fn, p1, p2) => fn |> returnP <*> p1 <*> p2;

let anyOf = (l: array(parser('a))) => {
  l
  |> Js.Array.slice(~start=1, ~end_=Js.Array.length(l))
  |> Js.Array.reduce((&|), l[0]);
};