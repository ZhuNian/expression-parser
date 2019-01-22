type result('success, 'error) =
  | Ok(('success, string))
  | Error('error);

type parser('success) =
  | Parser(string => result('success, string));

let bind_ = (result, f) => {
  switch (result) {
  | Ok(res) => f(res)
  | Error(e) => Error(e)
  };
};

let (-->) = bind_;

let or_ = (result, f) => {
  switch (result) {
  | Ok(result_) => Ok(result_)
  | Error(e) => f(e)
  };
};

let (&!=) = or_;