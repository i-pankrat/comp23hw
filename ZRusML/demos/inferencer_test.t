% arithmetic operations test
  $ ./inferencer_test.exe << EOF
  > let x = 3 * 4 / 2 + 7;;
  > let y = 15 + 7 / 2;;
  > let result = x + y * x;;
  val x : int
  val y : int
  val result : int
  _______

% let rec test
  $ ./inferencer_test.exe << EOF
  > let rec timer = fun y -> if y = 0 then 0 else timer (y - 1);;
  > let tmp = timer 17;;
  val timer : int -> int
  val tmp : int
  _______


% binary operator inference test
  $ ./inferencer_test.exe << EOF
  > let e = fun x -> (if x >= 2 then x = 15 else false);;
  val e : int -> bool
  _______

% typs test
  $ ./inferencer_test.exe << EOF
  > let int = 3;;
  > let bool = true;;
  val int : int
  val bool : bool
  _______

% factorial test
  $ ./inferencer_test.exe << EOF
  > let rec fact n = if n = 0 then 1 else n * (fact (n - 1));;
  val fact : int -> int
  _______

% factorial acc test
  $ ./inferencer_test.exe << EOF
  > let rec acc_fact acc n = if n = 0 then acc else acc_fact (acc * n) (n - 1);;
  > let test = acc_fact 1 3 = 6;;
  val acc_fact : int -> int -> int
  val test : bool
  _______
