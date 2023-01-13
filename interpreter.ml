open Str;;
(*#load "str.cma";; *)

(* Closure Type
(fname, arg, commands list, mutuals, defined previously)
Fun sets fname = Clo (fname, arg, commands list)
toString prints out Clo (fname, arg)   
*)

type const = 
  Int of int | String of string | Name of string | Left of string | Right of string | Tuple of const list | Closure of (string * string * string list * string list * string list)

(*type inp = const list list * string * bool * 
[stack, file_path, true, globalEnv, localEnv, toRun, ifThen, inFun, returned)
  [[]], output_file_path, false, [], [[]], true, []*)

let validateString (str: string) = 
  if Str.string_match (Str.regexp "\"[a-zA-Z]*\"") str 0 then true else false

let validateName (str: string) = 
  if Str.string_match (Str.regexp "[a-z][a-zA-Z_0-9]*") str 0 then true else false



let getStack stack = 
  List.nth stack 0

let getLocal localEnv: (string * const) list = 
  List.nth localEnv 0

let putStack stack newStack = 
  match stack with 
  | hd::tl -> newStack::tl
  | _ -> assert false

let putLocal local newLocal = 
  match local with
  | hd::tl -> newLocal::tl
  | _ -> assert false
  
let closureString (s: const) = 
  match s with
  | Closure (name, arg, cmds, mutuals, prevClosures) -> "Clo (" ^ name ^ ", " ^ arg ^ ")"
  | _ -> assert false

let rec toString (s: const) = 
  match s with
  | Int i -> string_of_int i 
  | String str -> str
  | Name name -> name
  | Left l -> l
  | Right r -> r
  | Tuple lst -> tupleString lst
  | Closure c -> closureString s

and tupleString (lst: const list) =
  let rec goThroughList (lst: const list) =
    match lst with 
    | hd::tl -> toString hd ^ (match tl with 
      | hd1::tl1 ->  ", " ^ goThroughList tl
      | [] -> ")")
    | [] -> ")" in
  "(" ^ (goThroughList lst)



  (*c must be an int*)
let toInt(c: const) = 
  match c with
  | Int i -> i 
  | _ -> assert false (*if c is not an int*)

(*env commands*)

(*make sure name is a valid name*)
let rec addVar env name value = 
  match env with 
  | [] -> [(name, value)]
  | hd::tl -> if fst hd = name then (name, value)::tl else hd::addVar tl name value

let rec getVar env name: const option = 
  match env with 
  | [] -> None
  | hd::tl -> if fst hd = name then Some(snd hd) else getVar tl name

let rec getLocalVar local name: const option = 
  match local with
  | [] -> None
  | hd::tl -> match (getVar hd name) with
   | None -> getLocalVar tl name
   | Some s -> Some s



(*let rec updateFun localEnv funName cmd = 
  match localEnv with
  | hd::tl -> match (updateFunOneEnv hd funName cmd) with
    | None -> updateFun tl funName cmd
    | Some s -> s 
  | [] -> assert false (* function should exists somewhere*)*)


let toStringList (lst: const list) = 
  List.fold_right (fun x acc -> toString x :: acc) lst []

(* Citing Source: This code was found here: https://stackoverflow.com/questions/43554262/how-to-validate-if-a-string-only-contains-number-chars-in-ocaml  *)
let validateInt (str: string) = 
  try int_of_string str |> ignore; true
  with Failure _ -> false 

let validateLeft (str: string) = 
  if String.length str < 4 then false else
  match (String.sub str 0 4) with
  | "Left" -> true
  | _ -> false

let validateRight (str: string) = 
  if String.length str < 5 then false else
  match (String.sub str 0 5) with
  | "Right" -> true
  | _ -> false

let isBool (s: const) = 
  match s with
  | Int i -> if i==0 || i==1 then true else false
  | _ -> false
  
let quit stack file_path globalEnv localEnv toRun ifThen inFun returned =
  let fp = open_out file_path in
  let () = Printf.fprintf fp "%s" ((String.concat (String.make 1 '\n') (toStringList (getStack stack))) ^ "\n") in
    close_out fp ; (stack, file_path, true, globalEnv, localEnv, toRun, ifThen, inFun, returned)

let error stack file_path globalEnv localEnv toRun ifThen inFun returned = 
  let fp = open_out file_path in
  let () = print_string "error" in
  let () = Printf.fprintf fp "\"Error\"" in
    close_out fp ; (stack, file_path, true, globalEnv, localEnv, toRun, ifThen, inFun, returned)

let makeFile file_path =
  let fp = open_out file_path in
    close_out fp ; ()

let push arg stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned = 
  match validateString arg with
  | true -> (putStack stack (String(arg) :: (getStack stack)), file_path, doneRunning, globalEnv, localEnv, toRun, ifThen, inFun, returned)
  | false -> match validateInt arg with
    | true -> (putStack stack (Int(int_of_string arg) :: (getStack stack)), file_path, doneRunning, globalEnv, localEnv, toRun, ifThen, inFun, returned)
    | false -> match validateLeft arg with
      | true -> (putStack stack (Left(arg) :: (getStack stack)), file_path, doneRunning, globalEnv, localEnv, toRun, ifThen, inFun, returned)
      | false -> match validateRight arg with
        | true -> (putStack stack (Right(arg) :: (getStack stack)), file_path, doneRunning, globalEnv, localEnv, toRun, ifThen, inFun, returned)
        | false -> match validateName arg with
          | true -> (match (getLocalVar localEnv arg) with 
            | Some(value) -> (putStack stack (value :: (getStack stack)), file_path, doneRunning, globalEnv, localEnv, toRun, ifThen, inFun, returned)
            | None -> (match (getVar globalEnv arg) with 
              | Some (value) -> (putStack stack (value :: (getStack stack)), file_path, doneRunning, globalEnv, localEnv, toRun, ifThen, inFun, returned)
              | None -> error stack file_path globalEnv localEnv toRun ifThen inFun returned))
          | false -> error stack file_path globalEnv localEnv toRun ifThen inFun returned

let pushConst tuple (stack: const list list) file_path doneRunning globalEnv localEnv toRun ifThen inFun returned = 
  (putStack stack (tuple :: (getStack stack)), file_path, doneRunning, globalEnv, localEnv, toRun, ifThen, inFun, returned)

let pop stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned =
  match (getStack stack) with 
  | [] -> error stack file_path globalEnv localEnv toRun ifThen inFun returned
  | hd::tl -> (putStack stack tl, file_path, doneRunning, globalEnv, localEnv, toRun, ifThen, inFun, returned)

let andFun stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned = 
  if List.length (getStack stack) < 2 then error stack file_path globalEnv localEnv toRun ifThen inFun returned else
    let num1 = List.nth (getStack stack) 0 and (stack, file_path, doneRunning, globalEnv, localEnv, toRun, ifThen, inFun, returned) = pop stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned in
    let num2 = List.nth (getStack stack) 0 and (stack, file_path, doneRunning, globalEnv, localEnv, toRun, ifThen, inFun, returned) = pop stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned in
    match (isBool num1, isBool num2) with
      | (true, true) -> push (string_of_int (toInt num1 * toInt num2)) stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned
      | _ -> error stack file_path globalEnv localEnv toRun ifThen inFun returned

let orFun stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned = 
  if List.length (getStack stack) < 2 then error stack file_path globalEnv localEnv toRun ifThen inFun returned else
    let num1 = List.nth (getStack stack) 0 and (stack, file_path, doneRunning, globalEnv, localEnv, toRun, ifThen, inFun, returned) = pop stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned in
    let num2 = List.nth (getStack stack) 0 and (stack, file_path, doneRunning, globalEnv, localEnv, toRun, ifThen, inFun, returned) = pop stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned in
    match (isBool num1, isBool num2) with
      | (true, true) -> (match (toInt num1, toInt num2) with
        | (0,0) -> push ("0") stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned
        | _ -> push ("1") stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned)
      | _ -> error stack file_path globalEnv localEnv toRun ifThen inFun returned

let notFun stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned = 
  if List.length (getStack stack) < 1 then error stack file_path globalEnv localEnv toRun ifThen inFun returned else
    let num = List.nth (getStack stack) 0 and (stack, file_path, doneRunning, globalEnv, localEnv, toRun, ifThen, inFun, returned) = pop stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned in
    if isBool num then 
      match toInt num with
        | 1 -> push ("0") stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned
        | 0 -> push ("1") stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned
        | _ -> assert false (*theoretically impossible to reach here but just to supress warning*)
    else error stack file_path globalEnv localEnv toRun ifThen inFun returned

let equal stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned = 
  if List.length (getStack stack) < 2 then error stack file_path globalEnv localEnv toRun ifThen inFun returned else
    let num1 = List.nth (getStack stack) 0 and (stack, file_path, doneRunning, globalEnv, localEnv, toRun, ifThen, inFun, returned) = pop stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned in
    let num2 = List.nth (getStack stack) 0 and (stack, file_path, doneRunning, globalEnv, localEnv, toRun, ifThen, inFun, returned) = pop stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned in
    match (num1, num2) with
     | (Int n1, Int n2) -> if n1==n2 then push ("1") stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned
      else push ("0") stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned
     | _ -> error stack file_path globalEnv localEnv toRun ifThen inFun returned

let lte stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned = 
  if List.length (getStack stack) < 2 then error stack file_path globalEnv localEnv toRun ifThen inFun returned else
    let num1 = List.nth (getStack stack) 0 and (stack, file_path, doneRunning, globalEnv, localEnv, toRun, ifThen, inFun, returned) = pop stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned in
    let num2 = List.nth (getStack stack) 0 and (stack, file_path, doneRunning, globalEnv, localEnv, toRun, ifThen, inFun, returned) = pop stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned in
    match (num1, num2) with
     | (Int n1, Int n2) -> if n1<=n2 then push ("1") stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned
      else push ("0") stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned
     | _ -> error stack file_path globalEnv localEnv toRun ifThen inFun returned

let add stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned = 
  if List.length (getStack stack) < 2 then error stack file_path globalEnv localEnv toRun ifThen inFun returned else
    let num1 = List.nth (getStack stack) 0 and (stack, file_path, doneRunning, globalEnv, localEnv, toRun, ifThen, inFun, returned) = pop stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned in
    let num2 = List.nth (getStack stack) 0 and (stack, file_path, doneRunning, globalEnv, localEnv, toRun, ifThen, inFun, returned) = pop stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned in
    match (num1, num2) with
     | (Int n1, Int n2) -> push (string_of_int (n1 + n2)) stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned
     | _ -> error stack file_path globalEnv localEnv toRun ifThen inFun returned

let sub stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned = 
  if List.length (getStack stack) < 2 then error stack file_path globalEnv localEnv toRun ifThen inFun returned else
    let num1 = List.nth (getStack stack) 0 and (stack, file_path, doneRunning, globalEnv, localEnv, toRun, ifThen, inFun, returned) = pop stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned in
    let num2 = List.nth (getStack stack) 0 and (stack, file_path, doneRunning, globalEnv, localEnv, toRun, ifThen, inFun, returned) = pop stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned in
    match (num1, num2) with
     | (Int n1, Int n2) -> push (string_of_int (n1 - n2)) stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned
     | _ -> error stack file_path globalEnv localEnv toRun ifThen inFun returned

let mul stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned = 
  if List.length (getStack stack) < 2 then error stack file_path globalEnv localEnv toRun ifThen inFun returned else
    let num1 = List.nth (getStack stack) 0 and (stack, file_path, doneRunning, globalEnv, localEnv, toRun, ifThen, inFun, returned) = pop stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned in 
    let num2 = List.nth (getStack stack) 0 and (stack, file_path, doneRunning, globalEnv, localEnv, toRun, ifThen, inFun, returned) = pop stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned in
    match (num1, num2) with
     | (Int n1, Int n2) -> push (string_of_int (n1 * n2)) stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned
     | _ -> error stack file_path globalEnv localEnv toRun ifThen inFun returned

let div stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned = 
  if List.length (getStack stack) < 2 then error stack file_path globalEnv localEnv toRun ifThen inFun returned else
    let num1 = List.nth (getStack stack) 0 and (stack, file_path, doneRunning, globalEnv, localEnv, toRun, ifThen, inFun, returned) = pop stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned in
    let num2 = List.nth (getStack stack) 0 and (stack, file_path, doneRunning, globalEnv, localEnv, toRun, ifThen, inFun, returned) = pop stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned in
    match (num1, num2) with
     | (_, Int 0) -> error stack file_path globalEnv localEnv toRun ifThen inFun returned
     | (Int n1, Int n2) -> push (string_of_int (n1 / n2)) stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned
     | _ -> error stack file_path globalEnv localEnv toRun ifThen inFun returned

let swap stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned = 
  if List.length (getStack stack) < 2 then error stack file_path globalEnv localEnv toRun ifThen inFun returned else
    let val1 = List.nth (getStack stack) 0 and (stack, file_path, doneRunning, globalEnv, localEnv, toRun, ifThen, inFun, returned) = pop stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned in
    let val2 = List.nth (getStack stack) 0 and (stack, file_path, doneRunning, globalEnv, localEnv, toRun, ifThen, inFun, returned) = pop stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned in  
    let (stack, file_path, doneRunning, globalEnv, localEnv, toRun, ifThen, inFun, returned) = (match val1 with
      | Tuple t -> pushConst val1 stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned
      | _ ->  push (toString val1) stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned) in
    match val2 with
      | Tuple t -> pushConst val2 stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned
      | _ -> push (toString val2) stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned

let neg stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned = 
  if List.length (getStack stack) < 1 then error stack file_path globalEnv localEnv toRun ifThen inFun returned else
    let val1 = List.nth (getStack stack) 0 and (stack, file_path, doneRunning, globalEnv, localEnv, toRun, ifThen, inFun, returned) = pop stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned in
    match val1 with 
    | Int num -> push (string_of_int (-1 * num)) stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned
    | _ -> error stack file_path globalEnv localEnv toRun ifThen inFun returned

let concat stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned = 
  if List.length (getStack stack) < 2 then error stack file_path globalEnv localEnv toRun ifThen inFun returned else
    let str1 = List.nth (getStack stack) 0 and (stack, file_path, doneRunning, globalEnv, localEnv, toRun, ifThen, inFun, returned) = pop stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned in
    let str2 = List.nth (getStack stack) 0 and (stack, file_path, doneRunning, globalEnv, localEnv, toRun, ifThen, inFun, returned) = pop stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned in
    match (str1, str2) with
     | (String n1, String n2) -> push ((String.sub n1 0 (String.length n1 - 1))  ^ (String.sub n2 1 (String.length n2 - 1))) stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned
     | _ -> error stack file_path globalEnv localEnv toRun ifThen inFun returned



let global arg stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned =
  if List.length (getStack stack) < 1 then error stack file_path globalEnv localEnv toRun ifThen inFun returned else
    let value = List.nth (getStack stack) 0 and (stack, file_path, doneRunning, globalEnv, localEnv, toRun, ifThen, inFun, returned) = pop stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned in
    let globalEnv = addVar globalEnv arg value  in
    (stack, file_path, doneRunning, globalEnv, localEnv, toRun, ifThen, inFun, returned)

let local arg stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned =
  if List.length (getStack stack) < 1 then error stack file_path globalEnv localEnv toRun ifThen inFun returned else
    let value = List.nth (getStack stack) 0 and (stack, file_path, doneRunning, globalEnv, localEnv, toRun, ifThen, inFun, returned) = pop stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned in
    let localEnv = putLocal localEnv (addVar (getLocal localEnv) arg value) in
    (stack, file_path, doneRunning, globalEnv, localEnv, toRun, ifThen, inFun, returned)

let beginFun stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned =
  if toRun then 
  ([]::stack, file_path, doneRunning, globalEnv, []::localEnv, toRun, "begin"::ifThen, inFun, returned)
  else (stack, file_path, doneRunning, globalEnv, localEnv, toRun, "begin"::ifThen, inFun, returned)

let endFun stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned =
  let endType = List.nth ifThen 0 and ifThen = (match ifThen with
    | hd::tl -> tl
    | [] -> assert false) in
  if endType="if" then (stack, file_path, doneRunning, globalEnv, localEnv, true, ifThen, inFun, returned) else
  if (not toRun) then (stack,file_path, doneRunning, globalEnv, localEnv, toRun, ifThen, inFun, returned) else
  if (List.length (getStack stack) = 0) then error stack file_path globalEnv localEnv toRun ifThen inFun returned else
  let stack = (
  match stack with
   | fst::snd::tl -> ((List.nth fst 0)::snd)::tl
   | _ -> assert false) (*if we're running end for a begin, there should be at least two stacks, otherwise its for an ifthen*) in
    let localEnv = (
      match localEnv with
      | hd::tl -> tl
      | _ -> assert false (*again, if we're running end, there should be at least two local environments*)
    ) in
  (stack, file_path, doneRunning, globalEnv, localEnv, true, ifThen, inFun, returned)

let ifFun stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned = 
  if (List.length (getStack stack) = 0) then error stack file_path globalEnv localEnv toRun ifThen inFun returned else
    if toRun = false then (stack, file_path, doneRunning, globalEnv, localEnv, toRun, "ignored if"::ifThen, inFun, returned) else
    let value = List.nth (getStack stack) 0 and (stack, file_path, doneRunning, globalEnv, localEnv, toRun, ifThen, inFun, returned) = pop stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned in
    if (isBool value = false) then error stack file_path globalEnv localEnv toRun ifThen inFun returned else
      match value with
      | Int 0 -> (stack, file_path, doneRunning, globalEnv, localEnv, false, "if"::ifThen, inFun, returned)
      | Int 1 -> (stack, file_path, doneRunning, globalEnv, localEnv, true, "if"::ifThen, inFun, returned)
      | _ -> assert false

let elseFun stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned = 
  match List.hd ifThen with
   | "ignored if" -> (stack, file_path, doneRunning, globalEnv, localEnv, toRun, ifThen, inFun, returned)
   | _ -> (stack, file_path, doneRunning, globalEnv, localEnv, (not toRun), ifThen, inFun, returned)

let getUnionContents (c: const) =
  match c with
  | Left l -> String.sub l 5 (String.length l - 5)
  | Right r -> String.sub r 6 (String.length r - 6)
  | _ -> toString c 


let caseLeft stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned = 
  if (List.length (getStack stack) = 0) then error stack file_path globalEnv localEnv toRun ifThen inFun returned else
    if toRun = false then (stack, file_path, doneRunning, globalEnv, localEnv, toRun, "ignored if"::ifThen, inFun, returned) else
    let value = List.nth (getStack stack) 0 and (stack, file_path, doneRunning, globalEnv, localEnv, toRun, ifThen, inFun, returned) = pop stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned in
      let (stack, file_path, doneRunning, globalEnv, localEnv, toRun, ifThen, inFun, returned) = push (getUnionContents value) stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned in
      match value with
      | Left l -> (stack, file_path, doneRunning, globalEnv, localEnv, true, "if"::ifThen, inFun, returned)
      | Right r -> (stack, file_path, doneRunning, globalEnv, localEnv, false, "if"::ifThen, inFun, returned)
      | _ -> error stack file_path globalEnv localEnv toRun ifThen inFun returned 

let right stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned = 
  match List.hd ifThen with
    | "ignored if" -> (stack, file_path, doneRunning, globalEnv, localEnv, toRun, "if"::ifThen, inFun, returned)
    | _ -> (stack, file_path, doneRunning, globalEnv, localEnv, (not toRun), "if"::ifThen, inFun, returned)

let injL stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned = 
  if List.length (getStack stack) < 1 then error stack file_path globalEnv localEnv toRun ifThen inFun returned else
    let val1 = List.nth (getStack stack) 0 and (stack, file_path, doneRunning, globalEnv, localEnv, toRun, ifThen, inFun, returned) = pop stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned in
    push ("Left " ^ (toString val1)) stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned
     
let injR stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned = 
  if List.length (getStack stack) < 1 then error stack file_path globalEnv localEnv toRun ifThen inFun returned else
    let val1 = List.nth (getStack stack) 0 and (stack, file_path, doneRunning, globalEnv, localEnv, toRun, ifThen, inFun, returned) = pop stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned in
    push ("Right " ^ (toString val1)) stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned

let makeTuple arg stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned = 
  if List.length (getStack stack) < arg then error stack file_path globalEnv localEnv toRun ifThen inFun returned else
  if arg < 0 then error stack file_path globalEnv localEnv toRun ifThen inFun returned else
  let rec getConsts num =
    match num with
    | 0 -> []
    | _ -> (List.nth (getStack stack) (num-1)) :: getConsts (num-1) in
  let rec removeVals num stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned = 
    match num with
    | 0 -> stack
    | _ -> (let (stack, file_path, doneRunning, globalEnv, localEnv, toRun, ifThen, inFun, returned) = pop stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned
    in (removeVals (num-1) stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned)) in
  let stack = removeVals arg stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned in
  pushConst (Tuple(getConsts arg)) stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned

let get idx stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned = 
  if List.length (getStack stack) = 0 then error stack file_path globalEnv localEnv toRun ifThen inFun returned else
  let val1 = List.nth (getStack stack) 0 in
  match val1 with 
   | Tuple t -> (match (List.nth t idx) with 
    | Tuple u -> pushConst (List.nth t idx) stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned
    | _ ->  push (toString (List.nth t idx)) stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned)
   | _ -> error stack file_path globalEnv localEnv toRun ifThen inFun returned

let rec updateFun env funName cmd: (string * const) list = (* make sure to plug in getLocal localEnv for env*)
match env with
| hd::tl -> if fst hd = funName then let Closure(name, arg, commands, mutuals, prevClosures) = (snd hd) in (funName, Closure(name, arg, commands @ [cmd], mutuals, prevClosures))::tl
              else hd::(updateFun tl funName cmd)
| [] -> assert false (*funName should be present*)

let rec getClosures localEnv =
  match localEnv with
  | hd::tl -> (match (snd hd) with
    | Closure (name, _, _, _, _) -> name::(getClosures tl)
    | _ -> getClosures tl)
  | [] -> []

let rec removeClosures localEnv = 
  match localEnv with
  | hd::tl -> (match (snd hd) with 
    | Closure s -> removeClosures tl
    | _ -> hd:: removeClosures tl)
  | [] -> []

let newFun name arg stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned =
    let localEnv = putLocal localEnv (addVar (getLocal localEnv) name (Closure(name, arg, [], [], name::(getClosures (getLocal localEnv))))) in
    (stack, file_path, doneRunning, globalEnv, localEnv, toRun, "fun"::ifThen, [name]::inFun, returned)

let rec addClosures names newLocal oldLocal = 
  let () = List.iter print_string names in
  match oldLocal with
   | hd::tl -> if List.mem (fst hd) names then addClosures names (putLocal newLocal (addVar (getLocal newLocal) (fst hd) (snd hd))) tl else newLocal
   | [] -> newLocal

let rec getMutuals env funName =
  match env with
  | hd::tl -> if fst hd = funName then let Closure(name, arg, commands, mutuals, prevClosures) = (snd hd) in mutuals
                else getMutuals tl funName
  | [] -> assert false (*funName should be present*)

let rec setMutuals mutuals env =
  match env with
  | hd::tl -> if List.mem (fst hd) mutuals then let Closure(name, arg, commands, _, prevClosures) = (snd hd) in (name, Closure(name, arg, commands, mutuals, prevClosures))::tl
  else hd::(setMutuals mutuals tl)
  | [] -> []

  let getFunName inFun = 
    List.hd (List.hd inFun)  

let mutFun name arg stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned =
  if List.length inFun = 0 then error stack file_path globalEnv localEnv toRun ifThen inFun returned else
  let originalFun = getFunName inFun in
  let mutuals = name::(getMutuals (getLocal localEnv) originalFun) in
  let localEnv = putLocal localEnv (addVar (getLocal localEnv) name (Closure(name, arg, [], mutuals, name::(getClosures (getLocal localEnv))))) in
  let localEnv = putLocal localEnv (setMutuals mutuals (getLocal localEnv)) in
  (stack, file_path, doneRunning, globalEnv, localEnv, toRun, ifThen, (name::(List.hd inFun))::(List.tl inFun), returned)

let ret stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned = 
      if List.length (getStack stack) = 0  then error stack file_path globalEnv localEnv toRun ifThen inFun returned else
      (stack, file_path, doneRunning, globalEnv, localEnv, toRun, ifThen, inFun, "returned")
    

let rec call stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned = 
  if List.length (getStack stack) < 2 then error stack file_path globalEnv localEnv toRun ifThen inFun returned else
    
    let clo = List.nth (getStack stack) 0 and (stack, file_path, doneRunning, globalEnv, localEnv, toRun, ifThen, inFun, returned) = pop stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned in
    let arg = List.nth (getStack stack) 0 and (stack, file_path, doneRunning, globalEnv, localEnv, toRun, ifThen, inFun, returned) = pop stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned in
    match clo with
     | Closure (name, argName, commands, mutuals, prevClosures) ->
       
       let newLocal = putLocal localEnv (removeClosures (getLocal localEnv)) in
       let () = print_string("removed") in
       let newLocal = addClosures mutuals newLocal (getLocal localEnv) in
       let () = print_string("added mutuals") in
       let () = print_string (string_of_int (List.length prevClosures)) in
       
       let newLocal = addClosures prevClosures newLocal (getLocal localEnv) in
       let newLocal = putLocal localEnv (addVar (getLocal localEnv) argName arg) in
       let () = print_string ("added prevClosures") in
    let (newStack, file_path, doneRunning, globalEnv, newLocal, toRun, ifThen, inFun, returned) = (List.fold_left parseCommandCall ([[]], file_path, doneRunning, globalEnv, newLocal, toRun, ifThen, inFun, "fun") commands) in
      if returned <> "returned" then error stack file_path globalEnv localEnv toRun ifThen inFun returned else
      let returnedVal = (List.hd (List.hd newStack)) in
        (putStack stack (returnedVal :: (getStack stack)), file_path, doneRunning, globalEnv, localEnv, toRun, ifThen, inFun, "")


and parseCommandCall (stack, file_path, doneRunning, globalEnv, localEnv, toRun, ifThen, inFun, (returned:string)) command = 
  let () = print_string ((String.concat (String.make 1 '\n') (toStringList (getStack stack))) ^ "\n") in
  let () = print_string (String.concat (String.make 1 '\n') (ifThen) ^ "\n") in
  (*let () = print_string((string_of_bool toRun) ^ "\n") in *)
  let () = print_string(command ^ "\n") in

  if returned = "returned" then 
    (stack, file_path, doneRunning, globalEnv, localEnv, toRun, ifThen, inFun, returned) else

  let lst = String.split_on_char ' ' command in
  let cmd = List.nth lst 0 in
  let arg = (if (List.length lst) > 1 then List.nth lst 1 else "") in
  let arg2 = (if (List.length lst) > 2 then List.nth lst 2 else "") in

  if List.length (List.hd inFun) != 0 && cmd <> "Mut" then 
      let inFun = (if (List.nth ifThen 0) = "fun" && (command = "End") then (List.tl inFun) else (inFun)) 
      and ifThen = (if command="End" then (List.tl ifThen) else ifThen) in
      if command <> "End" then
        let funName = getFunName inFun in let localEnv = putLocal localEnv (updateFun (getLocal localEnv) funName command) in
      let ifThen = 
        (match cmd with
      | "IfThen" -> "if"::ifThen
      | "Begin" -> "begin"::ifThen
      | "Fun" -> "otherFun"::ifThen
      | "CaseLeft" -> "if"::ifThen
      | _ -> ifThen) in
      (stack, file_path, doneRunning, globalEnv, localEnv, toRun, ifThen, inFun, returned)
      else
        (stack, file_path, doneRunning, globalEnv, localEnv, toRun, ifThen, inFun, returned) 
        else
  
  if doneRunning || (toRun = false && cmd <> "End" && cmd <> "IfThen" && cmd <> "Begin" && cmd <> "CaseLeft" && cmd <> "Right" && cmd <> "Else") then (stack, file_path, doneRunning, globalEnv, localEnv, toRun, ifThen, inFun, returned) else
  match cmd with 
  | "Quit" -> quit stack file_path globalEnv localEnv toRun ifThen inFun returned
  | "Push" -> push arg stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned
  | "Pop" -> pop stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned
  | "Add" -> add stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned
  | "Sub" -> sub stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned
  | "Mul" -> mul stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned
  | "Div" -> div stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned
  | "Swap" -> swap stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned
  | "Neg" -> neg stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned
  | "Concat" -> concat stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned
  | "And" -> andFun stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned
  | "Or" -> orFun stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned
  | "Not" -> notFun stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned
  | "Equal" -> equal stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned
  | "Lte" -> lte stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned
  | "Global" -> global arg stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned
  | "Local" -> local arg stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned
  | "Begin" -> beginFun stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned
  | "End" -> endFun stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned
  | "IfThen" -> ifFun stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned
  | "Else" -> elseFun stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned
  | "InjL" -> injL stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned
  | "InjR" -> injR stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned
  | "CaseLeft" -> caseLeft stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned
  | "Right" -> right stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned
  | "Tuple" -> makeTuple (int_of_string arg) stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned
  | "Get" -> get (int_of_string arg) stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned
  | "Fun" -> newFun arg arg2 stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned
  | "Mut" -> mutFun arg arg2 stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned
  | "Call" -> call stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned
  | "Return" -> ret stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned
  | _ -> error stack file_path globalEnv localEnv toRun ifThen inFun returned


(* make a new variable inFun that says if we are in function, equal to name of function, blank when none (can be a stack of names for nested)
   whenever inFun is true, no commands are run, all are deposited in commands list of function,
Mut just declares a new function, need to somehow associate them
Call just goes through commands and runs them sequentially thru parseCommand, initiates local environment with argument and mutually declared fun names
maybe each closure also has its own stack (const list, initially empty), and list of names of mutual functions
also need to know when we are in a current functino call for returning purposes *)

let parseCommand (stack, file_path, doneRunning, globalEnv, localEnv, toRun, ifThen, inFun, returned) command = 
  
  let () = print_string ((String.concat (String.make 1 '\n') (toStringList (getStack stack))) ^ "\n") in
  let () = print_string (String.concat (String.make 1 '\n') (ifThen) ^ "\n") in
  (*let () = print_string((string_of_bool toRun) ^ "\n") in *)
  let () = print_string(command ^ "\n") in

  let lst = String.split_on_char ' ' command in
  let cmd = List.nth lst 0 in
  let arg = (if (List.length lst) > 1 then List.nth lst 1 else "") in
  let arg2 = (if (List.length lst) > 2 then List.nth lst 2 else "") in

  if List.length (List.hd inFun) != 0 && cmd <> "Mut" then 
      let inFun = (if (List.nth ifThen 0) = "fun" && (command = "End") then (List.tl inFun) else (inFun)) 
      and ifThen = (if command="End" then (List.tl ifThen) else ifThen) in
      if command <> "End" then
        let funName = getFunName inFun in let localEnv = putLocal localEnv (updateFun (getLocal localEnv) funName command) in
      let ifThen = 
        (match cmd with
      | "IfThen" -> "if"::ifThen
      | "Begin" -> "begin"::ifThen
      | "Fun" -> "otherFun"::ifThen
      | "CaseLeft" -> "if"::ifThen
      | _ -> ifThen) in
      (stack, file_path, doneRunning, globalEnv, localEnv, toRun, ifThen, inFun, returned)
      else
        (stack, file_path, doneRunning, globalEnv, localEnv, toRun, ifThen, inFun, returned)

    
      else


  
  if doneRunning || (toRun = false && cmd <> "End" && cmd <> "IfThen" && cmd <> "Begin" && cmd <> "CaseLeft" && cmd <> "Right" && cmd <> "Else") then (stack, file_path, doneRunning, globalEnv, localEnv, toRun, ifThen, inFun, returned) else
  match cmd with 
  | "Quit" -> quit stack file_path globalEnv localEnv toRun ifThen inFun returned
  | "Push" -> push arg stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned
  | "Pop" -> pop stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned
  | "Add" -> add stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned
  | "Sub" -> sub stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned
  | "Mul" -> mul stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned
  | "Div" -> div stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned
  | "Swap" -> swap stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned
  | "Neg" -> neg stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned
  | "Concat" -> concat stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned
  | "And" -> andFun stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned
  | "Or" -> orFun stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned
  | "Not" -> notFun stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned
  | "Equal" -> equal stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned
  | "Lte" -> lte stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned
  | "Global" -> global arg stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned
  | "Local" -> local arg stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned
  | "Begin" -> beginFun stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned
  | "End" -> endFun stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned
  | "IfThen" -> ifFun stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned
  | "Else" -> elseFun stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned
  | "InjL" -> injL stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned
  | "InjR" -> injR stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned
  | "CaseLeft" -> caseLeft stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned
  | "Right" -> right stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned
  | "Tuple" -> makeTuple (int_of_string arg) stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned
  | "Get" -> get (int_of_string arg) stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned
  | "Fun" -> newFun arg arg2 stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned
  | "Mut" -> mutFun arg arg2 stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned
  | "Call" -> call stack file_path doneRunning globalEnv localEnv toRun ifThen inFun returned
  | _ -> error stack file_path globalEnv localEnv toRun ifThen inFun returned
  

let interpreter (src : string) (output_file_path: string): unit =
  let () = makeFile output_file_path in
  let commands = String.split_on_char '\n' src in
  ignore (List.fold_left parseCommand ([[]], output_file_path, false, [], [[]], true, [], [[]], "") commands) ; ()
