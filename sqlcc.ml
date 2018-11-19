open Codelib

type table = string
type schema = table list

(* -------------------------------------------------------------------------- *)

type ref
  = Field of string
  | Int of int
  | String of string

type pred
  = Eq  of ref * ref
  | Neq of ref * ref

type op
  = Scan of table * schema
  | Project of schema * op
  | Filter of pred * op
  | Print of op

type fields = (string code) list
type record = { fields: fields; schema: schema }

(* -------------------------------------------------------------------------- *)

let lookup_record (r : record) (key : string) : string code =
  let rec go = function
    | x::_, y::_ when x = key -> y
    | _::xs, _::ys -> go (xs, ys)
    | _ -> raise @@ Failure ("lookup_record: failed search for " ^ key)
  in match r with
  | { fields; schema; } -> go (schema, fields)

let project_fields (r: record) (s : schema) : fields
  = List.map (lookup_record r) s

let eval_ref (ref' : ref) (rec' : record) : string code
  = match ref' with
  | Field r -> lookup_record rec' r
  | Int i -> .< string_of_int i >.
  | String s -> .< s >.

let eval_pred (p : pred) (r : record) : bool code =
  match p with
  | Eq (x, y) -> .< .~(eval_ref x r) = .~(eval_ref y r) >.
  | Neq (x, y) -> .< .~(eval_ref x r) <> .~(eval_ref y r) >.

let process_csv row schema (push : record -> unit code) : unit code =
  let body s =
    let push x = push { schema = schema; fields = x } in
    push @@ List.map (fun col -> .<Row.fetch col .~s>.) schema
  in body row

let rec eval_op row (o : op) (push : record -> unit code) : unit code
  = match o with
  | Scan (_, schema) -> process_csv row schema push

  | Project (out, parent) ->
    eval_op row parent @@ fun r ->
        push { fields = project_fields r out ; schema = r.schema }

  | Filter (pred, parent) ->
    eval_op row parent @@ fun r ->
        .< if .~(eval_pred pred r) then .~(push r) >.

  | Print parent ->
    eval_op row parent @@ fun r ->
        let rec go = function
            | [x]   -> .< Rt.emit .~x >.
            | x::xs -> .< Rt.emit .~x ; .~(go xs) >.
            | _     -> .< Rt.void >.
        in go r.fields

(* -------------------------------------------------------------------------- *)

let eval_query row (o : op) : unit code = eval_op row o (fun _ -> .<()>.)

let snippet o : (Row.t Stdlib.ref -> unit) code = .<fun row -> .~(eval_query .<row>. (Print o))>.

let schematics = [ "room" ; "title" ; "speaker" ; "time" ; "description" ]

let example_0 =
  Scan ("talks.csv", schematics)

let example_1 =
  Project ([ "speaker" ],
    Scan ("talks.csv", schematics))

let example_2 =
  Project ([ "title" ; "room" ; "speaker" ],
    Filter(Eq(Field "time", String "09:00 AM"),
      Scan ("talks.csv", schematics)))

(* -------------------------------------------------------------------------- *)

let _ =
  (* this lets us dump the ocaml ast *)
  let dump v = Codelib.print_code Format.std_formatter v in

  (*
  (* this lets us dump an offshore ast *)
  let module M = struct
        include OffshoringIR.DefaultConv

        let id_conv path name = match (path,(name : OffshoringIR.varname :> string)) with
          | ("Csv","open_")    -> "csv_open"
          | ("Csv","has_next") -> "csv_has_next"
          | ("Csv","next")     -> "csv_next"
          | ("Csv","close")    -> "csv_close"

          | ("Stdlib","=") -> "strcmp"

          | ("Rt", "void") -> "no_op"
          | ("Rt", "say") -> "printf"

          | _ -> id_conv path name

        let type_conv path typ = match path with
          | "Csv.scanner" -> OffshoringIR.TUnit
          | _ -> type_conv path typ
      end in

  let offshore v = OffshoringIR.offshore (module M) v in
  *)

  (* go go gadget *)
  let bloviate query = dump (snippet query)

  in try
    bloviate @@ match Sys.argv.(1) with
      | "2" -> example_2
      | "1" -> example_1
      | _   -> example_0
  with
    Failure e -> print_string e; raise (Failure "catastrophic")
