type 'a action =
  | Flag of ('a -> 'a)
  | Int of (int -> 'a -> 'a)
  | String of (string -> 'a -> 'a)
  | Help
  | Version

(** Option identifier *)
type ident = {
  long: string option;
  short: char option
}

type 'a arg = {
  id: ident;
  action: 'a action;
  summary: string
}

type 'a anonymous = {
  name: string;
  action: 'a action;
  multiple: bool;
  summary: string
}

type 'a arg_spec =
  | Arg of 'a arg
  | Anon of 'a anonymous

type 'a t = {
  name: string;
  version: string;
  authors: string list;
  args: ('a arg) list;
  anons: ('a anonymous) list;
  summary: string;
}

let id_to_string id () =
  match id.long, id.short with
  | Some l, Some s ->
    Format.sprintf "-%c, --%s" s l
  | None, Some s ->
    Format.sprintf "-%c" s
  | Some l, None ->
    Format.sprintf "--%s" l
  | _ -> raise (Invalid_argument "invalid id")

let print_id id out =
  Format.fprintf out "%s" (id_to_string id ())

let print_usage t out =
  let proc_name = Sys.argv.(0) in
  Format.fprintf out "Usage: %s" proc_name;
  if List.length t.args > 0 then
    Format.fprintf out " [OPTIONS]";
  let introduce_anon (a : 'a anonymous) =
    Format.fprintf out " <%s>" a.name;
    if a.multiple then
      Format.fprintf out "..."
  in
  List.iter introduce_anon t.anons;
  (* ... *)
  Format.fprintf out "\n%s" t.summary;
  Format.fprintf out "\n\n%s %s" t.name t.version;
  let print_author a =
    Format.fprintf out "\n%s" a
  in
  List.iter print_author t.authors;
  (* ... *)
  let print_preformatted l =
    let tab_width = 4 in
    let find_max_width m (label, _) =
      max m (String.length label)
    in
    let max_width = List.fold_left find_max_width 0 l in
    let print_item (label, summary) =
      let space_width = max_width - (String.length label) + tab_width in
      Format.fprintf out "\n%s%s%s%s" (String.make tab_width ' ') label (String.make space_width ' ') summary
    in
    List.iter print_item l
  in
  if List.length t.args > 0 then
    begin
      Format.fprintf out "\n\nOPTIONS:";
      let preformat_option o =
        let label = Format.sprintf "%t" (id_to_string o.id) in
        label, o.summary
      in
      let preformatted = List.map preformat_option t.args in
      print_preformatted preformatted
    end;
  if List.length t.anons > 0 then
    begin
      Format.fprintf out "\n\nARGS:";
      let preformat_arg (a : 'a anonymous) =
        a.name, a.summary
      in
      let preformatted = List.map preformat_arg t.anons in
      print_preformatted preformatted
    end;
  Format.fprintf out "\n"

let arg id action summary =
  Arg {
    id = id;
    action = action;
    summary = summary
  }

let anon name ?(multiple=false) action summary =
  Anon {
    name = name;
    action = action;
    multiple = multiple;
    summary = summary
  }

let long id =
  {
    long = Some id;
    short = None
  }

let short id =
  {
    long = None;
    short = Some id
  }

let id long short =
  {
    long = Some long;
    short = Some short
  }

let ident_repr id =
  match id.long, id.short with
  | Some id, _ -> id
  | None, Some id -> String.make 1 id
  | None, None -> failwith "invalid arg id"

let ident_leq a b =
  compare (ident_repr a) (ident_repr b) <= 0

let arg_spec_leq a b = ident_leq a.id b.id

let rec insert a = function
  | [] -> [a]
  | b::l -> if arg_spec_leq a b then a::b::l else b::(insert a l)

let (+>) t = function
  | Arg a ->
    {
      t with
      args = insert a t.args
    }
  | Anon a ->
    {
      t with
      anons = t.anons @ [a]
    }

let app name version authors summary =
  {
    name = name;
    version = version;
    authors = authors;
    args = [];
    anons = [];
    summary = summary
  }
  +> arg (id "help" 'h') Help "Prints this message."
  +> arg (id "version" 'V') Version "Prints version information."

let parse_arg str =
  let len = String.length str in
  if str.[0] == '-' && len > 1 then
    if str.[1] == '-' && len > 2 then
      Some [{
          long = Some (String.sub str 2 (len-2));
          short = None
        }]
    else
      begin
        let seq = String.to_seq (String.sub str 1 (len-1)) in
        let fold_id ids id =
          ids @ [{ long = None; short = Some id }]
        in
        let ids = Seq.fold_left fold_id [] seq in
        Some ids
      end
  else
    None

let match_id id a =
  match (a.id.long, a.id.short), (id.long, id.short) with
  | (Some a, _), (Some b, _) -> a = b
  | (_, Some a), (_, Some b) -> a = b
  | _ -> false

let rec find_arg_opt id = function
  | [] -> None
  | a::_ when match_id id a -> Some a
  | _::l -> find_arg_opt id l

let anonymous_arg_opt : ('a anonymous) list -> (('a anonymous) list * 'a anonymous) option = function
  | [] -> None
  | a::l -> Some (l, a)

let parse_int i =
  int_of_string Sys.argv.(i)

let parse_string i =
  Sys.argv.(i)

exception Invalid_value of string * string
exception Unexpected_value of string
exception Unknown_option of ident

let parse t x =
  let rec special_flag i =
    if Array.length Sys.argv <= i then
      None
    else
      match special_flag (i+1) with
      | Some Help -> Some Help (* --help has priority over --version *)
      | s ->
        begin
          match parse_arg Sys.argv.(i) with
          | Some ids ->
            let rec special s = function
              | [] -> s
              | id::ids ->
                begin
                  match special s ids with
                  | Some Help -> Some Help
                  | s ->
                    if id.long = Some "help" || id.short = Some 'h' then
                      Some Help
                    else if id.long = Some "version" || id.short = Some 'V' then
                      Some Version
                    else
                      s
                end
            in
            special s ids
          | None -> s
        end
  in
  match special_flag 1 with
  | Some Help ->
    print_usage t Format.std_formatter;
    exit 0
  | Some Version ->
    Format.fprintf Format.std_formatter "%s\n" t.version;
    exit 0
  | _ ->
    begin
      try
        let process arg action i x =
          try
            match action with
            | Flag f ->
              f x, 0
            | Int f ->
              f (parse_int i) x, 1
            | String f ->
              f (parse_string i) x, 1
            | _ -> x, 0
          with
          | Invalid_argument msg ->
            raise (Invalid_value (arg, msg))
        in
        let rec read_arg anons i x =
          if Array.length Sys.argv <= i then x else
            begin
              let str = Sys.argv.(i) in
              match parse_arg str with
              | Some ids ->
                let rec process_ids i x = function
                  | [] -> x, i
                  | id::ids ->
                    begin
                      match find_arg_opt id t.args with
                      | Some arg ->
                        let (x, n) = process str arg.action i x in
                        process_ids (i+n) x ids
                      | None -> raise (Unknown_option id)
                    end
                in
                let x, i = process_ids (i + List.length ids) x ids in
                read_arg anons i x
              | None ->
                begin
                  match anonymous_arg_opt anons with
                  | Some (anons, arg) ->
                    let (x, n) = process arg.name arg.action i x in
                    let n = max n 1 in
                    read_arg anons (i+n) x
                  | None -> raise (Unexpected_value str)
                end
            end
        in
        read_arg t.anons 1 x
      with
      | Invalid_value (arg, msg) ->
        Format.eprintf "Invalid value `%s' for option `%s'\n" msg arg;
        print_usage t Format.err_formatter;
        exit 1
      | Unknown_option id ->
        Format.eprintf "Unknown option `%t'\n" (print_id id);
        print_usage t Format.err_formatter;
        exit 1
      | Unexpected_value value ->
        Format.eprintf "Unexpected value `%s'\n" value;
        print_usage t Format.err_formatter;
        exit 1
    end
