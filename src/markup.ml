(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)



module type IO =
sig
  type 'a t

  val return : 'a -> 'a t
  val of_cps : ((exn -> unit) -> ('a -> unit) -> unit) -> 'a t
  val to_cps : (unit -> 'a t) -> ((exn -> unit) -> ('a -> unit) -> unit)
end

module Synchronous : IO with type 'a t = 'a =
struct
  type 'a t = 'a

  exception Not_synchronous

  let return x = x

  let of_cps f =
    let result = ref None in
    f raise (fun v -> result := Some v);
    match !result with
    | None -> raise Not_synchronous
    | Some v -> v

  let to_cps f =
    fun throw k ->
      match f () with
      | v -> k v
      | exception exn -> throw exn
end



type async = unit
type sync = unit

type ('data, 'sync) stream = 'data Kstream.t

let kstream s = s
let of_kstream s = s

let of_list = Kstream.of_list



type location = Common.location
let compare_locations = Common.compare_locations

module Error = Error



type name = Common.name

type xml_declaration = Common.xml_declaration =
  {version    : string;
   encoding   : string option;
   standalone : bool option}

type doctype = Common.doctype =
  {doctype_name      : string option;
   public_identifier : string option;
   system_identifier : string option;
   raw_text          : string option;
   force_quirks      : bool}

type signal = Common.signal

let signal_to_string = Common.signal_to_string

type 's parser =
  {mutable location : location;
   mutable signals  : (signal, 's) stream}

let signals parser = parser.signals
let location parser = parser.location

let stream_to_parser s =
  let parser = {location = (1, 1); signals = Kstream.empty ()} in
  parser.signals <-
    s |> Kstream.map (fun (l, v) _ k -> parser.location <- l; k v);
  parser

module Cps =
struct
  let parse_xml
      report ?encoding namespace entity context source =
    let byte_src () =
      let r = ref None in
      Kstream.next_option source raise (fun v -> r := v);
      match !r with
      | None -> -1
      | Some c -> Char.code c
    in

    let with_encoding (encoding : Encoding.t) byte_src k =
      let decoder = encoding ~report ~byte_src in
      let context' =
        match context with
        | None -> None
        | Some `Document -> Some `Document
        | Some `Fragment -> Some `Fragment
      in
      let xi = Xml_parser.make ~report ~resolve_reference:entity
        ~namespace ~context:context' ~decoder in
      Kstream.make (fun _ e k ->
        match Xml_parser.next_signal xi with
        | None -> e ()
        | Some v -> k v)
      |> k
    in

    let constructor _throw k =
      match encoding with
      | Some enc -> with_encoding enc byte_src k
      | None ->
        let enc, replay = Detect.select_xml byte_src in
        with_encoding enc replay k
    in

    Kstream.construct constructor
    |> stream_to_parser

  let write_xml report prefix signals =
    signals
    |> Xml_writer.write report prefix
    |> Utility.strings_to_bytes

  let parse_html report ?encoding context source =
    let byte_src () =
      let r = ref None in
      Kstream.next_option source raise (fun v -> r := v);
      match !r with
      | None -> -1
      | Some c -> Char.code c
    in

    let with_encoding (encoding : Encoding.t) byte_src k =
      let decoder = encoding ~report ~byte_src in
      let context' = match context with
        | None -> None
        | Some `Document -> Some `Document
        | Some (`Fragment s) -> Some (`Fragment s)
      in
      let hi = Html_direct.make ~report ?context:context' decoder in
      Kstream.make (fun _ e k ->
        match Html_direct.next_signal hi with
        | None -> e ()
        | Some v -> k v)
      |> k
    in

    let constructor _throw k =
      match encoding with
      | Some enc -> with_encoding enc byte_src k
      | None ->
        let enc, replay = Detect.select_html byte_src in
        with_encoding enc replay k
    in

    Kstream.construct constructor
    |> stream_to_parser

  let write_html ?escape_attribute ?escape_text signals =
    signals
    |> Html_writer.write ?escape_attribute ?escape_text
    |> Utility.strings_to_bytes
end



let string s =
  let src = Stream_io.string s in
  Kstream.make (fun _ e k ->
    let b = src () in
    if b = -1 then e () else k (Char.chr b))

let buffer b =
  let src = Stream_io.buffer b in
  Kstream.make (fun _ e k ->
    let b = src () in
    if b = -1 then e () else k (Char.chr b))

let channel c =
  let src = Stream_io.channel c in
  Kstream.make (fun _ e k ->
    let b = src () in
    if b = -1 then e () else k (Char.chr b))

let file f =
  let src, close = Stream_io.file f in
  let s = Kstream.make (fun _ e k ->
    let b = src () in
    if b = -1 then e () else k (Char.chr b))
  in
  s, close

(** Convert a char Kstream to a pull function for use with stream_io output. *)
let kstream_to_pull (s : char Kstream.t) : unit -> char option =
  fun () ->
    let r = ref None in
    Kstream.next_option s raise (fun v -> r := v);
    !r

let to_channel c bytes =
  Stream_io.to_channel c (kstream_to_pull bytes)

let to_file f bytes =
  Stream_io.to_file f (kstream_to_pull bytes)



let preprocess_input_stream source =
  Input.preprocess (fun _ -> true) Error.ignore_errors source



include Utility



module Ns =
struct
  let html = Common.html_ns
  let svg = Common.svg_ns
  let mathml = Common.mathml_ns
  let xml = Common.xml_ns
  let xmlns = Common.xmlns_ns
  let xlink = Common.xlink_ns
end



module type ASYNCHRONOUS =
sig
  type 'a io

  module Encoding :
  sig
    type t = Encoding.t

    val decode :
      ?report:(location -> Error.t -> unit io) -> t ->
      (char, _) stream -> (int, async) stream
  end

  val parse_xml :
    ?report:(location -> Error.t -> unit io) ->
    ?encoding:Encoding.t ->
    ?namespace:(string -> string option) ->
    ?entity:(string -> string option) ->
    ?context:[< `Document | `Fragment ] ->
    (char, _) stream -> async parser

  val write_xml :
    ?report:((signal * int) -> Error.t -> unit io) ->
    ?prefix:(string -> string option) ->
    ([< signal ], _) stream -> (char, async) stream

  val parse_html :
    ?report:(location -> Error.t -> unit io) ->
    ?encoding:Encoding.t ->
    ?context:[< `Document | `Fragment of string ] ->
    (char, _) stream -> async parser

  val write_html :
  ?escape_attribute:(string -> string) ->
  ?escape_text:(string -> string) ->
  ([< signal ], _) stream -> (char, async) stream

  val fn : (unit -> char option io) -> (char, async) stream

  val to_string : (char, _) stream -> string io
  val to_buffer : (char, _) stream -> Buffer.t io

  val stream : (unit -> 'a option io) -> ('a, async) stream

  val next : ('a, _) stream -> 'a option io
  val peek : ('a, _) stream -> 'a option io

  val transform :
    ('a -> 'b -> ('c list * 'a option) io) -> 'a -> ('b, _) stream ->
      ('c, async) stream
  val fold : ('a -> 'b -> 'a io) -> 'a -> ('b, _) stream -> 'a io
  val map : ('a -> 'b io) -> ('a, _) stream -> ('b, async) stream
  val filter : ('a -> bool io) -> ('a, _) stream -> ('a, async) stream
  val filter_map : ('a -> 'b option io) -> ('a, _) stream -> ('b, async) stream
  val iter : ('a -> unit io) -> ('a, _) stream -> unit io
  val drain : ('a, _) stream -> unit io

  val to_list : ('a, _) stream -> 'a list io

  val load : ('a, _) stream -> ('a, sync) stream io

  val tree :
    ?text:(string list -> 'a) ->
    ?element:(name -> (name * string) list -> 'a list -> 'a) ->
    ?comment:(string -> 'a) ->
    ?pi:(string -> string -> 'a) ->
    ?xml:(xml_declaration -> 'a) ->
    ?doctype:(doctype -> 'a) ->
    ([< signal ], _) stream -> 'a option io
end

module Asynchronous (IO : IO) =
struct
  (* Convert an async report handler (returns IO.t) to a direct-style one.
     The IO action is run synchronously via IO.to_cps with trivial continuations. *)
  let wrap_report report =
    fun l e ->
      IO.to_cps (fun () -> report l e) (fun _exn -> ()) (fun () -> ())

  module Encoding =
  struct
    include Encoding

    let decode ?(report = fun _ _ -> IO.return ()) (f : Encoding.t) s =
      let byte_src () =
        let r = ref None in
        Kstream.next_option s raise (fun v -> r := v);
        match !r with
        | None -> -1
        | Some c -> Char.code c
      in
      let decoder = f ~report:(wrap_report report) ~byte_src in
      Kstream.make (fun _ e k ->
        let v = decoder () in
        if v = -1 then e () else k v)
  end

  let parse_xml
      ?(report = fun _ _ -> IO.return ())
      ?encoding
      ?(namespace = fun _ -> None)
      ?(entity = fun _ -> None)
      ?context
      source =

    Cps.parse_xml
      (wrap_report report) ?encoding namespace entity context source

  let write_xml
      ?(report = fun _ _ -> IO.return ())
      ?(prefix = fun _ -> None)
      signals =

    Cps.write_xml (wrap_report report) prefix signals

  let parse_html
      ?(report = fun _ _ -> IO.return ())
      ?encoding
      ?context
      source =

    Cps.parse_html (wrap_report report) ?encoding context source

  let write_html ?escape_attribute ?escape_text signals =
    Cps.write_html ?escape_attribute ?escape_text signals

  let to_string bytes =
    (fun _throw k -> k (Stream_io.to_string (kstream_to_pull bytes))) |> IO.of_cps
  let to_buffer bytes =
    (fun _throw k -> k (Stream_io.to_buffer (kstream_to_pull bytes))) |> IO.of_cps

  let stream f =
    let f = IO.to_cps f in
    (fun throw e k ->
      f throw (function
        | None -> e ()
        | Some v -> k v))
    |> Kstream.make

  let fn = stream

  let next s = Kstream.next_option s |> IO.of_cps
  let peek s = Kstream.peek_option s |> IO.of_cps

  (* Without Flambda, thunks are repeatedly created and passed on IO.to_cps,
     resulting in a performance penalty. Flambda seems to optimize this away,
     however. *)

  let transform f v s =
    Kstream.transform (fun v s -> IO.to_cps (fun () -> f v s)) v s

  let fold f v s =
    Kstream.fold (fun v v' -> IO.to_cps (fun () -> f v v')) v s |> IO.of_cps

  let map f s = Kstream.map (fun v -> IO.to_cps (fun () -> f v)) s

  let filter f s = Kstream.filter (fun v -> IO.to_cps (fun () -> f v)) s

  let filter_map f s = Kstream.filter_map (fun v -> IO.to_cps (fun () -> f v)) s

  let iter f s =
    Kstream.iter (fun v -> IO.to_cps (fun () -> f v)) s |> IO.of_cps

  let drain s = iter (fun _ -> IO.return ()) s

  let to_list s = Kstream.to_list s |> IO.of_cps

  let load s =
    (fun throw k -> Kstream.to_list s throw (fun l -> k (Kstream.of_list l)))
    |> IO.of_cps

  let tree ?text ?element ?comment ?pi ?xml ?doctype s =
    Utility.tree ?text ?element ?comment ?pi ?xml ?doctype s |> IO.of_cps
end

include Asynchronous (Synchronous)
