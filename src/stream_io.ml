(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

(** Byte source: returns next byte as int 0-255, or -1 at EOF. *)
type byte_src = unit -> int

let string s =
  let i = ref 0 in
  fun () ->
    if !i >= String.length s then -1
    else begin
      let c = Char.code s.[!i] in
      i := !i + 1;
      c
    end

let buffer b =
  let i = ref 0 in
  fun () ->
    if !i >= Buffer.length b then -1
    else begin
      let c = Char.code (Buffer.nth b !i) in
      i := !i + 1;
      c
    end

let channel c =
  let buf = Bytes.create 4096 in
  let pos = ref 0 in
  let len = ref 0 in
  let at_eof = ref false in
  fun () ->
    if !pos < !len then begin
      let ch = Char.code (Bytes.get buf !pos) in
      pos := !pos + 1;
      ch
    end else if !at_eof then
      -1
    else begin
      let n = input c buf 0 4096 in
      if n = 0 then begin at_eof := true; -1 end
      else begin
        len := n;
        pos := 1;
        Char.code (Bytes.get buf 0)
      end
    end

let file f =
  let c = open_in f in
  let src = channel c in
  let src' () =
    let b = src () in
    if b = -1 then (close_in_noerr c; -1)
    else b
  in
  src', (fun () -> close_in_noerr c)

(** Output: consume a char stream and write to various sinks. *)

let to_buffer s =
  let buf = Buffer.create 4096 in
  let rec loop () =
    match s () with
    | None -> buf
    | Some c -> Buffer.add_char buf c; loop ()
  in
  loop ()

let to_string s =
  Buffer.contents (to_buffer s)

let to_channel c s =
  let rec loop () =
    match s () with
    | None -> ()
    | Some ch -> output_char c ch; loop ()
  in
  loop ()

let to_file f s =
  let c = open_out f in
  (try to_channel c s with exn -> close_out_noerr c; raise exn);
  close_out_noerr c
