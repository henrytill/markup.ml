(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

open Common

(* The report parameter in Parsing and Writing is a callback of type
   (unit -> Error.t -> unit): call report () error to signal a namespace error. *)

module Parsing :
sig
  type context

  val init : (string -> string option) -> context
  val push :
    (unit -> Error.t -> unit) ->
    context ->
    string -> (string * string) list ->
    (exn -> unit) ->
      ((name * (name * string) list) -> unit) ->
        unit
  val pop : context -> unit
  val expand_element :
    (unit -> Error.t -> unit) -> context -> string -> name

  val parse : string -> string * string
end

module Writing :
sig
  type context

  val init : (string -> string option) -> context
  val push :
    (unit -> Error.t -> unit) ->
    context ->
    name -> (name * string) list ->
    (exn -> unit) ->
      ((string * (string * string) list) -> unit) ->
        unit
  val pop : context -> unit
end
