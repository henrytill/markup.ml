(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

open Common

type html_input

val make :
  report:Error.parse_handler ->
  ?context:[< `Document | `Fragment of string > `Fragment ] ->
  (unit -> int) ->
  html_input

val next_signal : html_input -> (location * signal) option
