(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Clflags
open Compenv

let usage = "Usage: ocamlc <options> <files>\nOptions are:"

(* Error messages to standard error formatter *)
let ppf = Format.err_formatter

let show_config () =
  Config.print_config stdout;
  exit 0;
;;

let block_of_ocaml code =
  try
    let xml_str = My_compile.implementation ppf code in
    (Some xml_str, None)
  with e ->
    (None, Some e)

let main filename =
  try
    let _ = My_compile.implementation ppf filename in
    ()
  with x ->
    Location.report_exception ppf x;
    exit 2

let _ =
  Timings.(time All) main "tests/let.ml";
  if !Clflags.print_timings then Timings.print Format.std_formatter;
  exit 0
