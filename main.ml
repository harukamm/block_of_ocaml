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

let main () =
  try
    readenv ppf Before_args;
    Arg.parse [] anonymous usage;
    Compenv.process_deferred_actions
      (ppf,
       My_compile.implementation,
       My_compile.interface,
       ".cmo",
       ".cma");
    readenv ppf Before_link;
    if
      List.length (List.filter (fun x -> !x)
                      [make_archive;make_package;compile_only;output_c_object])
        > 1
    then
      if !print_types then
        fatal "Option -i is incompatible with -pack, -a, -output-obj"
      else
        fatal "Please specify at most one of -pack, -a, -c, -output-obj";
    if !make_archive then begin
      Compmisc.init_path false;

      Bytelibrarian.create_archive ppf
                                   (Compenv.get_objfiles ~with_ocamlparam:false)
                                   (extract_output !output_name);
      Warnings.check_fatal ();
    end
    else if !make_package then begin
      Compmisc.init_path false;
      let extracted_output = extract_output !output_name in
      let revd = get_objfiles ~with_ocamlparam:false in
      Bytepackager.package_files ppf (Compmisc.initial_env ())
        revd (extracted_output);
      Warnings.check_fatal ();
    end
    else if not !compile_only && !objfiles <> [] then begin
      let target =
        if !output_c_object then
          let s = extract_output !output_name in
          if (Filename.check_suffix s Config.ext_obj
            || Filename.check_suffix s Config.ext_dll
            || Filename.check_suffix s ".c")
          then s
          else
            fatal
              (Printf.sprintf
                 "The extension of the output file must be .c, %s or %s"
                 Config.ext_obj Config.ext_dll
              )
        else
          default_output !output_name
      in
      Compmisc.init_path false;
      Bytelink.link ppf (get_objfiles ~with_ocamlparam:true) target;
      Warnings.check_fatal ();
    end;
  with x ->
    Location.report_exception ppf x;
    exit 2

let _ =
  Timings.(time All) main ();
  if !Clflags.print_timings then Timings.print Format.std_formatter;
  exit 0
