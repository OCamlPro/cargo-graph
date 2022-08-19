(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2022 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Lesser General    *)
(*  Public License version 2.1, with the special exception on linking     *)
(*  described in the LICENSE.md file in the root directory.               *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)


(* If you delete or rename this file, you should add
   'src/cargo_graph_lib/main.ml' to the 'skip' field in "drom.toml" *)

open Ez_file.V1
open EzFile.OP
open EzCompat


type package = {
  p_name : string ;
  p_file : string ;
  p_deps : string list ;
}

let main () =


  let format = ref "pdf" in
  let excludes = ref StringSet.empty in
  let to_print = ref [] in
  Arg.parse [ "-x", Arg.String (fun s ->
      excludes := StringSet.add s !excludes), " exclude directory";
    ] (fun s -> to_print := s :: !to_print) "";

  let packages = ref StringMap.empty in
  let add_package p =
    match StringMap.find p.p_name !packages with
    | p2 ->
        Printf.eprintf
          "WARN: package %s from\n%s\nconflicts with same package from\n%s\n%!"
          p.p_name p.p_file
          p2.p_file
    | exception Not_found ->
        packages := StringMap.add p.p_name p !packages
  in

  let read_cargo filename =
    try
      let table = EzToml.EZ.from_file_exn filename in
      let package_name =
        EzToml.get_string_option table [ "package" ; "name" ] in
      match package_name with
      | None ->
          Printf.eprintf "WARN: no package name in %s\n%!" filename
      | Some package_name ->
          Printf.eprintf "* %s in %s\n%!" package_name filename;
          match EzToml.get table ["dependencies"] with
          | exception _ ->
              Printf.eprintf "     (no dependencies)\n%!"
          | TTable t ->
              let deps = ref [] in
              EzToml.iter (fun k _v ->
                  deps := k :: !deps
                ) t;
              Printf.eprintf "     %d dependencies\n%!" ( List.length !deps);
              add_package {
                p_name = package_name;
                p_deps = !deps;
                p_file = filename;
              }
          | _ -> assert false

    with Failure s ->
      Printf.eprintf "Error: %s\n  in %s\n%!" s filename;
      exit 2
  in

  let rec iter dir =
    let files = Sys.readdir (if dir = "" then "." else dir) in
    Array.iter (fun file ->
        let fullname = dir // file in
        if not ( StringSet.mem fullname !excludes ) then
          match file, file.[0] with
          | "Cargo.toml", _ ->
              read_cargo fullname
          | _, ( '.' | '_' ) -> ()
          | _ ->
              match (Unix.lstat fullname).Unix.st_kind with
              | S_DIR ->
                  iter fullname
              | _ ->
                  ()
      ) files;
  in
  iter "";


  List.iter (fun s ->
      let graph = Ez_dot.V1.create "dependencies" [] in
      let dot_file = s ^ ".dot" in

      let packages = ref !packages in
      let nodes = ref StringMap.empty in

      let open Ez_dot.V1 in
      let rec get_node s =
        match StringMap.find s !nodes with
        | (node, exists, deps) -> (node,exists, deps)
        | exception Not_found ->
            let (node, exists, deps) =
              match StringMap.find s !packages with
              | exception Not_found ->
                  Printf.eprintf "ERROR: missing package %S\n%!" s;
                  let attributes = [NodeColor "red"; NodeStyle Filled] in
                  let node = Ez_dot.V1.node graph s attributes in
                  node, false, StringSet.empty
              | p ->
                  let attributes =
                    if EzString.starts_with p.p_file ~prefix:"compiler" then
                      [NodeColor "green"; NodeStyle Filled]
                    else
                      []
                  in
                  let node = Ez_dot.V1.node graph s attributes in

                  let first_deps = ref StringSet.empty in
                  let second_deps = ref StringSet.empty in
                  let deps =
                    List.map (fun dep ->
                        first_deps := StringSet.add dep !first_deps ;
                        let (dep_node, dep_exists, dep_deps) = get_node dep in
                        second_deps := StringSet.union dep_deps !second_deps;
                        if not dep_exists then
                          Printf.eprintf "WARN: missing dep %S of %S\n%!"
                            dep p.p_name;
                        (dep, dep_node, dep_deps)
                      ) p.p_deps;
                  in
                  List.iter (fun (dep, dep_node, _dep_deps) ->
                      if not ( StringSet.mem dep !second_deps ) then
                        Ez_dot.V1.add_edge node dep_node []
                    ) deps;
                  let deps = StringSet.union !first_deps !second_deps in
                  node, true, deps
            in
            nodes := StringMap.add s (node, exists, deps) !nodes;
            (node, exists, deps)
      in
      ignore ( get_node s );

      Ez_dot.V1.save graph dot_file ;
      let outfile = s ^ "." ^ !format in
      Ez_dot.V1.dot2file ~dotfile:dot_file ~format:!format ~outfile;
      Printf.eprintf "Generated %S and %S\n%!" dot_file outfile;

    ) !to_print;
