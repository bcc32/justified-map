open! Core_kernel
open! Import

let example = Map.of_alist_exn (module String) [ "hello", 0; "world", 2 ]

(* FIXME: factor out common key inputs *)
let%expect_test "basic usage" =
  Justified_map.with_map example ~f:(fun (T map) ->
    let () =
      match Justified_map.mem map "hello" with
      | None -> print_cr [%here] [%sexp "Expected key to be present"]
      | Some key -> printf "%d\n" (Justified_map.find map key)
    in
    [%expect {| 0 |}];
    let () =
      match Justified_map.mem map "hey there" with
      | None -> ()
      | Some key ->
        print_cr
          [%here]
          [%sexp
            "Expected key to be absent", { key = (Justified_map.Key.get key : string) }]
    in
    [%expect {| |}])
;;

let%expect_test "with_singleton example" =
  Justified_map.with_singleton
    (module Int)
    ~key:1
    ~data:'a'
    ~f:(fun (T (key, map)) -> printf "%c\n" (Justified_map.find map key));
  [%expect {| a |}]
;;

let%expect_test "inserting example" =
  Justified_map.with_map example ~f:(fun (T map) ->
    let present_key = Justified_map.mem_exn map "hello" in
    let () =
      match Justified_map.mem map "hey there" with
      | None -> ()
      | Some key ->
        print_cr
          [%here]
          [%sexp
            "Expected key to be absent", { key = (Justified_map.Key.get key : string) }]
    in
    (* TODO: It would be nice to use some sort of extended let syntax here. *)
    Justified_map.inserting
      map
      ~key:"hey there"
      ~data:5
      ~f:(fun (T { key; infer; map }) ->
        require_does_not_raise [%here] (fun () ->
          ignore
            (Justified_map.mem_exn map "hey there" : (string, _) Justified_map.Key.t));
        print_s [%sexp (Justified_map.find map key : int)];
        [%expect {| 5 |}];
        print_s [%sexp (Justified_map.find map (infer present_key) : int)];
        [%expect {| 0 |}];
        require_equal [%here] (module String) "hey there" (Justified_map.Key.get key));
    [%expect {| |}])
;;
