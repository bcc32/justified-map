open! Core_kernel
open! Import

let example = Map.of_alist_exn (module String) [ "hello", 0; "world", 2 ]

let%expect_test "basic usage" =
  Justified_map.with_map
    example
    { f =
        (fun map ->
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
                   "Expected key to be absent"
                 , { key = (Justified_map.Key.get key : string) }]
           in
           [%expect {| |}])
    }
;;
