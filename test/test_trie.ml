open Trie
open Alcotest

let test_contains_exact_full () =
    let root = make_node None in
    insert root "Hi, my name is Grant";
    check bool "contains exact" (contains_exact root "Hi, my name is Grant") true;;

let test_contains_exact_partial () =
    let root = make_node None in
    insert root "Hi, my name is Grant";
    check bool "contains exact" (contains_exact root "Hi, my name is") false;;

let test_contains_exact_empty () =
    let root = make_node None in
    insert root "Hi, my name is Grant";
    check bool "contains exact" (contains_exact root "") false;;

let test_contains_prefix_full () =
    let root = make_node None in
    insert root "Hi, my name is Grant";
    check bool "contains exact" (contains_prefix root "Hi, my name is Grant") true;;

let test_contains_prefix_partial () =
    let root = make_node None in
    insert root "Hi, my name is Grant";
    check bool "contains exact" (contains_prefix root "Hi, my name is") true;;

let test_contains_prefix_empty () =
    let root = make_node None in
    insert root "Hi, my name is Grant";
    check bool "contains exact" (contains_prefix root "") true;;

let contains_exact_suite =
    [ "Contains exact", `Quick, test_contains_exact_full
    ; "Contains prefix", `Quick, test_contains_exact_partial
    ; "Contains empty", `Quick, test_contains_exact_empty
    ]

let contains_prefix_suite =
    [ "Contains exact", `Quick, test_contains_prefix_full
    ; "Contains prefix", `Quick, test_contains_prefix_partial
    ; "Contains empty", `Quick, test_contains_prefix_empty
    ]

let () =
    Alcotest.run "Functions" [ "Contains Exact", contains_exact_suite
                             ; "Contains Partial", contains_prefix_suite
                             ]
