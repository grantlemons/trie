open Alcotest

let test_contains_exact () =
    let root = make_node None in
    insert root "Hi, my name is Grant";
    check bool "contains exact" (contains_exact root "Hi, my name is Grant") true;
    check bool "contains prefix" (contains_exact root "Hi, my name is") false;
    check bool "contains exmpty" (contains_exact root "") false;;

let test_contains_prefix () =
    let root = make_node None in
    insert root "Hi, my name is Grant";
    check bool "contains exact" (contains_prefix root "Hi, my name is Grant") true;
    check bool "contains prefix" (contains_prefix root "Hi, my name is") true;
    check bool "contains exmpty" (contains_prefix root "") true;;

let contains_suite =
    [ "Contains exact", `Quick, test_contains_exact
    ; "Contains prefix", `Quick, test_contains_prefix
    ]

let () =
    Alcotest.run "Functions" [ "Contains", contains_suite ]
