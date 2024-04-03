let explode str = List.init (String.length str) (String.get str);;
let implode lst = String.of_seq (List.to_seq lst);;

type trie =
    | Node of { children : trie option array }
    | Terminal of { children : trie option array };;

let set_terminal = function
    | Some Node nd -> Some (Terminal { children = nd.children })
    | n -> n;;

let make_node nd = match nd with
    | None -> Some (Node { children = Array.init 95 (fun _ -> None) })
    | _ -> nd;;

let get_children = function
    | Node { children } -> children
    | Terminal { children } -> children;;

let char_index ch = Char.code ch - Char.code ' '
let index_char i = Char.chr (i + Char.code ' ')

let set_child arr ch nd =
    arr.(char_index ch) <- nd;;

let get_child nd ch =
    (get_children nd).(char_index ch);;

let rec insert nd str =
    match nd with
    | None -> ()
    | Some nd -> match explode str with
        | [] -> ()
        | [ch] -> set_child (get_children nd) ch (set_terminal @@ make_node @@ get_child nd ch)
        | ch :: rem -> set_child (get_children nd) ch (make_node @@ get_child nd ch); insert (get_child nd ch) (implode rem);;

let rec contains_prefix nd str = match nd with
    | None -> false
    | Some nd -> match explode str with
        | [] -> true
        | [ch] -> Option.is_some (get_child nd ch)
        | ch :: rem -> contains_prefix (get_child nd ch) (implode rem);;

let rec contains_exact nd str = match nd with
    | None -> false
    | Some nd -> match explode str with
        | [] -> false
        | [ch] ->
            begin
                match (get_child nd ch) with
                    | Some Terminal _ -> true
                    | _ -> false
            end
        | ch :: rem -> contains_exact (get_child nd ch) (implode rem);;

let rec count nd = match nd with
    | None -> 0
    | Some Node { children } -> Array.fold_left (fun acc x -> acc + count x) 0 children
    | Some Terminal { children } -> Array.fold_left (fun acc x -> acc + count x) 1 children 

let rec for_each nd fn = 
    let rec for_each_internal nd lst = match nd with
        | None -> ()
        | Some Node { children } -> Array.iteri (fun i x -> for_each_internal x ((index_char i) :: lst)) children
        | Some Terminal { children } -> fn @@ List.rev lst; Array.iteri (fun i x -> for_each_internal x ((index_char i) :: lst)) children;
    in
    for_each_internal nd []

let root = make_node None;;
insert root "Hi, my name is Grant";;
insert root "Hi, my name is Byron";;
insert root "Hi, my name is Elijah";;
