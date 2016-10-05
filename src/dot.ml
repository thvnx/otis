let open_subgraph id =
  Printf.printf "subgraph cluster_%s {\n" id

let close_subgraph name =
  Printf.printf "label=\"%s\";\n}\n" name

let node id name =
  Printf.printf "\"%s\" [label=\"%s\"];\n" id name

let edge src dst label =
  Printf.printf "\"%s\" -> \"%s\" [label=\"%s\"];\n" src dst label
