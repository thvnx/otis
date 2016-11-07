let rec maxn ?prev l =
  let p = match prev with None -> 1 | Some pre -> pre in 
  match l with
  | [] -> p
  | h::t -> maxn ~prev:(max h p) t
    
