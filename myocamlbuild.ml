open Ocamlbuild_plugin;;

dispatch begin function
  | After_rules ->
    flag ["ocaml"; "compile"] (S[A"-w"; A"@5@8@10@11@12@14@23@24@26@29"]);
  | _ -> ()
end
