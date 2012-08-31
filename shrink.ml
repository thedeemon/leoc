open ExtLib
open Commons
open Leoc

let shrink_vals code =
	let nums = Hashtbl.create 42 in
	let vars = ref [] in
	let mkvar c = 
		let varname = Printf.sprintf "value_%Ld_%d" c (uid ()) in 
		vars := (varname, c) :: !vars;
		varname, 0 in	 
	let o = object(self)
		inherit mapper as super
		method map_raw_rvalue = function
			| Val x -> if Int64.abs x < 80000L then Val x else
					begin
						let (var, count) =
							try let (v,c) = Hashtbl.find nums x in
									if c < 500 then (v,c) else mkvar x										
							with Not_found -> mkvar x in
						Hashtbl.replace nums x (var,count+1);
						LV(Var var)
					end
			| rv -> super#map_raw_rvalue rv
	end in
	let code1 = o#map_code code in
	let init_code = List.rev !vars |> List.map (fun (v,x) -> [DefVar v, 1; Assign(ASInt, Var v, (Val x, 1)),1])
		|> List.concat in
	init_code @ code1