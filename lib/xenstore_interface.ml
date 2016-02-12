open Xenstore

(* Use Some to indicate that we know the value and None to indicate that we believe this path to not exist *)
let xenstore_cache = Hashtbl.create 1024
let xenstore_dir_cache = Hashtbl.create 128

let normalise_path path =
	Xstringext.String.split '/' path |> List.filter (fun x -> x <> "")

let parent_of path =
	path |> List.rev |> List.tl |> List.rev

let xenstore_update xs path =
	let norm_path = normalise_path path in
	let value =
		try Some (xs.Xs.read path)
		with Xs_protocol.Enoent _ -> None
	in
	Hashtbl.replace xenstore_cache norm_path value;
	(* Invalidate the cache entry for the parent *)
	let norm_parent = parent_of norm_path in
	Hashtbl.remove xenstore_dir_cache norm_parent

let xenstore_read xs path =
	let norm_path = normalise_path path in
	let read_val path =
		match Hashtbl.find xenstore_cache norm_path with
		| None -> raise (Xs_protocol.Enoent path)
		| Some x -> x
	in
	if Hashtbl.mem xenstore_cache norm_path then
		read_val path
	else begin
		xenstore_update xs path;
		read_val path
	end

let xenstore_dir xs path =
	let norm_path = normalise_path path in
	if Hashtbl.mem xenstore_dir_cache norm_path then
		Hashtbl.find xenstore_dir_cache norm_path
	else begin
		let ents = xs.Xs.directory path in
		Hashtbl.replace xenstore_dir_cache norm_path ents;
		ents
	end

let xenstore_rm xs path =
	xs.Xs.rm path;
	let norm_path = normalise_path path in
	Hashtbl.remove xenstore_cache norm_path;
	(* Invalidate the cache entry for the parent *)
	let norm_parent = parent_of norm_path in
	Hashtbl.remove xenstore_dir_cache norm_parent
