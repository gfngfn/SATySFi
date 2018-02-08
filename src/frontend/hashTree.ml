
open MyUtil

module type S =
  sig
    type key
    type 'a t
    val empty : 'a -> 'a t
    val to_string : (key -> string) -> ('a -> string) -> 'a t -> string
    val find_stage : 'a t -> key list -> 'a option
    val update : 'a t -> key list -> ('a -> 'a) -> ('a t) option
    val add_stage : 'a t -> key list -> key -> 'a -> ('a t) option
    val search_backward : 'a t -> key list -> key list -> ('a -> 'b option) -> 'b option
  end


module Make (Key : Map.OrderedType) =
  struct

    module InternalMap = Map.Make(Key)

    type key = Key.t

    type 'a t =
      | Stage of 'a * ('a t) InternalMap.t


    let empty (vroot : 'a) : 'a t =
      Stage(vroot, InternalMap.empty)


    let rec to_string (strk : key -> string) (strf : 'a -> string) (Stage(x, imap) : 'a t) : string =
      (strf x) ^ ", { " ^ (InternalMap.fold (fun k hshtr s -> (strk k) ^ ": " ^ (to_string strk strf hshtr) ^ " " ^ s) imap "") ^ "}"


    (* -- 'find_stage hshtr addr' returns the stage at 'addr/' if it exists,
          and returns 'None' otherwise. -- *)
    let rec find_stage (Stage(x, imap) : 'a t) (addr : key list) : 'a option =
      let open OptionMonad in
      match addr with
      | [] ->
          return x

      | k :: tail ->
          InternalMap.find_opt k imap >>= fun hshtr ->
          find_stage hshtr tail


    (* -- 'update hshtr addr f' returns the hash tree that updates 'hshtr' by 'f' as to the stage at 'addr'.
          Returns 'None' if 'hshtr' does NOT have a stage at 'addr/'. -- *)
    let update (hshtr : 'a t) (addr : key list) (f : 'a -> 'a) : ('a t) option =
      let open OptionMonad in
      let rec aux (Stage(x, imap) : 'a t) (addr : key list) =
        match addr with
        | [] ->
            return (Stage(f x, imap))

        | k :: tail ->
            InternalMap.find_opt k imap >>= fun hshtrnext ->
            aux hshtrnext tail >>= fun res ->
            return (Stage(x, InternalMap.add k res imap))
      in
        aux hshtr addr


    (* -- 'add_stage hshtr addr knew vnew' returns the hash tree to which 'vnew' is inserted at the address 'addr/knew/' .
          Returns 'None' if 'hshtr' does NOT have a stage at the address 'addr'. -- *)
    let add_stage (hshtr : 'a t) (addr : key list) (knew : key) (vnew : 'a) : ('a t) option =
      let open OptionMonad in
      let rec aux (Stage(x, imap) : 'a t) (addr : key list) =
        match addr with
        | [] ->
            return (Stage(x, InternalMap.add knew (Stage(vnew, InternalMap.empty)) imap))

        | k :: tail ->
            InternalMap.find_opt k imap >>= fun hshtrnext ->
            aux hshtrnext tail >>= fun res ->
            let imapnew = InternalMap.add k res imap in
            return (Stage(x, imapnew))
      in
        aux hshtr addr


    (* -- 'search_backward hshtr addr addrlast findf' lookups stages
          from the address 'd_1/.../d_n/addrlast', 'd_1/.../d_(n-1)/addrlast' down to 'addrlast'.
          Returns 'None' if every candidate stage does NOT exist or answers 'None' for 'findf'. -- *)
    let rec search_backward (Stage(_, imap) as hshtr : 'a t) (addr : key list) (addrlast : key list) (findf : 'a -> 'b option) : 'b option =
      let open OptionMonad in
      match addr with
      | [] ->
          find_stage hshtr addrlast >>= fun xsub ->
          findf xsub

      | k :: tail ->
          InternalMap.find_opt k imap >>= fun hshtrnext ->
          let res = search_backward hshtrnext tail addrlast findf in
          begin
            match res with
            | Some(_) ->
                res

            | None ->
                find_stage hshtr addrlast >>= fun xsub ->
                findf xsub
          end

  end
