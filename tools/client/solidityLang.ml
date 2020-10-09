open Tezos_protocol_sol.Protocol
open Solidity_ast
open Solidity_common
open Tezos_data_encoding
open LiquidClientUtils

module Lang = struct

  exception NoAssociatedConstant of Solidity_ast.type_

  type const = Solidity_ast.constant

  type contract = Solidity_ast.program

  type datatype = Solidity_ast.type_ list

  type location = Solidity_common.loc

  type loc_info = string option

  type compiled_init = 
    | No_init
    | Init_constant of const
    | Init_code of contract * (string * datatype) list
    | Init_components of (string * datatype) list

  let name = "Solidity"

  let unit = Solidity_ast.CTuple []

  let tuple l = Solidity_ast.CTuple l

  let tunit = []

  let ttuple l =
    List.map
      (function
        | [t] -> t
        | _ -> failwith "Solidity.ttuple: bad type") l
  let list_big_maps (storage : const) (storage_ty : datatype) =
    [] (* No bigmaps in Solidity yet *)

  let storage _contract = tunit (* No representation *)

  let return_to_typ_return ((typ, stor, _) : Solidity_ast.return) = (typ, stor)
    

  let entries (contract : contract) =
    let module V = Solidity_visitor in
    let vis = object
      inherit V.Ast.init_ast_visitor

      val mutable ctrct : string = ""

      val mutable res : (string * datatype) list = []

      method getRes () = res

      method! visitContractDef c =
        let () = ctrct <- Solidity_common.Ident.to_string c.contract_name.contents in
        DoChildren

      method! visitFunctionDef fd =
        match fd.fun_mutability with
        | MNonPayable | MPayable ->
          let ty = Solidity_ast.FunctionType {
              fun_type_params = fd.fun_params;
              fun_type_returns = List.map return_to_typ_return fd.fun_returns;
              fun_type_visibility = fd.fun_visibility;
              fun_type_mutability = fd.fun_mutability
            } in
          let name = 
            Format.sprintf
              "%s.%s"
              ctrct
              (Solidity_common.Ident.to_string fd.fun_name.contents) in
          res <- (name, [ty]) :: res;
          SkipChildren
        | MView | MPure -> SkipChildren
    end in
    let () =
      V.Ast.visitProgram
        (vis :> V.Ast.ast_visitor)
        contract in
    vis#getRes ()

  let apply_big_map_subst _ c = c (* No bigmaps yet *)

  let default_empty_const t = match t with
    | Solidity_ast.ElementaryType e -> begin
        match e with
        | Solidity_common.TypeBool -> Solidity_ast.CBool true
        | TypeInt size
        | TypeUint size
        | TypeFixed (size, _)
        | TypeUfixed (size, _) -> CNumber (Q.zero, Unit, size)
        | TypeAddress _ -> CAddress (Contract_repr.zero)
        | TypeBytes _ -> raise (NoAssociatedConstant t)
        | TypeString -> CString "__default_solidity_string__"
      end
  | Array _ -> CArray []
  | Mapping _ 
  | FunctionType _
  | UserDefinedType _ -> raise (NoAssociatedConstant t)

  let default_empty_const (t : datatype) : const =
    match List.map default_empty_const t with
    | [c] -> c
    | l -> tuple l

  let print_loc fmt ((l1, c1), (l2, c2)) =
    Format.fprintf fmt "%d.%d-%d.%d" l1 c1 l2 c2

  let parse_const _ = failwith "TODO: parse_const"

  let parse_contract _ = failwith "TODO: parse_contract"

  let parse_datatype _ = failwith "TODO: parse datatype"

  let const_encoding = Solidity_json_encoding.value_encoding

  let contract_encoding = Solidity_json_encoding.SourceAst.code_encoding

  let (const_encoding : const Json_encoding.encoding) =
    Json_encoding.conv
      (fun _ -> failwith "Solidity.const_encoding unimplemented")
      (fun _ -> failwith "Solidity.const_encoding unimplemented")
      Json_encoding.unit

  let (contract_encoding : contract Json_encoding.encoding)  =
    Json_encoding.conv
      (fun _ -> failwith "Solidity.contract_encoding unimplemented")
      (fun _ -> failwith "Solidity.contract_encoding unimplemented")
      Json_encoding.unit

  let (datatype_encoding : datatype Json_encoding.encoding)  =
    Json_encoding.conv
      (fun _ -> failwith "Solidity.datatype_encoding unimplemented")
      (fun _ -> failwith "Solidity.datatype_encoding unimplemented")
      Json_encoding.unit

  let print_datatype t =
    String.concat "," @@ List.map Solidity_printer.SourceAst.string_of_type t

  let print_const c =
    Solidity_printer.string_of_constant c

  let print_contract c = Solidity_printer.SourceAst.string_of_code c

  let datatype = new Lazy_superposed.superposer (object
    method parse = parse_datatype
    method print = print_datatype
    method encoding = datatype_encoding
  end)

  let const = new Lazy_superposed.superposer (object
    method parse = parse_const
    method print = print_const
    method encoding = const_encoding
  end)

  let contract = new Lazy_superposed.superposer (object
    method parse = parse_contract
    method print = print_contract
    method encoding = contract_encoding
  end)
  
  let normalize_loc (l : loc) = LiquidTypes.{
    loc_file = "solidity_project";
    loc_pos = Some l
  }

  let loc_encoding =
    let pair = Json_encoding.(tup2 int int) in
    Json_encoding.tup2 pair pair

  let next_loc ((_, _), (l2, _)) =
    (l2 + 1, 1), (l2 + 1, 2)

  let compareXX cmp (c1, c1') (c2, c2') =
    let c = cmp c1 c2 in
    if c = 0 then
      cmp c1' c2'
    else c

  let compare_loc (l1 : loc) (l2 : loc) =
    compareXX (compareXX (-)) l1 l2 
end

module Source = Lang
module Target = Lang

let compile_contract c =
  c, Source.No_init, []

let decompile_contract c = c

let compile_const ?ty c = ignore ty; c
let decompile_const ?ty c = ignore ty; c

let compile_datatype t = t
