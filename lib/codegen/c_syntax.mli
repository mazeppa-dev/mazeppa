(* This module is designed to be opened. *)

type c_identifier

val c_identifier : Symbol.t -> c_identifier

type c_storage_class_specifier

val c_static : [> `CStorageClassSpecifier of c_storage_class_specifier ]

val c_extern : [> `CStorageClassSpecifier of c_storage_class_specifier ]

type c_type_specifier

val c_void : [> `CTypeSpecifier of c_type_specifier ]

val c_char : [> `CTypeSpecifier of c_type_specifier ]

val c_int : [> `CTypeSpecifier of c_type_specifier ]

val c_typedef_name : c_identifier -> [> `CTypeSpecifier of c_type_specifier ]

type c_type_qualifier

val c_const : [> `CTypeQualifier of c_type_qualifier ]

type c_specifier_qualifier =
  [ `CTypeSpecifier of c_type_specifier
  | `CTypeQualifier of c_type_qualifier
  ]

type c_declaration_specifier =
  [ `CStorageClassSpecifier of c_storage_class_specifier
  | `CTypeSpecifier of c_type_specifier
  | `CTypeQualifier of c_type_qualifier
  ]

type c_declarator

val c_identifier_declarator : c_identifier -> c_declarator

type c_parameter_declaration

val c_parameter_declaration
  :  c_declaration_specifier list * c_declarator option
  -> c_parameter_declaration

val c_array_declarator : c_identifier * int -> c_declarator

val c_function_declarator : c_identifier * c_parameter_declaration list -> c_declarator

type c_struct_declaration

val c_struct_declaration
  :  c_specifier_qualifier list * c_declarator
  -> c_struct_declaration

val c_struct_specifier
  :  c_identifier * c_struct_declaration list option
  -> [> `CTypeSpecifier of c_type_specifier ]

type c_enumerator

val c_enumerator : c_identifier * int option -> c_enumerator

val c_enum_specifier
  :  c_identifier * c_enumerator list
  -> [> `CTypeSpecifier of c_type_specifier ]

type c_expression

val c_integer_constant_expression : Checked_oint.generic -> c_expression

val c_identifier_expression : c_identifier -> c_expression

val c_string_literal : string -> c_expression

val c_array_subscript : c_expression * c_expression -> c_expression

val c_function_call : c_identifier * c_expression list -> c_expression

val c_struct_member : c_expression * c_identifier -> c_expression

val c_cast_expression : c_specifier_qualifier list * c_expression -> c_expression

val c_assignment_expression : c_expression * c_expression -> c_expression

type c_statement

val c_initialization_declaration
  :  c_declaration_specifier list * c_declarator * c_expression option
  -> c_statement

val c_case_statement : c_expression * c_statement -> c_statement

val c_default_statement : c_statement -> c_statement

val c_compound_statement : c_statement list -> c_statement

val c_expression_statement : c_expression -> c_statement

val c_switch_statement : c_expression * c_statement -> c_statement

val c_break_statement : c_statement

val c_return_statement : c_expression -> c_statement

val c_statement_expression : c_statement list -> c_expression

type c_external_declaration

val c_declaration : c_declaration_specifier list -> c_external_declaration

val c_function_prototype
  :  c_declaration_specifier list * c_identifier * c_parameter_declaration list
  -> c_external_declaration

val c_function_definition
  :  c_declaration_specifier list
     * c_identifier
     * c_parameter_declaration list
     * c_statement list
  -> c_external_declaration

val c_include : [ `Angles of string | `Quotes of string ] -> c_external_declaration

val c_expand : c_identifier * c_expression list -> c_external_declaration

val c_translation_unit : oc:out_channel -> c_external_declaration list -> unit
