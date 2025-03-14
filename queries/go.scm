(function_declaration
  body: (block)? @function.inside) @function.around

(func_literal
  (_)? @function.inside) @function.around

(method_declaration
  body: (block)? @function.inside) @function.around

;; struct and interface declaration as class textobject?
(type_declaration
  (type_spec
    type: (struct_type
      (field_declaration_list) @class.inner))) @class.around

(struct_type
  (field_declaration_list) @class.inner) @class.around

(type_declaration
  (type_spec
   type: (struct_type
          (field_declaration_list
           (field_declaration
            type: (function_type) @function.inner))))) @function.around
 
 (type_parameter_list
   ((_) @parameter.inside . ","? @parameter.around) @parameter.around)

(type_parameter_list
  ((_) @parameter.inside . ","? @parameter.around) @parameter.around)

(parameter_list
  ((_) @parameter.inside . ","? @parameter.around) @parameter.around)

(argument_list
  ((_) @parameter.inside . ","? @parameter.around) @parameter.around)

(comment) @comment.inside

(comment)+ @comment.around

((function_declaration
   name: (identifier) @_name
   body: (block)? @test.inside) @test.around
 (#match "^Test" @_name))
