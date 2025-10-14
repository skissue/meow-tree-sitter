(function_definition (_)? @function.inside) @function.around

(arrow_function_expression (_)? @function.inside) @function.around

(macro_definition (_)? @function.inside) @function.around

(struct_definition (_)? @class.inside) @class.around

(abstract_definition (type_head)? @class.inside) @class.around

(primitive_definition (type_head)? @class.inside) @class.around

(argument_list ((_) @parameter.inside) @parameter.around)

(line_comment) @comment.inside
(line_comment)+ @comment.around
(block_comment) @comment.inside
(block_comment)+ @comment.around

(macrocall_expression
  (macro_identifier
    (identifier) @_name
    (#match "^(test|test_throws|test_logs|inferred|test_deprecated|test_warn|test_nowarn|test_broken|test_skip)$" @_name))
  (macro_argument_list) @test.inside) @test.around
