;; function definition
(binary_operator
  operator: "<-"
  rhs: (function_definition
         body: (braced_expression) @function.inside)) @function.around

(call
  function: (identifier) @_name
  arguments:
    (arguments) @class.inside
  (#equal @_name "setClass")) @class.around

;; testthat tests
(call
  function: (identifier) @_name
  arguments:
    (arguments
      (argument
        value: (braced_expression) @test.inside))
  (#equal @_name "test_that")) @test.around

;; List entry
(argument
  name: (identifier)) @entry.inside @entry.around

;; Parameters and arguments
[(parameter) (argument)] @parameter.inside @parameter.around

;; Comments
(comment) @comment.inside
(comment)+ @comment.around
