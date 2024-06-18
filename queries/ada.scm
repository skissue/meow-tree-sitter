;; Support for high-level text objects selections.
;;
;; For Textobject queries explaination, check out link below:
;; https://docs.helix-editor.com/master/guides/textobject.html

(subprogram_body) @function.around
(subprogram_body (non_empty_declarative_part) @function.inside)
(subprogram_body (handled_sequence_of_statements) @function.inside)
(function_specification) @function.around
(procedure_specification) @function.around
(package_declaration) @function.around
(generic_package_declaration) @function.around
(package_body) @function.around
