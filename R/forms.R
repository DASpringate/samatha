#' Samatha forms
#' Functions for building elements of html forms

#' incomplete - need to look at name parameter...
#' @name input.field
#' @description Creates a new input field
#' @examples
#' input.field("fuzzy", "hello", 12)
input.field <- function(type, name, value){
    html("input", opts = list(type = type,
                              name = name,
                              value = value))
}