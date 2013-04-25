#' Functions for creating HTML elements
#' 
#' @name javascript.tag
#' @description Wraps the script string in script tags and a CDATA section
javascript.tag <- function(script){
    html("script", "//<![CDATA[\n", script, "\n//]]>")
}