#' Samatha pages
#' Functions for wrapping html in doctype boilerplate
#' 


#' @name include.css
#' @description Include a list of external stylesheet files
#' @examples 
#' include.css(c("mysheeet.css", "sheet2.css", "sheet3.css"))
include.css <- function(stylesheets){
    paste0(lapply(stylesheets, 
                  function(style) html("link", opts = list(type = "text/css",
                                                           href = style,
                                                           rel = "stylesheet"))),
           collapse = "\n")
}

#' @name include.js
#' @description Include a list of external javascript files
#' @examples 
#' include.js(c("script1.js", "script2.js", "script3.js"))
include.js <- function(scripts){
    paste0(lapply(scripts, 
                  function(script) html("script", opts = list(type = "text/javascript",
                                                           src = script))),
           collapse = "\n")
}




