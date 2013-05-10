#' Samatha pages
#' Functions for wrapping html in doctype boilerplate
#' 

#' html doctype header represntation
#' @name doctype
#' @description List of document type declarations for html and xml
doctypes <- list(html4 = list(paste("<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01//EN\"",
                              "\"http://www.w3.org/TR/html4/strict.dtd\">"), 
                             list(tag = "html", opts = list())),
                xhtml.strict = list(paste("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"",
                                     "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"), 
                                    list(tag = "html", opts = list())),
                xhtml.transitional = list(paste("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"",
                                           "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">"), 
                                          list(tag = "html", opts =  list())),
                html5 =  list("<!DOCTYPE html>", list(tag = "html", opts = list())))

#' Wraps input in head tags
#' @name html_head
#' @description Build an html header
#' @export
#' @examples
#' html_head("My first page")
html_head <- function(title, ..., opts = list()){
    m("head", ..., opts = c(list(title = title), opts))
}

#' Wraps input in body tags
#' @name html_body
#' @description Build an html body
#' @export
#' @examples 
#' html_body("Hello world!")
html_body <- function(..., opts = list()){
    m("body", ..., opts = opts)
}

#' Representation of a webpage in R
#' @name webdoc
#' @description Build a web document with the appropriate doctype
#' @export
#' @examples
#' elements = list("apples", "oranges", "bananas")
#' webdoc("html5",html_head("My first page"),html_body("Hello world"), unordered.list(elements))
webdoc <- function(doctype, ...){
    content <- m(doctypes[[doctype]][[2]]$tag, paste0(..., collapse = ""), 
                    opts = doctypes[[2]]$opts)
    outdoc <- sprintf("%s%s", doctypes[[doctype]][[1]], content)
    structure(outdoc, class="Samatha.Webdoc")
    outdoc
}

#' CSS include representation
#' @name include.css
#' @description Include a list of external stylesheet files
#' @export
#' @examples 
#' include.css(c("mysheeet.css", "sheet2.css", "sheet3.css"))
include.css <- function(stylesheets){
    paste0(lapply(stylesheets, 
                  function(style) m("link", opts = list(type = "text/css",
                                                           href = style,
                                                           rel = "stylesheet"))),
           collapse = "\n")
}

#' js include represntation
#' @name include.js
#' @description Include a list of external javascript files
#' @export
#' @examples 
#' include.js(c("script1.js", "script2.js", "script3.js"))
include.js <- function(scripts){
    paste0(lapply(scripts, 
                  function(script) m("script", opts = list(type = "text/javascript",
                                                           src = script))),
           collapse = "\n")
}



