#' Samatha pages
#' Functions for wrapping html in doctype boilerplate
#' 

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

#' @name head
#' @description Build an html header
#' @examples
#' head("My first page")
head <- function(title, ..., opts = list()){
    m("head", ..., opts = c(list(title = title), opts))
}

#' @name body
#' @description Build an html body
#' @examples
#' body("Hello world!")
body <- function(..., opts = list()){
    m("body", ..., opts = opts)
}


#' @name webdoc
#' @description Build a web document with the appropriate doctype
#' examples
#' webdoc("html5",
#' head("My first page"),
#' body("Hello world"),
#' unordered.list(elements))

webdoc <- function(doctype, ...){
    content <- m(doctypes[[doctype]][[2]]$tag, paste0(..., collapse = ""), 
                    opts = doctypes[[2]]$opts)
    outdoc <- sprintf("%s%s", doctypes[[doctype]][[1]], content)
    structure(outdoc, class="samatha.webdoc")
    outdoc
}

#' @name include.css
#' @description Include a list of external stylesheet files
#' @examples 
#' include.css(c("mysheeet.css", "sheet2.css", "sheet3.css"))
include.css <- function(stylesheets){
    paste0(lapply(stylesheets, 
                  function(style) m("link", opts = list(type = "text/css",
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
                  function(script) m("script", opts = list(type = "text/javascript",
                                                           src = script))),
           collapse = "\n")
}




