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
#' @param title character string
#' @param \dots optional tag body
#' @param opts optional html attributes
#' @seealso {
#' m for generating generic html
#' html_body for wrapping input in body tags
#' }
#' @export
#' @examples html_head("My first page")
html_head <- function(title, ..., opts = list()){
    m("head", m("title", title), ..., opts = opts)
}

#' Wraps input in body tags
#' @name html_body
#' @description Build an html body
#' @param \dots optional tag body
#' @param opts optional html attributes
#' @export
#' @seealso {
#' m for generating generic html
#' html_head for wrapping input in head tags
#' }
#' @examples html_body("Hello world!")
html_body <- function(..., opts = list()){
    m("body", ..., opts = opts)
}

#' Representation of a webpage in R
#' @name webdoc
#' @description Build a web document with the appropriate doctype
#' @export
#' @param doctype a doctype header, e.g. one of the elements of the list doctype
#' @param \dots the contents of the webpage, typically a call to html_head() and a call to html_body() 
#' @return an object of class Samatha.Webdoc which is a character vector of length 1
#' @examples {
#' elements = list("apples", "oranges", "bananas")
#' webdoc("html5",html_head("My first page"),html_body("Hello world", unordered.list(elements)))
#' }
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
#' @param stylesheets a character vector of css files (all css files should be stored in site/basename(site)/css)
#' @return character vector of length 1
#' @examples include.css(c("mysheeet.css", "sheet2.css", "sheet3.css"))
#' @seealso include.js
include.css <- function(stylesheets){
    paste0(lapply(stylesheets, 
                  function(style) m("link", opts = list(type = "text/css",
                                                           href = style,
                                                           rel = "stylesheet"))),
           collapse = "\n")
}

#' js include representation
#' @name include.js
#' @description Include a list of external javascript files
#' @export
#' @param scripts a character vector of js files (all js files should be stored in site/basename(site)/js)
#' @examples include.js(c("script1.js", "script2.js", "script3.js"))
#' @return character vector of length 1
#' @seealso include.css
include.js <- function(scripts){
    paste0(lapply(scripts, 
                  function(script) m("script", opts = list(type = "text/javascript",
                                                           src = script))),
           collapse = "\n")
}




