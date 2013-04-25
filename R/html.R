require(stringr)

#' samatha
#'
#' @name html
#' @description Render arguments to a string of HTML.
#' @examples
#' html("p")
#' # Any strings after that become the content of the tag
#' html("p", "This is a paragraph")
#' # Tags can be nested inside of tags and everything ends up concatenated
#' html("p", "Goodbye", html("strong", "cruel"), "world")
#' # You can specify attributes by supplying a list after the content
#' html("span",  "bar", opts = list(class = "foo"))
#' #' There are CSS-style shortcuts for setting ID and class
#' html("p#my-p", html("span.pretty", "hey"))
#' html("span", opts = list(id = "foo", class = "bar"), "baz")
#' # You can escape a string using the (escape-html) function
#' html("p", html("script", "Do something evil", escape.htmp.p = TRUE))
html <- function(tag, ..., opts = list(), specials = list(id = "#", class = "\\."), escape.html.p = FALSE){
    for(i in 1:length(specials)){
        tag <- str_split(tag, specials[[i]], 2)[[1]]
        if(length(tag) > 1){
            opts[[names(specials)[i]]] <- tag[2]
            tag <- tag[1]
        }
    }
    content <- paste0(..., collapse = "")
    if(!nchar(content)){
        sprintf("<%s %s />", tag, render.opts(opts))
    } else {
        if(escape.html.p) {
            content <- escape.html(sprintf("<%s%s>%s</%s>", tag, render.opts(opts), content, tag))
        } else sprintf("<%s%s>%s</%s>", tag, render.opts(opts), content, tag)   
    }
}

render.opts <- function(opts){
    if(length(opts)){
        paste(lapply(1:length(opts), function(x) sprintf(" %s=\"%s\"", names(opts)[x], opts[[x]])),
              collapse = "")
    } else ""
    
}

escape.html <- function(s){
    escapes <- list("&" = "&amp;",
                    "<" = "&lt;",
                    ">" = "&gt;",
                    "\""= "&quot;")
    for(htm in 1:length(escapes)){
        s <- str_replace_all(s, names(escapes)[htm], escapes[[htm]])
    }
    s
}




