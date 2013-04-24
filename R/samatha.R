require(stringr)

#' samatha
#'
#' @name html
#' @description Render arguments to a string of HTML.
#' @examples
#' html("p")
#' #' # Tags can be nested inside of tags and everything ends up concatenated
#' html("p", "Goodbye", html("strong", "cruel"), "world")
#' # Any strings after that become the content of the tag
#' html("p", "This is a paragraph")
#' # You can specify attributes by supplying a list after the content
#' html("span", list(class = "foo"), "bar")
#' #' There are CSS-style shortcuts for setting ID and class
#' html("p#my-p", html("span.pretty", "hey"))
#' html("span", list(id = "foo", class = "bar"), "baz")
#' # You can escape a string using the (escape-html) function
#' "<p>&lt;script&gt;Do something evil&lt;/script&gt;</p>"
html <- function(tag, ..., opts = list(), specials = list(id = "#", class = "\\."), escape.html.p = FALSE){
    for(i in 1:length(specials)){
        tag <- str_split(tag, specials[[i]], 2)[[1]]
        if(length(tag) > 1){
            opts[[names(specials)[i]]] <- tag[2]
            tag <- tag[1]
        }
    }
    content <- paste0(..., collapse = "")
    if(!length(opts) & !nchar(content)){
        sprintf("<%s />", tag)
    } else {
        if(escape.html.p) {
            content <- escape.html(sprintf("<%s%s>%s</%s>", tag, render.opts(opts), content, tag))
        } else sprintf("<%s%s>%s</%s>", tag, render.opts(opts), content, tag)   
    }
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

