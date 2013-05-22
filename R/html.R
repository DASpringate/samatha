#' A vector of elements that need an explicit endin tag
#' @name container_tags
container_tags <- c("a", "article", "aside", "b", "body", "canvas", "dd", "div", "dl", "dt", "em", "fieldset",
                    "footer", "form", "h1", "h2", "h3", "h4", "h5", "h6", "head", "header", "hgroup", "html",
                    "i", "iframe", "label", "li", "nav", "ol", "option", "pre", "section", "script", "span",
                    "strong", "style", "table", "textarea", "title", "ul")

#' Generic representaion of an HTML tag in R
#' @name m
#' @description Render arguments to a string of markup (HTML but also generic XML)
#' @export
#' @examples
#' m("p")
#' m("div")
#' # Any strings after that become the content of the tag
#' m("p", "This is a paragraph")
#' # Tags can be nested inside of tags and everything ends up concatenated
#' m("p", "Goodbye", m("strong", "cruel"), "world")
#' # You can specify attributes by supplying a list after the content
#' m("span",  "bar", opts = list(class = "foo"))
#' #' There are CSS-style shortcuts for setting ID and class
#' m("p#my-p", m("span.pretty", "hey"))
#' m("span", opts = list(id = "foo", class = "bar"), "baz")
#' # You can escape a string using the (escape-html) function
#' m("p", m("script", "Do something evil", escape.html.p = TRUE))
#' # Also caters for singleton tags:
#' m("meta", opts = list(charset = "utf-8"), singleton = TRUE)
m <- function(tag, ..., opts = list(), specials = list(id = "#", class = "\\."), 
              escape.html.p = FALSE, singleton = FALSE){
    for(i in 1:length(specials)){
        tag <- str_split(tag, specials[[i]], 2)[[1]]
        if(length(tag) > 1){
            opts[[names(specials)[i]]] <- tag[2]
            tag <- tag[1]
        }
    }
    content <- paste0(..., collapse = "")
    if(!nchar(content)){
        if(tag %in% container_tags){
                 sprintf("<%s%s></%s>", tag, render.opts(opts), tag)
        } else{
            if(singleton){
                sprintf("<%s%s>", tag, render.opts(opts))
            } else sprintf("<%s%s />", tag, render.opts(opts))
        } 
    } else {
        if(singleton){
            out <- sprintf("<%s%s%s>", tag, render.opts(opts), content)
        } else {
            out <- sprintf("<%s%s>%s</%s>", tag, render.opts(opts), content, tag)
        }
        if(escape.html.p) {
            escape.html(out)
        } else out  
    }
}

#' Render html attributes
#' @name render.opts
#' @description Helper function to render optional attributes for an html tag in m()
render.opts <- function(opts){
    if(length(opts)){
        paste(lapply(1:length(opts), function(x) sprintf(" %s=\"%s\"", names(opts)[x], opts[[x]])),
              collapse = "")
    } else ""
    
}

#' Escapes html tags
#' @name escape.html
#' @description Escapes html by converting &,<,> and quotes
#' @export
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




