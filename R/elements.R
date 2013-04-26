#' Functions for creating HTML elements. These are all wrappers around the html function
#'



#' @name javascript.tag
#' @description Wraps the script string in script tags and a CDATA section
#' @examples
#' javascript.tag("Some javascript")
javascript.tag <- function(script){
    m("script", "//<![CDATA[\n", script, "\n//]]>")
}

#' @name link.to
#' @description Wraps content in an HTML hyperlink with the supplied URL
#' @examples
#' link.to("www.google.com", "Google")
link.to <- function(url, ..., opts = list()){
    m("a", ..., opts = c(opts, list(href = url)))
}

#' @name mail.to
#' @description {Wraps content in html hyperlink with the supplied email address.  
#' If no content provided the email address is supplied as content}
#' @examples
#' #mail.to("me@me.com")
#' #mail.to("me@me.com", "email me")
#' #mail.to("me@me.com", "email me", subject = "you are great")
mail.to <- function(email, ..., subject = "", opts = list()){
    content <- paste0(..., collapse = "")
    if(nchar(content)){
        if(nchar(subject)){
            m("a", ..., opts = c(list(href = paste0("mailto:", 
                                                       email,"?Subject=",
                                                       str_replace_all(subject,"[[:space:]]", "%20" ))), 
                                    opts))
        } else m("a", ..., opts = c(list(href = paste0("mailto:",  email)), 
                                       opts))
    } else m("a", email, opts = c(list(href = paste0("mailto:",  email)), 
                                     opts))
}

#' @name unordered.list
#' @description Wraps a list of strings into an unordered list
#' @examples
#' elements = list("apples", "oranges", "bananas")
#' unordered.list(elements)
unordered.list <- function(elements, opts = list(), element.opts = list()){
    m("ul", lapply(elements, function(x) m("li", x, opts = element.opts)), opts)
}

#' @name ordered.list
#' @description Wraps a list of strings into an ordered list
#' @examples
#' elements = list("apples", "oranges", "bananas")
#' ordered.list(elements)
ordered.list <- function(elements, opts = list(), element.opts = list()){
    m("ol", lapply(elements, function(x) m("li", x, opts = element.opts)), opts)
}

#' @name image.link
#' @description Builds an image element
#' @examples image.link("www.beautifulthings.com/12538675", opts = list(alt = "A lovely picture of something"))
image.link <- function(uri, opts = list()){
    m("img", opts = c(list(src = uri), opts))
}

#' @name content
#' @description Seqentially concatenates strings separated by newlines
content <- function(...){
    paste0(..., sep = "\n", collapse = "\n")
}


# need meta tags : just opening and closing <>, no <\meta>
