#' Functions for creating HTML elements. These are all wrappers around the m() function


#' javascript tag representation
#' @name javascript.tag
#' @description Wraps the script string in script tags and a CDATA section
#' @param script character string representation of javascript code
#' @export
#' @return character
#' @examples javascript.tag("Some javascript")
javascript.tag <- function(script){
    m("script", "//<![CDATA[\n", script, "\n//]]>")
}

#' link tag representaion
#' @name link.to
#' @description Wraps content in an HTML hyperlink with the supplied URL
#' @param url character the url the link points to
#' @param \dots the content of the tag
#' @param opts list of optional tag attributes
#' @export
#' @return character
#' @examples link.to("www.google.com", "Google")
link.to <- function(url, ..., opts = list()){
    m("a", ..., opts = c(opts, list(href = url)))
}

#' mail to tag representaion
#' @name mail.to
#' @description {
#' Wraps content in html hyperlink with the supplied email address.  
#' If no content provided the email address is supplied as content
#' }
#' @param email character an email address
#' @param subject character subject heading for the email
#' @param \dots character vector The body of the email
#' @param opts list of optional tag attributes
#' @export
#' @return character
#' @examples \dontrun{
#' # can't use ampersand in roxygen2 comments! 
#' mail.to("me.at.me.com")
#' mail.to("me.at.me.com", "email me")
#' mail.to("me.at.me.com", "email me", subject = "you are great")
#' }
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

#' unordered list representation
#' @name unordered.list
#' @description Wraps a list of strings into an unordered list
#' @param elements list of elements matching the elements of the html unordered list
#' @param list.opts list of optional attributes to be applied to the <ul> tag
#' @param element.opts list of optional attributes to be applied to each <li> tag in the list
#' @export
#' @return character
#' @examples {
#' elements = list("apples", "oranges", "bananas")
#' unordered.list(elements)
#' }
unordered.list <- function(elements, list.opts = list(), element.opts = list()){
    m("ul", lapply(elements, function(x) m("li", x, opts = element.opts)), opts = list.opts)
}

#' ordered list representation
#' @name ordered.list
#' @description Wraps a list of strings into an ordered list
#' @param elements list of elements matching the elements of the html ordered list
#' @param list.opts list of optional attributes to be applied to the <ol> tag
#' @param element.opts list of optional attributes to be applied to each <li> tag in the list
#' @export
#' @return character
#' @examples {
#' elements = list("apples", "oranges", "bananas")
#' ordered.list(elements)
#' }
ordered.list <- function(elements, list.opts = list(), element.opts = list()){
    m("ol", lapply(elements, function(x) m("li", x, opts = element.opts)), opts = list.opts)
}

#' image tag representaion
#' @name img.link
#' @description Builds an image element
#' @param uri character link to the image
#' @param opts list of optional tag attributes
#' @export
#' @return character
#' @examples img.link("www.beautifulthings.com/12538675", opts = list(alt = "A lovely picture of something"))
img.link <- function(uri, opts = list()){
    m("img", opts = c(list(src = uri), opts))
}

#' concatenates html tags
#' @name content
#' @description Seqentially concatenates strings separated by newlines
#' @param \dots list or character vector of items to be concatenated
#' @export
#' @return character
#' @examples content(m("article", "article 1"), m("h1", "title"), m("section", "this is a section"))
content <- function(...){
    paste0(..., sep = "\n", collapse = "\n")
}


# need meta tags : just opening and closing <>, no <\meta>
