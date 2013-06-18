#' Extracts the tag names from a markdown post file
#' tags should be the first line of the file and preceeded with the percent sign
#' tags are delimited by spaces, commas or semicolons
#' @name extract.tags
#' @param md.file .md file of a post
#' @return list of tag names
#' @seealso extract.title
extract.tags <- function(md.file){
    tags <- readLines(md.file, n = 1)
    if(str_detect(tags, "^%")){
        tags <- str_split(tags, "[[:space:],;%]+")[[1]]
        tags[sapply(tags, nchar) > 0]
    } else NULL
}

#' reads a markdown post file and extracts the title
#' Title is taken as the first line in which the line underneath is a double underline
#' i.e. a h1 tag in markdown
#' n.b. the title must be in this format, not e.g. # This is a title
#' @name extract.title
#' @param md.file .md file of a post
#' @return character sting of the title of the post
#' @seealso extract.tags
extract.title <- function(md.file){
    f <- readChar(md.file, n = file.info(md.file)$size)
    str_match(f, "(\n)([^\n.]+)(\n={3,})")[3]
}

#' returns the path to the post in the site
#' @name get.postpath
#' @param post path to the post source file
#' @return path to the html file corresponding to the post
#' @export
get.postpath <- function(post){
    postnames <- str_match(post, pattern = "([[:digit:]]{4}_[[:digit:]]{2}_[[:digit:]]{2})_(.*)")
    month.dir <- file.path("/posts", 
                           str_replace(str_extract(postnames[2], 
                                                   "[[:digit:]]{4}_[[:digit:]]{2}"), 
                                       "_", "/"))
    file.path(month.dir, 
              str_replace(postnames[3], "\\.md", "\\.html"))
}

#' builds a list of links to all blog posts. List is in decreasing order of post date
#' @name html.postlist
#' @param site absolute path to the Samatha site
#' @return character vector representing an unordered html list of links to blog posts in descending date order 
#' @export
html.postlist <- function(site){
    postlist <- list.files(file.path(site, "template/posts"))
    postlist <- postlist[str_detect(postlist, "\\.md")]
    postdates <- as.Date(str_extract(postlist, "[[:digit:]]{4}_[[:digit:]]{2}_[[:digit:]]{2}"), 
                              format = "%Y_%m_%d")
    postlist <- postlist[order(postdates, decreasing = TRUE)]
    postdates <- postdates[order(postdates, decreasing = TRUE)]
    posttitles <- lapply(postlist, function(x) extract.title(file.path(site, "template/posts",x)))
    postpaths <- lapply(postlist, get.postpath)
    if(!is.null(postlist)){
        postlinks <- lapply(1:length(postlist),
                            function(x) link.to(url = postpaths[[x]],
                                                sprintf("%s (%s)", posttitles[[x]], postdates[[x]])))
        unordered.list(postlinks)
    }
} 

#' renders the contents of a markdown file as an html character vector
#' A wrapper around markdown::markdownToHTML
#' @name include.markdown
#' @param md.file a post file in markdown format
#' @return a character vector of length 1 of rendered html
#' @seealso include.textfile
include.markdown <- function(md.file){
    markdownToHTML(file=md.file, fragment.only = TRUE)
}

#' reads a text file into a character vector
#' A wrapper around readChar
#' @name include.textfile
#' @param text.file a text file you wish to include in an html page, e.g. a javascript function
#' @return a character vector of length 1
#' @seealso include.markdown
include.textfile <- function(text.file){
    readChar(text.file, nchars = file.info(text.file)$size)
}




