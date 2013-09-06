

#' Builds a list of post tags and links to their associated posts
#' Extracts tags from the top of all .md files in a list of posts
#' @name collate.tags
#' @param posts a character vector of names of posts
#' @return a list of tag names, each with associated vectors of titles and urls for associated posts
collate.tags <- function(posts){
    taglist <- list()
    postlist <- list.files(posts, recursive = TRUE)
    postlist <- postlist[str_detect(postlist, "\\.md$")]
    for(f in postlist){
        fname <- str_match(f, pattern = "([[:digit:]]{4}_[[:digit:]]{2}_[[:digit:]]{2})_(.*)")
        tags <- extract.tags(file.path(posts, f))
        title <- extract.title(file.path(posts, f))
        postname <- str_match(f, pattern = "([[:digit:]]{4}_[[:digit:]]{2}_[[:digit:]]{2})_(.*)")
        for(tag in tags){
            if(is.null(taglist[[tag]])){
                taglist[[tag]] <- list()
                taglist[[tag]]$titles <- title
                taglist[[tag]]$urls <- file.path("posts", 
                                                 str_replace(str_extract(fname[2],
                                                                         "[[:digit:]]{4}_[[:digit:]]{2}"), 
                                                             "_", "/"),
                                                 str_replace(fname[3], "\\.md", "\\.html"))
            } else {
                taglist[[tag]]$titles <- c(taglist[[tag]]$titles, title)
                taglist[[tag]]$urls <- c(taglist[[tag]]$urls, 
                                         file.path("posts", 
                                                   str_replace(str_extract(fname[2],
                                                                           "[[:digit:]]{4}_[[:digit:]]{2}"), 
                                                               "_", "/"),
                                                   str_replace(fname[3], "\\.md", "\\.html")))
            }
        }
    }
    taglist
}

#' writes a json file of post tags and links to their associated posts
#' Seperate from collate.tags to maintain functional style
#' @name write.tags.to.file
#' @param site Path to the Samatha site
#' @return NULL writes to file as a side effect
#' @seealso collate.tags
write.tags.to.file <- function(site){
    tags <- collate.tags(file.path(site, "template/posts"))
    dir.create(file.path(site, "template/resources/json"), showWarnings = FALSE)
    cat(toJSON(tags), file = file.path(site, "template/resources/json/tags.json"))
}

#' Generates page content for a tag, to be rendered with a layout
#' @name build.tagpage
#' @param tag list element with associated vectors of titles and urls for associated posts
#' @param tagname name of the tag
#' @seealso collate.tags
#' @return character vector of length 1 containing html for an unordered list of links to posts associated with the tag
build.tagpage <- function(tag, tagname){
    taglinks <- lapply(1:length(tag$titles), 
                       function(x) link.to(url = paste0("/", tag$urls[[x]]), tag$titles[[x]]))
    content(m("h1", paste0("Posts about ", tagname,":")),
            unordered.list(taglinks))
}

#' Reads in a  JSON tagfile
#' @name import.tagfile
#' @param tagfile path to JSON tag list file
#' @return list of tags and associated vectors of posts and links, or NULL if file doesn't exist
import.tagfile <- function(tagfile){
    if(file.exists(tagfile)){
        return(fromJSON(readChar(tagfile, nchars = file.info(tagfile)$size), simplify = FALSE))
    } else {
        cat(sprintf("No tagfile at %s\n", tagfile))
        return(NULL)
    }
}
    
#' Renders new html pages listing the associated posts for each tag
#' loops over elements of a taglist, building pages for each tag
#' @name render.tagfiles
#' @param site the absolute path to the Samatha site
#' @param tag.layout name of the layout file to be used to render tags
#' @return NULL writes to files as a side effect
render.tagfiles <- function(site, tag.layout){
    tagfile <- file.path(site, "template/resources/json/tags.json")
    taglist <- import.tagfile(tagfile)
    if(!is.null(taglist)){
        for(tag in 1:length(taglist)){
            page <- build.tagpage(taglist[[tag]], names(taglist)[tag])
            cat(source(file.path(site, "template/layouts", tag.layout), local = TRUE)$value, 
                file = file.path(site, 
                                 basename(site), "tags",
                                 paste0(names(taglist)[tag], ".html")))
        }
    }
}

#' returns a character string of an html formatted list of tags, 
#' with numbers of posts associated with each and links to the tag pages
#' This is a useful addition to an index page
#' @name html.taglist
#' @param site the absolute path to the Samatha site
#' @return {
#' character string representation of an unordered html list of tags, 
#' with numbers of posts associated with each and links to the tag pages
#' }
#' @export
html.taglist <- function(site){
    tagfile <- file.path(site, "template/resources/json/tags.json")
    taglist <- import.tagfile(tagfile)
    if(!is.null(taglist)){
        tagslinks <- lapply(1:length(taglist),
                            function(x) link.to(url = file.path("/tags",
                                                                paste0(names(taglist)[x], ".html")),
                                                sprintf("%s (%d)", names(taglist)[x], length(taglist[[x]]$urls))))
        unordered.list(tagslinks)
    }
} 


