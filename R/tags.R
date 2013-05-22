

#' Builds a list of post tags and links to their associated posts
#' @name collate.tags
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
#' @name write.tags.to.file
write.tags.to.file <- function(site){
    tags <- collate.tags(file.path(site, "template/posts"))
    cat(toJSON(tags), file = file.path(site, "template/resources/json/tags.json"))
}

#' Generates page content for a tag, to be rendered with a layout
#' @name build.tagpage
build.tagpage <- function(tag, tagname){
    taglinks <- lapply(1:length(tag$titles), 
                       function(x) link.to(url = paste0("/", tag$urls[[x]]), tag$titles[[x]]))
    content(m("h1", paste0("Posts about ", tagname,":")),
            unordered.list(taglinks))
}

#' Reads in a  JSON tagfile
#' @name import.tagfile
import.tagfile <- function(tagfile){
    if(file.exists(tagfile)){
        return(fromJSON(readChar(tagfile, n = file.info(tagfile)$size), simplify = FALSE))
    } else {
        cat(sprintf("No tagfile at %s\n", tagfile))
        return(NULL)
    }
}
    
#' Renders new html pages listing the associated posts for each tag
#' @name render.tagfiles
render.tagfiles <- function(site){
    source(file.path(site, "template/layouts/tags.R"), local = TRUE)
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


