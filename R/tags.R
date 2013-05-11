

extract.tags <- function(md.file){
    tags <- readLines(md.file, n = 1)
    if(str_detect(tags, "^%")){
        tags <- str_split(tags, "[[:space:],;%]+")[[1]]
        tags[sapply(tags, nchar) > 0]
    } else NULL
}

extract.title <- function(md.file){
    f <- readChar(md.file, n = file.info(md.file)$size)
    str_match(f, "(\n)([^\n.]+)(\n={3,})")[3]
}


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

write.tags.to.file <- function(site){
    tags <- collate.tags(file.path(site, "template/posts"))
    cat(toJSON(tags), file = file.path(site, basename(site), "tags/tags.json"))
}

