#' @title Render a page using the Samatha html dsl
#' @name render.page
#' @description Renders a page according to its layout template
#' pages are stored in site/template/pages
#' @export
render.page <- function(site, pagename){
    source(file.path(site, "template/pages", pagename), local = TRUE)
    page.obj <- structure(list(html = source(file.path(site, "template/layouts", layout), local = TRUE)$value,
                               content = page,
                               layout = layout,
                               file = file.path(site, 
                                                basename(site), 
                                                str_replace(pagename, "\\.R", "\\.html")),
                               title = title,
                               sourcefile = pagename,
                               tags = ""),
                          class = "Samatha.Page")
    page.obj
} 

#' Render a post from an R markdown file
#' @name render.post
#' @description Render an .Rmd file into a page according to its layout template
#' post templates are stored in site/template
#' Better date functionality
#' @export
# render.post(site, "2013_04_16_scraping_metadata.Rmd")
render.post <- function(site, postname, layout = "default.R", fig.path = "img"){
    postnames <- str_match(postname, pattern = "([[:digit:]]{4}_[[:digit:]]{2}_[[:digit:]]{2})_(.*)")
    md.file <- file.path(site, "template/posts",str_replace(postnames[1], "\\.Rmd", "\\.md"))
    if(length(postnames) != 3 | ! str_detect(postnames[3], "\\.Rmd")){
        cat(sprintf("Bad post filename: %s", postnames[1]))
        return(FALSE)
    } 
    opts_chunk$set(fig.path = file.path(site, basename(site), paste0(fig.path, "/")))
    knit(input = file.path(site, "template/posts", postnames[1]),
         output = md.file)
    page <- markdownToHTML(md.file, fragment.only = TRUE)
    page <- paste0(page, m("h6", sprintf("Posted on %s", as.Date(postnames[2], format("%Y_%m_%d")))))
    page <- str_replace_all(page, 
                            paste0("img src=\"",file.path(site, basename(site), fig.path)), 
                            paste0("img src=\"/", fig.path))
    month.dir <- file.path(site, basename(site), "posts", 
                           str_replace(str_extract(postnames[2], 
                                                   "[[:digit:]]{4}_[[:digit:]]{2}"), 
                                       "_", "/"))
    dir.create(month.dir, showWarnings = FALSE, recursive = TRUE)
    post.obj <- structure(list(html = source(file.path(site, "template/layouts", layout), local = TRUE)$value,
                               content = page,
                               layout = layout,
                               file = file.path(month.dir, 
                                                str_replace(postnames[3], "\\.Rmd", "\\.html")),
                               title = extract.title(md.file),
                               sourcefile = postname,
                               tags = extract.tags(md.file)),
                          class = "Samatha.Page")
    post.obj
}

#' Writes the html content of a Samatha.Page object to 
#' the file specified in the file element
#' @name write.html
write.html <- function(samatha.page){
    if(class(samatha.page) == "Samatha.Page"){
        cat(samatha.page$html, 
            file = samatha.page$file)
        TRUE
    } else {
        cat("Not a valid Samatha.Page object")
        FALSE
    }
}


site <- "/home/mdehsds4/github/blogtest/"
pagename <- "index.R"



#' complete rewrite of the samatha engine:
#' render.post and render.page now return samatha.page objects. DONE.
#' write function takes a samatha.page object and writes it as an html file. DONE.
#' get.site.state fnc DONE.
#' samatha replaces samatha.engine
#'  - reburns rss and tags



#' Gets modification times for a vector of files
#' @name file.status
file.states <- function(files){
    setNames(file.info(files)$mtime, files)
}

#' Gets modification dates for all source and dest files in a site
#' @name get.site.name
get.site.state <- function(site){
    setNames(lapply(c("template/layouts", "template/pages", "template/posts", 
             file.path(basename(site), "pages"), file.path(basename(site), "posts")), 
           function(x){
               if(x == file.path(basename(site), "pages")){
                   upper <- list.files(file.path(site), full.names = TRUE)
                   upper <- upper[str_detect(upper, "\\.html$")]
                   c(file.states(upper), 
                     file.states(list.files(file.path(site, x), 
                                          recursive = TRUE, full.names = TRUE)))
               } else if(x == "template/posts"){
                   posts <- list.files(file.path(site, x), 
                                       recursive = TRUE, full.names = TRUE)
                   posts <- posts[str_detect(posts, "\\.Rmd$")]
                   file.states(posts)
               } else {
                   file.states(list.files(file.path(site, x), 
                                          recursive = TRUE, full.names = TRUE))
               }
           }),
             c("layouts", "source_pages", "source_posts", "dest_pages", "dest_posts"))
}

#' Checks if source files were modified after the corresponding dest files
#' if :
#'  - source newer than html file : rebuild file
#'  - layouts newer than any html files : rebuild everything - DONE
#'  - no corresponding html for source : build
#'  - html files with no source : delete html
#'  - otherwise do nothing
#'  Building must have error testing
 
#' 
#' @name update.site
update.site <- function(site, site.state, post.layout, tag.layout, fig.path){
    updated <- FALSE
    sp <- str_replace(as.character(sapply(names(site.state$source_pages), 
                              function(x) basename(x))),
                      "\\.R", "\\.html") # source pages
    dp <- as.character(sapply(names(site.state$dest_pages), 
                              function(x) basename(x))) # dest pages
    sb <- str_replace(str_replace(as.character(sapply(names(site.state$source_posts), 
                              function(x) basename(x))), 
                                  "\\.Rmd", "\\.html"),
                      "^[[:digit:]]{4}_[[:digit:]]{2}_[[:digit:]]{2}_", "") # source pages
    db <- as.character(sapply(names(site.state$dest_posts), 
                              function(x) basename(x))) # dest pages
    for(layout in site.state$layouts){
        # any layout changes - rebuild whole site
        if(any(layout > c(site.state$dest_pages, site.state$dest_posts))){
            for(post in site.state$source_posts) {
                render.post(site, basename(post), layout = post.layout, fig.path = figure.path)
            }
            for(page in site.states$dest_pages{
                render.page(site, page) ### should be source_pages... cbb
                # get correct paths for pages:
                sapply(str_split(names(a$dest_pages), 
                                 paste0(basename(site), "//?")), 
                       function(x) x[length(x)])
            }
        updated <- TRUE
        }
    }
    for(source %in% site.state$source_pages[!sp %in% dp])
}

sp <- as.character(sapply(names(site.state$source_pages), function(x) str_replace(basename(x), "\\.R", "\\.html")))
dp <- as.character(sapply(names(site.state$dest_pages), function(x) basename(x)))

#' Samatha: Runs an infinite loop, updating the site as necessary
#' @name samatha
samatha <- function(site, domain, 
                    post.layout = "default.R", 
                    tag.layout = "default.R", 
                    figure.path = "img"){
    while(TRUE){
        site.state <- get.site.state(site)
        post.checker(site.info, domain, 
                     post.layout, tag.layout, figure.path)
        Sys.sleep(1)
    }
}

#' Watches the site directory for changes and recompiles html appropriately
#' @name samatha.engine
samatha.engine <- function(site, domain, post.layout = "default.R", figure.path = "img"){
    cat("Running Samatha static site engine.\n")
    posts <- list.files(file.path(site, "template/posts"))
    pages <- list.files(file.path(site, "template/pages"), recursive = TRUE)
    for(post in posts[str_detect(posts, "Rmd$")]) {
        render.post(site, post, layout = post.layout, fig.path = figure.path)
    }
    for(page in pages[str_detect(pages, "R$")]) {
        render.page(site, page)
    }
    write.tags.to.file(site)
    render.tagfiles(site)
    render.rss(site, domain)
    samatha.watch(path = file.path(site, "template"), site.watcher)
}


site.watcher <- function(added, deleted, modified){
    changed <- c(added, modified)
    changed <- changed[str_detect(changed, "\\.R(md)?$")]
    deleted <- deleted[str_detect(deleted, "\\.R(md)?$")]
    if(length(deleted)){
        cat("Flushing posts and pages directories of html files...\n")
        unlink(file.path(site, basename(site), "posts/*.html"), recursive = TRUE)
        unlink(file.path(site, basename(site), "pages/*.html"), recursive = FALSE)
        unlink(file.path(site, basename(site), "*.html"), recursive = FALSE)
        cat("changed:", changed, "\n")
        for(post in changed[str_detect(changed, "posts/.+\\.Rmd$")]) {
            render.post(site, basename(post), layout = post.layout, fig.path = figure.path)
        }
        for(page in changed[str_detect(changed, "pages/.+R$")]) {
            render.page(site, page)
        }
        write.tags.to.file(site)
        render.tagfiles(site)
        render.rss(site, domain)
    }
    if(length(changed)){
        cat("changed:", changed, "\n")
        for(post in changed[str_detect(changed, "posts/.+\\.Rmd$")]) {
            render.post(site, basename(post), layout = post.layout, fig.path = figure.path)
        }
        for(page in changed[str_detect(changed, "pages/.+R$")]) {
            render.page(site, page)
        }
        write.tags.to.file(site)
        render.tagfiles(site)
        render.rss(site, domain)
    }
    TRUE
}

