#' Start a new site 
#' @name create.site.structure
#' @description Sets up the directory structure for a new static site
#' @param site path to the directory you want to set up
#' @export
create.site.structure <- function(site){
    new.site.p <- dir.create(site, showWarnings = FALSE)
    if(new.site.p){
        for(d in c("posts", "pages", "css", "img", "js", "resources", "tag")){
            dir.create(file.path(site, basename(site), d), 
                       showWarnings = FALSE, recursive = TRUE)
        }
        for(d in c("layouts", "posts", "pages/pages", "markdown")){
            dir.create(file.path(site, "template", d), 
                       showWarnings = FALSE, recursive = TRUE)
    }
        cat(sprintf("Directory structure set up for %s\n", site))
        TRUE
    } else {
        cat(sprintf("Directory %s already exists\n", site))
        FALSE 
    }
}

#' @title Render a page using the Samatha html dsl
#' @name render.page
#' @description Renders a page according to its layout template
#' pages are stored in site/template/pages
#' @export
render.page <- function(site, pagename){
    source(file.path(site, "template/pages", pagename), local = TRUE)
    cat(source(file.path(site, "template/layouts", layout), local = TRUE)$value, 
               file = file.path(site, 
                                basename(site), 
                                str_replace(pagename, "\\.R", "\\.html")))
} 

#' Render a post from an R markdown file
#' @name render.post
#' @description Render an .Rmd file into a page according to its layout template
#' post templates are stored in site/template
#' Need to eventually fix the file copy of figures to take into account changes in files
#' Better date functionality
#' @export
# render.post("testsite", "2013_05_01_The_first_post.Rmd")
render.post <- function(site, postname, layout = "default.R", fig.path = "img"){
    postnames <- str_match(postname, pattern = "([[:digit:]]{4}_[[:digit:]]{2}_[[:digit:]]{2})_(.*)")
    if(length(postnames) != 3 | ! str_detect(postnames[3], "\\.Rmd")) stop(sprintf("Bad post filename: %s", postnames[1]))
    opts_chunk$set(fig.path = file.path(site, basename(site), paste0(fig.path, "/")))
    knit(input = file.path(site, "template/posts", postnames[1]),
         output = file.path(site, "template/posts", str_replace(postnames[1], "\\.Rmd", "\\.md")))
    #for(f in list.files("img")) file.copy(file.path("img", f), file.path(site, basename(site), "img", f), overwrite = FALSE)
    page <- markdownToHTML(file.path(site, "template/posts", str_replace(postnames[1], "\\.Rmd", "\\.md")),
                                   fragment.only = TRUE)
    page <- paste0(page, m("h6", sprintf("Posted on %s", as.Date(postnames[2], format("%Y_%m_%d")))))
    page <- str_replace_all(page, 
                            paste0("img src=\"",file.path(site, basename(site), fig.path)), 
                            paste0("img src=\"/", fig.path))
    month.dir <- file.path(site, basename(site), "posts", 
                          str_replace(str_extract(postnames[2], 
                                                  "[[:digit:]]{4}_[[:digit:]]{2}"), 
                                      "_", "/"))
    dir.create(month.dir, showWarnings = FALSE, recursive = TRUE)
    cat(source(file.path(site, "template/layouts", layout), local = TRUE)$value, 
        file = file.path(month.dir, 
                         str_replace(postnames[3], "\\.Rmd", "\\.html")))   
}

#' Index the pages of a site REPLACE WITH WATCH FROM TESTTHAT
#' @name index.site
#' @description makes an .RData file of all of the compiled pages in the site with modification dates
#' @export
index.site <- function(site){
    static.files <- list.files(file.path(site, basename(site)), recursive = TRUE)
    index <- lapply(static.files,
                    function(x) file.info(file.path(site,basename(site), x))$mtime)
    names(index) <- static.files
    save(index, file = file.path(site, "template/index.RData"))
}

# site <- "testsite"
# create.site.structure(site)
# render.page(site, "testpage.R")
# render.post(site, postname, layout = "default.R")

# to do:
# Loop renderers for all files in posts and pages
# record modification dates for posts/pages to control looping
# set up local test server (Rook?)
# improve the figure folder stuff (change the base figure file to be inside the template then change the paths to the figure file in the md docs)
# Update pages in real time while the server is running

#' Watches the site directory for changes and recompiles html appropriately
#' @name samatha.engine
samatha.engine <- function(site, post.layout = "default.R", figure.path = "img"){
    posts <- list.files(file.path(site, "template/posts"))
    pages <- list.files(file.path(site, "template/pages"), recursive = TRUE)
    for(post in posts[str_detect(posts, "Rmd$")]) {
        render.post(site, post, layout = post.layout, fig.path = figure.path)
    }
    for(page in pages[str_detect(pages, "R$")]) {
        render.page(site, page)
    }
    samatha.watch(path = file.path(site, "template"), site.watcher)
}


site.watcher <- function(added, deleted, modified){
    changed <- c(added, modified)
    changed <- changed[str_detect(changed, "\\.R(md)?$")]
    if(length(changed)){
        cat("changed:", changed, "\n")
        for(post in changed[str_detect(changed, "posts/.+\\.Rmd$")]) {
            render.post(site, basename(post), layout = post.layout, fig.path = figure.path)
        }
        for(page in changed[str_detect(changed, "pages/.+R$")]) {
            render.page(site, page)
        }
        
    }
    TRUE
}



