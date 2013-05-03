#' Start a new site 
#' @name create.site.structure
#' @description Sets up the directory structure for a new static site
#' @param site path to the directory you want to set up
create.site.structure <- function(site){
    new.site.p <- dir.create(site, showWarnings = FALSE)
    if(new.site.p){
        for(d in c("posts", "pages", "css", "img", "js", "resources", "tag")){
            dir.create(file.path(site, basename(site), d), 
                       showWarnings = FALSE, recursive = TRUE)
        }
        for(d in c("layouts", "posts", "pages")){
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

#' Render a page using the Samatha html dsl
#' @name render.page
#' @description Renders a page according to its layout template
#' pages are stored in site/template/pages
render.page <- function(site, pagename, subdir = ""){
    source(file.path(site, "template/pages", pagename), local = TRUE)
    cat(source(file.path(site, "template/layouts", layout), local = TRUE)$value, 
               file = file.path(site, 
                                basename(site), 
                                subdir, 
                                str_replace(pagename, "\\.R", "\\.html")))
} 

#' Render a post from an R markdown file
#' @name render.post
#' @description Render an .Rmd file into a page according to its layout template
#' post templates are stored in site/template
#' Need to eventually fix the file copy of figures to take into account changes in files
#' Better date functionality
render.post <- function(site, postname, layout = "default.R", fig.path = "img"){
    postnames <- str_match(postname, pattern = "([[:digit:]]{4}_[[:digit:]]{2}_[[:digit:]]{2})_(.*)")
    if(length(postnames) != 3 | ! str_detect(postnames[3], "\\.Rmd")) stop(sprintf("Bad post filename: %s", postnames[1]))
    opts_chunk$set(fig.path = "img/")
    knit(input = file.path(site, "template/posts", postnames[1]),
         output = file.path(site, "template/posts", str_replace(postnames[1], "\\.Rmd", "\\.md")))
    for(f in list.files("img")) file.copy(file.path("img", f), file.path(site, basename(site), "img", f), overwrite = FALSE)
    page <- markdownToHTML(file.path(site, "template/posts", str_replace(postnames[1], "\\.Rmd", "\\.md")),
                                   fragment.only = TRUE)
    page <- paste0(page, m("h6", sprintf("Posted on %s", as.Date(postnames[2], format("%Y_%m_%d")))))
    month.dir <- file.path(site, basename(site), "posts", 
                          str_extract(postnames[2], "[[:digit:]]{4}_[[:digit:]]{2}"))
    dir.create(month.dir, showWarnings = FALSE)
    cat(source(file.path(site, "template/layouts", layout), local = TRUE)$value, 
        file = file.path(month.dir, 
                         str_replace(postnames[3], "\\.Rmd", "\\.html")))   
}

#' Index the pages of a site
#' @name index.site
#' @description makes an .RData file of all of the compiled pages in the site with modification dates
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



