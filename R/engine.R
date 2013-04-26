#' An engine for building static sites in R
#' 


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

#' @name render.pages
#' @description Renders a page according to its layout template
render.page <- function(site, pagename, subdir = ""){
    source(file.path(site, "template/pages", pagename), local = TRUE)
    cat(source(file.path(site, "template/layouts", layout))$value, 
               file = file.path(site, 
                                basename(site), 
                                subdir, 
                                str_replace(pagename, "\\.R", "\\.html")))
} 



