#' Sets up directory structure for a new Samatha site
#' 2 main subdirectories: template (for source code for the site) and basename(site) for the html etc. of the site itself
#' @name skeleton
#' @description Sets up the directory structure for a new static site
#' @param site path to the directory you want to set up
#' @return logical TRUE if successful, FALSE if directory already exists
#' @export
#' @examples \dontrun{
#' mysite <- "/home/david/github/mysite"
#' skeleton(mysite)
#' }
skeleton <- function(site){
    new.site.p <- dir.create(site, showWarnings = FALSE)
    if(new.site.p){
        for(d in c("posts", "pages", "css", "img", "js", "resources", "tags")){
            dir.create(file.path(site, basename(site), d), 
                       showWarnings = FALSE, recursive = TRUE)
        }
        for(d in c("layouts", "posts", "pages/pages", "config",
                   "resources/markdown", "resources/html", "resources/json")){
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

#' Sets up an example site
#' @name setup_example_site
#' @description provides a barebones Samatha site
#' @param site path to the directory you want to set up
#' @param resources path to the resources directory in the Samatha package
#' @export
setup_example_site <- function(site, resources = "resources"){
    # copy over twitter bootstrap files
    csslist <- list.files(file.path(resources, "css"), full.names = TRUE)
    file.copy(csslist, file.path(site, basename(site), "css"), overwrite = TRUE)
    # Copy default template file and config file
    file.copy(file.path(resources, "default_template.R"), file.path(site, "template/layouts"), overwrite = TRUE)
    file.copy(file.path(resources, "config.R"), file.path(site, "template/config/config.R"), overwrite = TRUE)
    pagelist <- list.files(file.path(resources, "pages"),full.names = TRUE, recursive = FALSE)
    file.copy(pagelist, file.path(site, "template/pages"), recursive= TRUE, overwrite = TRUE)
    postlist <- list.files(file.path(resources, "posts"),full.names = TRUE, recursive = TRUE)
    file.copy(postlist, file.path(site, "template/posts"), recursive= TRUE, overwrite = TRUE)
    resourcelist <- list.files(file.path(resources, "resources"),full.names = TRUE, recursive = TRUE)
    file.copy(resourcelist, file.path(site, "template/resources"), recursive= TRUE, overwrite = TRUE)
}

