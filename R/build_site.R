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
