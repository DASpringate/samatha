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
                            upper <- list.files(file.path(site, basename(site)), full.names = TRUE)
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
#'  - layouts newer than any html files : rebuild everything - DONE
#'  - no corresponding html for source  OR source newer than html file : build
#'  - html files with no source : delete html
#'  - otherwise do nothing and return false
#'  todo - Error testing
#' @name update.site
update.site <- function(site, site.state, post.layout, tag.layout, fig.path){
    ## combine into a single function? --
    orphan.pages.p <- function(){
        # html pages with no R source
        sapply(1:length(site.state$dest_pages),
               function(x) !dp[x] %in% sp)
    }
    orphan.posts.p <- function(){
        # html posts with no R source
        sapply(1:length(site.state$dest_posts),
               function(x) !db[x] %in% sb)
    }
    check.layouts <- function(lays, states){
        # are any layouts newer than any files in states?
        for(l in lays){
            if(any(l > states)) return(TRUE)
        }
        FALSE
    }
    ## combine into a single function? --
    check.pages <- function(){
        # boolean vector -- true if dest file doesn't exist or is younger than source file
        sapply(1:length(site.state$source_pages),
               function(x) !sp[x] %in% dp || 
                   site.state$source_pages[x] > site.state$dest_pages[which(dp == sp[1])])
    }
    check.posts <- function(){
        # boolean vector -- true if dest file doesn't exist or is younger than source file
        sapply(1:length(site.state$source_posts),
               function(x) !sb[x] %in% db || 
                   site.state$source_posts[x] > site.state$dest_posts[which(db == sb[1])])
    }
    
    sp <- str_replace(as.character(sapply(names(site.state$source_pages), 
                                          function(x) basename(x))),
                      "\\.R", "\\.html") # source pages
    dp <- as.character(sapply(names(site.state$dest_pages), 
                              function(x) basename(x))) # dest pages
    sb <- str_replace(str_replace(as.character(sapply(names(site.state$source_posts), 
                                                      function(x) basename(x))), 
                                  "\\.Rmd", "\\.html"),
                      "^[[:digit:]]{4}_[[:digit:]]{2}_[[:digit:]]{2}_", "") # source blog posts
    
    db <- as.character(sapply(names(site.state$dest_posts), 
                              function(x) basename(x))) # dest blog posts
    orphan.pages <- names(site.state$dest_pages[orphan.pages.p()])
    orphan.posts <- names(site.state$dest_posts[orphan.posts.p()])
    if(length(orphan.pages) || length(orphan.posts)){
        for(f in c(orphan.pages, orphan.posts)){
            unlink(f)
        }
        cat(paste0("Orphan files deleted:\n",paste(c(orphan.pages, orphan.posts), 
                                                   collapse = ", ")))
        return(FALSE)
    }
    if(check.layouts(site.state$layouts, c(site.state$dest_pages, site.state$dest_posts))){
        for(post in names(site.state$source_posts)) {
            write.html(render.post(site, basename(post), 
                                   layout = post.layout, 
                                   fig.path = figure.path))
        }
        pages <- list.files(file.path(site, "template/pages"), recursive = TRUE)
        for(page in pages[str_detect(pages, "R$")]){
            write.html(render.page(site, page)) 
        }
        cat("rebuild after layout changes.\n")
        return(TRUE)
    }
    pages.tobuild <- names(site.state$source_pages[check.pages()])
    if(length(pages.tobuild)){
        p2b <- str_match(names(pages.tobuild), "(template/pages/)(.+)")[,3]
        for(p in p2b){
            write.html(render.page(site, p)) 
        }
        cat(paste0("Re/built pages:\n",paste(p2b, collapse = ", ")))
        return(TRUE)
    }
    posts.tobuild <- names(site.state$source_posts)[check.posts()]
    if(length(posts.tobuild)){
        for(post in posts.tobuild) {
            write.html(render.post(site, basename(post), 
                                   layout = post.layout, 
                                   fig.path = figure.path))
        }
        cat(paste0("Re/built posts:\n",paste(posts.tobuild, collapse = ", ")))
        return(TRUE)
    }
    FALSE
}

# site <- "/home/david/github/blog"
# site.state <- get.site.state(site)
# post.layout = "default.R" 
# tag.layout = "default.R" 
# figure.path = "img"
# pagename <- "index.R"


#' Samatha: Runs an infinite loop, updating the site as necessary
#' @name samatha
#' @export
samatha <- function(site, domain, 
                    post.layout = "default.R", 
                    tag.layout = "default.R", 
                    figure.path = "img"){
    while(TRUE){
        site.state <- get.site.state(site)
        site.updated <- update.site(site = site, site.state = site.state, 
                                    post.layout = post.layout, tag.layout = tag.layout, 
                                    fig.path = figure.path)
        if(site.updated){
            write.tags.to.file(site)
            render.tagfiles(site)
            render.rss(site, domain)
        }
        Sys.sleep(1)
    }
}
