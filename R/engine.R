#' Contstuct a page from the individual elements
#' 
#' @name make.samatha.page
#' @param content the actual contents of the page
#' @param title the title of the page, used if your template uses it
#' @param layout which layout file should be used
#' @export
#' @return Object of class Samatha.Page
make.samatha.page <- function(content, title, layout){
  page.obj <- structure(list(content=content,
                 title=title,
                 layout=layout),
            class="Samatha.Page")
}

#' Render a page using the Samatha html dsl
#'  pages are stored in site/template/pages
#'
#' @title Render a page using the Samatha html dsl
#' @name render.page
#' @description Renders a page according to its layout template
#' @export
#' @param site Absolute path to your Samatha site
#' @param pagename name of the R source file for the page to be rendered as html
#' @return Object of class Samatha.Page
#' An object of class Samatha.Page is a list containing at least the following components:
#' html         A character string of the html of a page
#' layout       The name of the layout file used to render the html
#' file         Name of the file to write the html to
#' title        title for the page
#' sourcefile   path to the source R or Rmd file for the page
#' @examples \dontrun{
#' render.post(site, "index.R", layout = "default")
#' }
render.page <- function(site, pagename){
    source(file.path(site, "template/pages", pagename), local = TRUE)
    page$html <- source(file.path(site, "template/layouts", page$layout), local=TRUE)$value
    page$file <- file.path(site, basename(site), str_replace(pagename, "\\.R", "\\.html"))
    page$sourcefile <- pagename
    page$tags <- ""
    
    page
} 

#' Render a post from an R markdown file
#' @name render.post
#' @description Render an .Rmd file into a page according to its layout template
#' post templates are stored in site/template
#' Better date functionality
#' @export
#' @param site Absolute path to your Samatha site
#' @param postname Name of the Rmd source for the post
#' @param layout The name of the layout file used to render the post
#' @param fig.path name of the directory in the site where figures (particularly R charts etc.) are to be kept
#' @return Object of class Samatha.Page
#' An object of class Samatha.Page is a list containing at least the following components:
#' html         A character string of the html of a page
#' layout       The name of the layout file used to render the html
#' file         Name of the file to write the html to
#' title        title for the page
#' sourcefile   path to the source R or Rmd file for the page
#' 
#' @examples \dontrun{
#' render.post(site, "My_first_post.Rmd", layout = "default", fig.path = "img")
#' } 
render.post <- function(site, postname, layout, fig.path, includetags){
    postnames <- str_match(postname, pattern = "([[:digit:]]{4}_[[:digit:]]{2}_[[:digit:]]{2})_(.*)")
    rmd.file <- file.path(site, "template/posts", postnames[1])
    md.file <- str_replace(rmd.file, "\\.Rmd", "\\.md")
    if(length(postnames) != 3 | ! str_detect(postnames[3], "\\.Rmd")){
        cat(sprintf("Bad post filename: %s", postnames[1]))
        return(FALSE)
    }
    
    fullFig.path <- file.path(site, basename(site), paste0(fig.path, "/"))
    
    # logic to only knit if necessary. i.e. no markdown file, or the Rmd has been updated compared to the md.
    if (file.exists(md.file)){
      md.mod <- file.info(md.file)["mtime"][1,1]
      rmd.mod <- file.info(rmd.file)["mtime"][1,1]
      
      if (difftime(md.mod, rmd.mod) < 0){
        knit.post(rmd.file, md.file, fullFig.path)
      }
      
    } else {
      knit.post(rmd.file, md.file, fullFig.path)
    }

    page.cont <- markdownToHTML(md.file, fragment.only = TRUE)
    
    page.tags <- extract.tags(md.file)
    tag.linklist <- tag.links(page.tags)
    if (includetags){
      tagstring <- paste0("Tagged in: ", paste0(tag.linklist, collapse=", "))
      page.cont <- paste0(page.cont, m("h5", tagstring))
    }
    
    page.cont <- paste0(page.cont, m("h6", sprintf("Posted on %s", as.Date(postnames[2], format("%Y_%m_%d")))))
    page.cont <- str_replace_all(page.cont, 
                            paste0("img src=\"",file.path(site, basename(site), fig.path)), 
                            paste0("img src=\"/", fig.path))
    month.dir <- file.path(site, basename(site), "posts", 
                           str_replace(str_extract(postnames[2], 
                                                   "[[:digit:]]{4}_[[:digit:]]{2}"), 
                                       "_", "/"))
    dir.create(month.dir, showWarnings = FALSE, recursive = TRUE)
    
    page <- list(content=page.cont, title=extract.title(md.file))
    post.obj <- structure(list(html = source(file.path(site, "template/layouts", layout), local = TRUE)$value,
                               content = page$content,
                               layout = layout,
                               file = file.path(month.dir, 
                                                str_replace(postnames[3], "\\.Rmd", "\\.html")),
                               title = page$title,
                               sourcefile = postname,
                               tags = extract.tags(md.file)),
                          class = "Samatha.Page")
    post.obj
}

#' knits an Rmd post to md
#' 
#' If a blog post involves a lot of heavy computation, it may be useful to be able to knit the Rmd to md first. This should be true whether using \code{samatha} with \code{initial=TRUE} or \code{FALSE}. 
#' @name knit.post
#' @param rmd.file the full path to the R markdown file to knit
#' @param md.file the full path to the markdown file output
#' @param fig.path the absolute figure path used by the samatha site
#' @return character md file name if successful, empty value otherwise
#' @export
knit.post <- function(rmd.file, md.file, fig.path){
  opts_chunk$set(fig.path = fig.path)
  knit(input =  rmd.file,
       output = md.file)
  md.file
}

#' Writes the html content of a Samatha.Page object to 
#' the file specified in the file element. 
#' @name write.html
#' @param samatha.page an object of class Samatha.Page
#' @return logical FALSE if the object is not a Samantha.Page object, otherwise TRUE
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
#' @name file.states
#' @param files character vector of file paths
#' @return a named vector of modification times with file paths as namess
file.states <- function(files){
    setNames(file.info(files)$mtime, files)
}

#' Gets modification dates for all source and dest files in a site
#' @name get.site.state
#' @param site Absolute path to your Samatha site
#' @return {
#' a list of file states (as returned by file.states()) for the different elements of the site:
#'  "layouts", "source_pages", "source_posts", "dest_pages", "dest_posts"
#' }
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
#' @param site Absolute path to your Samatha site
#' @param site.state The value of get.site.state(site): modification times for elements of the site
#' @param post.layout The name of the layout file used to render posts
#' @param tag.layout The name of the layout file used to render subject tags
#' @param fig.path name of the directory in the site where figures (particularly R charts etc.) are to be kept
#' @return logical TRUE if site has been updated, FALSE otherwise
update.site <- function(site, site.state, post.layout, tag.layout, fig.path){
    ## combine into a single function? --
    orphan.pages.p <- function(){
        # html pages with no R source
        if(length(site.state$dest_pages)){
            sapply(1:length(site.state$dest_pages),
                   function(x) !dp[x] %in% sp)
        } else FALSE
    }
    
    orphan.posts.p <- function(){
        # html posts with no R source
        if(length(site.state$dest_posts)){
            sapply(1:length(site.state$dest_posts),
                   function(x) !db[x] %in% sb)
        } else FALSE
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
                   site.state$source_pages[x] > site.state$dest_pages[which(dp == sp[x])])
    }
    check.posts <- function(){
        # boolean vector -- true if dest file doesn't exist or is younger than source file
        sapply(1:length(site.state$source_posts),
               function(x) !sb[x] %in% db || 
                   site.state$source_posts[x] > site.state$dest_posts[which(db == sb[x])])
    }
    
    
    sp <- catch_char_zero(str_replace(as.character(sapply(names(site.state$source_pages), 
                                          function(x) basename(x))),
                      "\\.R", "\\.html")) # source pages
    dp <- catch_char_zero(as.character(sapply(names(site.state$dest_pages), 
                              function(x) basename(x)))) # dest pages
    sb <- catch_char_zero(str_replace(str_replace(as.character(sapply(names(site.state$source_posts), 
                                                      function(x) basename(x))), 
                                  "\\.Rmd", "\\.html"),
                      "^[[:digit:]]{4}_[[:digit:]]{2}_[[:digit:]]{2}_", "")) # source blog posts
    
    db <- catch_char_zero(as.character(sapply(names(site.state$dest_posts), 
                              function(x) basename(x)))) # dest blog posts
    orphan.pages <- names(site.state$dest_pages[orphan.pages.p()])
    orphan.posts <- names(site.state$dest_posts[orphan.posts.p()])
    
    if(length(orphan.pages) || length(orphan.posts)){
        for(f in c(orphan.pages, orphan.posts)){
            unlink(f)
        }
        cat(paste0("Orphan files deleted:\n",paste(c(orphan.pages, orphan.posts), 
                                                   collapse = ", ")), "\n")
        return(FALSE)
    }
    if(check.layouts(site.state$layouts, c(site.state$dest_pages, site.state$dest_posts))){
        for(post in names(site.state$source_posts)) {
            write.html(render.post(site, basename(post), 
                                   layout = post.layout, 
                                   fig.path = fig.path))
        }
        pages <- list.files(file.path(site, "template/pages"), recursive = TRUE)
        for(page in pages[str_detect(pages, "R$")]){
            write.html(render.page(site, page)) 
        }
        cat("Full site rebuild after layout changes.\n")
        return(TRUE)
    }
    pages.tobuild <- names(site.state$source_pages[check.pages()])
    if(length(pages.tobuild)){
        p2b <- str_match(pages.tobuild, "(template/pages/)(.+)")[,3]
        for(p in p2b){
            write.html(render.page(site, p)) 
        }
        cat(paste0("Re/built pages:\n",paste(p2b, collapse = ", ")), "\n")
        return(TRUE)
    }
    posts.tobuild <- names(site.state$source_posts)[check.posts()]
    if(length(posts.tobuild)){
        for(post in posts.tobuild) {
            write.html(render.post(site, basename(post), 
                                   layout = post.layout, 
                                   fig.path = figure.path))
        }
        cat(paste0("Re/built posts:\n",paste(posts.tobuild, collapse = ", ")), "\n")
        return(TRUE)
    }
    FALSE
}

#' Refresh all posts and pages
#' if :
#'  - layouts newer than any html files : rebuild everything - DONE
#'  - no corresponding html for source  OR source newer than html file : build
#'  - html files with no source : delete html
#'  - otherwise do nothing and return false
#'  todo - Error testing
#' @name refresh.site
#' @param site Absolute path to your Samatha site
#' @param site.state The value of get.site.state(site): modification times for elements of the site
#' @param post.layout The name of the layout file used to render posts
#' @param tag.layout The name of the layout file used to render subject tags
#' @param fig.path name of the directory in the site where figures (particularly R charts etc.) are to be kept
#' @return logical TRUE if site has been updated, FALSE otherwise
refresh.site <- function(site, site.state, post.layout, tag.layout, fig.path, includetags){
    ## combine into a single function? --
    orphan.pages.p <- function(){
        # html pages with no R source
        if(length(site.state$dest_pages)){
            sapply(1:length(site.state$dest_pages),
                   function(x) !dp[x] %in% sp)
        } else FALSE
    }
    
    orphan.posts.p <- function(){
        # html posts with no R source
        if(length(site.state$dest_posts)){
            sapply(1:length(site.state$dest_posts),
                   function(x) !db[x] %in% sb)
        } else FALSE
    }
    
    check.layouts <- function(lays, states){
        # are any layouts newer than any files in states?
        for(l in lays){
            if(any(l > states)) return(TRUE)
        }
        FALSE
    }
        catch_char_zero <- function(x){
        if(!length(x))x <- ""
        x
    }
    
    sp <- catch_char_zero(str_replace(as.character(sapply(names(site.state$source_pages), 
                                                          function(x) basename(x))),
                                      "\\.R", "\\.html")) # source pages
    dp <- catch_char_zero(as.character(sapply(names(site.state$dest_pages), 
                                              function(x) basename(x)))) # dest pages
    sb <- catch_char_zero(str_replace(str_replace(as.character(sapply(names(site.state$source_posts), 
                                                                      function(x) basename(x))), 
                                                  "\\.Rmd", "\\.html"),
                                      "^[[:digit:]]{4}_[[:digit:]]{2}_[[:digit:]]{2}_", "")) # source blog posts
    
    db <- catch_char_zero(as.character(sapply(names(site.state$dest_posts), 
                                              function(x) basename(x)))) # dest blog posts
    orphan.pages <- names(site.state$dest_pages[orphan.pages.p()])
    orphan.posts <- names(site.state$dest_posts[orphan.posts.p()])
    
    if(length(orphan.pages) || length(orphan.posts)){
        for(f in c(orphan.pages, orphan.posts)){
            unlink(f)
        }
        cat(paste0("Orphan files deleted:\n",paste(c(orphan.pages, orphan.posts), 
                                                   collapse = ", ")), "\n")
        return(FALSE)
    }
    for(post in names(site.state$source_posts)) {
        write.html(render.post(site, basename(post), 
                               layout = post.layout, 
                               fig.path = fig.path,
                               includetags = includetags))
    }
    
    pages <- list.files(file.path(site, "template/pages"), recursive = TRUE)
    pages.tobuild <- names(site.state$source_pages)
    if(length(pages.tobuild)){
        p2b <- str_match(pages.tobuild, "(template/pages/)(.+)")[,3]
        for(p in p2b){
            write.html(render.page(site, p)) 
        }
        cat(paste0("Re/built pages:\n",paste(p2b, collapse = ", ")), "\n")
     }
    posts.tobuild <- names(site.state$source_posts)
    if(length(posts.tobuild)){
        for(post in posts.tobuild) {
            write.html(render.post(site, basename(post), 
                                   layout = post.layout, 
                                   fig.path = figure.path,
                                   includetags = includetags))
        }
        cat(paste0("Re/built posts:\n",paste(posts.tobuild, collapse = ", ")), "\n")
    }
    TRUE
}

#' Samatha: Runs an infinite loop, updating the site as necessary
#' This is the main command to update your site.  You can leave this running while you make edits to source files
#' @name samatha
#' @param site character absolute path to your Samatha site
#' @param rss boolean build rss page?
#' @param initial boolean if true, runs the whole engine once, rebuilding the whole site.  If false runs an infinite loop updating only where necessary
#' @export
samatha <- function(site, rss = TRUE, initial = FALSE){
    source(file.path(site, "template/config/config.R"), echo = TRUE, local = FALSE)
    if(initial){
        site.state <- get.site.state(site)
        site.updated <- refresh.site(site = site, site.state = site.state, 
                                    post.layout = post.layout, tag.layout = tag.layout, 
                                    fig.path = figure.path, includetags=includetags)
        site.updated <- refresh.site(site = site, site.state = site.state, 
                                     post.layout = post.layout, tag.layout = tag.layout, 
                                     fig.path = figure.path, includetags=includetags)
        write.tags.to.file(site)
        render.tagfiles(site, tag.layout = tag.layout)
        if(rss){
            render.rss(site, domain = domain, rss.title = rss.title, 
                       rss.description = rss.description, rss.categories = rss.categories)
            rss.category(site, domain = domain, categories = rss.category.feeds)
        }
    } else{
        while(TRUE){
            site.state <- get.site.state(site)
            site.updated <- update.site(site = site, site.state = site.state, 
                                        post.layout = post.layout, tag.layout = tag.layout, 
                                        fig.path = figure.path)
            if(site.updated){
                write.tags.to.file(site)
                render.tagfiles(site, tag.layout = tag.layout)
                if(rss){
                    render.rss(site, domain = domain, rss.title = rss.title, 
                               rss.description = rss.description, rss.categories = rss.categories)
                    rss.category(site, domain = domain, categories = rss.category.feeds)
                }
            }
            Sys.sleep(1)
        }    
    }
}
