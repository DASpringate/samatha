#' Representation of an RSS page in R
#' @name rssdoc
#' @export
rssdoc <- function(title, link, description, categories, rssitems){
    content <- paste('<?xml version="1.0" encoding="ISO-8859-1" ?>',
                     '<rss version="2.0">\n  <channel>\n    ', 
                     paste0(m("title", title), "\n    ", collapse = ""),
                     paste0(m("link", link), "\n    ", collapse = ""),
                     paste0(m("description", description), "\n    ", collapse = ""),
                     paste0(lapply(categories, function(x) m("category", x)), collapse = "\n    "),"\n    ",
                     paste0(rssitems, collapse = "\n    "), 
                     sep = "", collapse = "",
                     "\n  </channel>",
                     "\n</rss>")
    
    structure(content, class="Samatha.Rssdoc")
    content
}

#' wraps the details of a post up as an rss item
#' @name itemise.post
#' @export
itemise.post <- function(post, domain){
    fname <- str_match(post, pattern = "([[:digit:]]{4}_[[:digit:]]{2}_[[:digit:]]{2})_(.*)")
    item.title <- extract.title(post)
    item.content <- markdownToHTML(post,
                           fragment.only = TRUE)
    item.content <- str_replace_all(item.content, 'src=\"/', sprintf('src=\"%s/', domain))
    item.content <- escape.html(item.content)
    item.categories <- extract.tags(post)
    item.link <- file.path(domain, "posts", 
              str_replace(str_extract(fname[2],
                                      "[[:digit:]]{4}_[[:digit:]]{2}"), 
                          "_", "/"),
              str_replace(fname[3], "\\.md", "\\.html"))
    m("item",
      m("title", item.title),
      m("link", item.link),
      paste0(lapply(item.categories, 
             function(x) m("category", x)), collapse = ""),
      m("description", item.content))
}

#' Builds an rss file with all posts in the site
#' @name render.rss
render.rss <- function(site, domain){
    source(file.path(site, "template/layouts/rss.R"), local = TRUE)
    posts <- list.files(file.path(site, "template/posts"), full.names = TRUE)
    posts <- posts[str_detect(posts, "\\.md$")]
    postdates <- as.Date(str_extract(posts, "[[:digit:]]{4}_[[:digit:]]{2}_[[:digit:]]{2}"), 
                         format = "%Y_%m_%d")
    posts<- posts[order(postdates, decreasing = TRUE)]
    post.items <- lapply(posts, function(x) itemise.post(x, domain))
    cat(rssdoc(title = rss.title, link = rss.link, 
               description = rss.description,
               categories = rss.categories, 
               rssitems = post.items), 
        file = file.path(site, basename(site), "rss.xml"))
}



