#' Functions for building html versions of an R package and rendering them as Static docs with Samatha layouts
#' 
#' 

#' Converts an Rd file to an html fragment
#' @name doc.to.html
#' @param doc a path to an Rd file
#' @return list containing a named character vector of the corresponding HTML fragment
doc.to.html <- function(doc){
    html <- capture.output(Rd2HTML(parse_Rd(doc), fragment = TRUE))
    html <- html[nchar(html)>0]
    html <- html[!str_detect(html, ">NA<")]
    name <- html[1]
    list(page = content(c(m("h1", name), html[2:length(html)])),
                 title = name)
}

render.doc <- function(site, doc, layout, path){
    doc.obj <- doc.to.html(doc)
    page <- content(doc.obj$page, link.to(paste(path,"index.html", sep = "/"), "<< Back to index"))
    title <- doc.obj$title
    structure(list(html = source(file.path(site, "template/layouts", layout), local = TRUE)$value,
                               content = page,
                               layout = layout,
                               file = file.path(site, 
                                                basename(site), path,
                                                paste0(str_replace(title, "\\.", "_"), ".html")),
                               title = title,
                               sourcefile = doc,
                               tags = "Rd_file"),
                          class = "Samatha.Page")
}

render.all.docs <- function(site, doc.path, layout, out.path){
    doc.files <- list.files(doc.path)
    lapply(doc.files,
           function(doc){
               render.doc(site, file.path(doc.path, doc), layout, out.path)
           })
}

write.docs <- function(site, doc.path, layout, out.path){
    dir.create(file.path(site, basename(site), out.path), 
               showWarnings = FALSE, recursive = TRUE)
    message(sprintf("Rendering doc files to html and writing to %s", out.path))
    doc.pages <- render.all.docs(site, doc.path, layout, out.path)
    
    for(pg in doc.pages){
        write.html(pg)
    }
}



