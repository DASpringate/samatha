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

#' renders an .Rd file as a Samatha.Page object
#' @name render.doc
#' @param site Absolute path to the samatha site
#' @param doc path to the .Rd document
#' @param layout name of the layout file to render the document files with
#' @param path relative path to the document directory in the Samatha site
#' @param ftype character file type
#' @return Object of class Samatha.Page
render.doc <- function(site, doc, layout, path, ftype = "Rd_file"){
    doc.obj <- doc.to.html(doc)
    page <- content(doc.obj$page)
    title <- doc.obj$title
    structure(list(html = source(file.path(site, "template/layouts", layout), local = TRUE)$value,
                               content = page,
                               layout = layout,
                               file = file.path(site, 
                                                basename(site), path,
                                                paste0(str_replace(title, "\\.", "_"), ".html")),
                               title = title,
                               sourcefile = doc,
                               tags = ftype),
                          class = "Samatha.Page")
}

#' Builds a list of Samatha.Page objects for all Rd documents in a given directory
#' @name render.all.docs
#' @param site character path to the Samatha site
#' @param doc.path character path to the directory containing the Rd files
#' @param layout name of the Samatha layout to be used to render the documents
#' @param out.path path to documents relative to the Samatha site
#' @return list of samatha.Page objects
render.all.docs <- function(site, doc.path, layout, out.path){
    doc.files <- list.files(doc.path)
    lapply(doc.files,
           function(doc){
               render.doc(site, file.path(doc.path, doc), layout, out.path)
           })
}

#' Produces an unordered list of links to rendered documentation files
#' @name doc.index.list
#' @param doc.pages a list of Samatha.Page objects, as produced by render.all.docs()
#' @param out.path path to documents relative to the Samatha site
#' @return character string representation of an unorderd list of links to rendered documentation files
doc.index.list <- function(doc.pages, out.path){
    doc.names <- sapply(doc.pages, function(x) x$title)    
    descriptions <- sapply(doc.pages, function(x) str_match(x$content, ".(<p>)([[:alnum:] , \\.]+)(</p>)")[3])
    unordered.list(lapply(1:length(doc.names), 
                          function(x) paste0(link.to(url = paste("",out.path, 
                                                                  paste0(str_replace(doc.names[x], "\\.", "_"), ".html"), 
                                                                  sep = "/"), 
                                                      m("b", doc.names[x])), 
                                              " ", descriptions[x])))
}

#' Renders a directory of Rd files to html and writes within a Samatha site directory
#' @name write.docs
#' @param site character path to the Samatha site
#' @param doc.path path to the directory of Rd files
#' @param layout Samatha layout file to render documentation with
#' @param out.path path to documents relative to the Samatha site
write.docs <- function(site, doc.path, layout, out.path){
    dir.create(file.path(site, basename(site), out.path), 
               showWarnings = FALSE, recursive = TRUE)
    message(sprintf("Rendering doc files to html and writing to %s", out.path))
    doc.pages <- render.all.docs(site, doc.path, layout, out.path)
    for(pg in doc.pages){
        write.html(pg)
    }
    page <- content(m("h1", "Package index"), doc.index.list(doc.pages, out.path))
    index.obj <- structure(list(html = source(file.path(site, "template/layouts", layout), local = TRUE)$value,
                   content = page,
                   layout = layout,
                   file = file.path(site, 
                                    basename(site), out.path, "index.html"),
                   title = title,
                   sourcefile = doc,
                   tags = "doc_index"),
              class = "Samatha.Page")
    write.html(index.obj)
}



