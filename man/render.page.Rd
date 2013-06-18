\name{render.page}
\alias{render.page}
\title{Render a page using the Samatha html dsl}
\usage{
  render.page(site, pagename)
}
\arguments{
  \item{site}{Absolute path to your Samatha site}

  \item{pagename}{name of the R source file for the page to
  be rendered as html}
}
\value{
  {Object of class Samatha.Page An object of class
  "Samatha.Page is a list containing at least the following
  components: html A character string of the html of a page
  layout The name of the layout file used to render the
  html file Name of the file to write the html to title
  title for the page sourcefile path to the source R or Rmd
  file for the page } examples \dontrun{ render.post(site,
  "index.R", layout = "default") }
}
\description{
  Renders a page according to its layout template
}
\details{
  Render a page using the Samatha html dsl pages are stored
  in site/template/pages
}

