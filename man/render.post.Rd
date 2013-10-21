\name{render.post}
\alias{render.post}
\title{Render a post from an R markdown file}
\usage{
  render.post(site, postname, layout, fig.path,
    includetags)
}
\arguments{
  \item{site}{Absolute path to your Samatha site}

  \item{postname}{Name of the Rmd source for the post}

  \item{layout}{The name of the layout file used to render
  the post}

  \item{fig.path}{name of the directory in the site where
  figures (particularly R charts etc.) are to be kept}
}
\value{
  Object of class Samatha.Page An object of class
  Samatha.Page is a list containing at least the following
  components: html A character string of the html of a page
  layout The name of the layout file used to render the
  html file Name of the file to write the html to title
  title for the page sourcefile path to the source R or Rmd
  file for the page
}
\description{
  Render an .Rmd file into a page according to its layout
  template post templates are stored in site/template
  Better date functionality
}
\examples{
\dontrun{
render.post(site, "My_first_post.Rmd", layout = "default", fig.path = "img")
}
}

