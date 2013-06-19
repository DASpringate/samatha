\name{render.doc}
\alias{render.doc}
\title{renders an .Rd file as a Samatha.Page object}
\usage{
  render.doc(site, doc, layout, path, ftype = "Rd_file")
}
\arguments{
  \item{site}{Absolute path to the samatha site}

  \item{doc}{path to the .Rd document}

  \item{layout}{name of the layout file to render the
  document files with}

  \item{path}{relative path to the document directory in
  the Samatha site}

  \item{ftype}{character file type}
}
\value{
  Object of class Samatha.Page
}
\description{
  renders an .Rd file as a Samatha.Page object
}

