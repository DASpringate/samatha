\name{render.all.docs}
\alias{render.all.docs}
\title{Builds a list of Samatha.Page objects for all Rd documents in a given directory}
\usage{
  render.all.docs(site, doc.path, layout, out.path)
}
\arguments{
  \item{site}{character path to the Samatha site}

  \item{doc.path}{character path to the directory
  containing the Rd files}

  \item{layout}{name of the Samatha layout to be used to
  render the documents}

  \item{out.path}{path to documents relative to the Samatha
  site}
}
\value{
  list of samatha.Page objects
}
\description{
  Builds a list of Samatha.Page objects for all Rd
  documents in a given directory
}

