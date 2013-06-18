\name{content}
\alias{content}
\title{concatenates html tags}
\usage{
  content(...)
}
\value{
  character
}
\description{
  Seqentially concatenates strings separated by newlines
}
\examples{
content(m("article", "article 1"), m("h1", "title"), m("section", "this is a section"))
}

