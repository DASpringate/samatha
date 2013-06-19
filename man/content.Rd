\name{content}
\alias{content}
\title{concatenates html tags}
\usage{
  content(...)
}
\arguments{
  \item{\dots}{list or character vector of items to be
  concatenated}
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

