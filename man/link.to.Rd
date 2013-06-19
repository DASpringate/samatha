\name{link.to}
\alias{link.to}
\title{link tag representaion}
\usage{
  link.to(url, ..., opts = list())
}
\arguments{
  \item{url}{character the url the link points to}

  \item{\dots}{the content of the tag}

  \item{opts}{list of optional tag attributes}
}
\value{
  character
}
\description{
  Wraps content in an HTML hyperlink with the supplied URL
}
\examples{
link.to("www.google.com", "Google")
}

