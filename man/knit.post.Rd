\name{knit.post}
\alias{knit.post}
\title{knits an Rmd post to md}
\usage{
  knit.post(rmd.file, md.file, fig.path)
}
\arguments{
  \item{rmd.file}{the full path to the R markdown file to
  knit}

  \item{md.file}{the full path to the markdown file output}

  \item{fig.path}{the absolute figure path used by the
  samatha site}
}
\value{
  character md file name if successful, empty value
  otherwise
}
\description{
  If a blog post involves a lot of heavy computation, it
  may be useful to be able to knit the Rmd to md first.
  This should be true whether using \code{samatha} with
  \code{initial=TRUE} or \code{FALSE}.
}

