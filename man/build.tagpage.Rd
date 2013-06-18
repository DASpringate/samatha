\name{build.tagpage}
\alias{build.tagpage}
\title{Generates page content for a tag, to be rendered with a layout}
\usage{
  build.tagpage(tag, tagname)
}
\arguments{
  \item{list}{element with associated vectors of titles and
  urls for associated posts}

  \item{tagname}{name of the tag}
}
\value{
  character vector of length 1 containing html for an
  unordered list of links to posts associated with the tag
}
\description{
  Generates page content for a tag, to be rendered with a
  layout
}
\seealso{
  collate.tags
}

