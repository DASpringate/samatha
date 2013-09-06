\name{refresh.site}
\alias{refresh.site}
\title{Refresh all posts and pages
if :
 - layouts newer than any html files : rebuild everything - DONE
 - no corresponding html for source  OR source newer than html file : build
 - html files with no source : delete html
 - otherwise do nothing and return false
 todo - Error testing}
\usage{
  refresh.site(site, site.state, post.layout, tag.layout,
    fig.path)
}
\arguments{
  \item{site}{Absolute path to your Samatha site}

  \item{site.state}{The value of get.site.state(site):
  modification times for elements of the site}

  \item{post.layout}{The name of the layout file used to
  render posts}

  \item{tag.layout}{The name of the layout file used to
  render subject tags}

  \item{fig.path}{name of the directory in the site where
  figures (particularly R charts etc.) are to be kept}
}
\value{
  logical TRUE if site has been updated, FALSE otherwise
}
\description{
  Refresh all posts and pages if : - layouts newer than any
  html files : rebuild everything - DONE - no corresponding
  html for source OR source newer than html file : build -
  html files with no source : delete html - otherwise do
  nothing and return false todo - Error testing
}

