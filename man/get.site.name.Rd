\name{get.site.name}
\alias{get.site.name}
\title{Gets modification dates for all source and dest files in a site}
\usage{
  get.site.state(site)
}
\arguments{
  \item{site}{Absolute path to your Samatha site}
}
\value{
  { a list of file states (as returned by file.states())
  for the different elements of the site: "layouts",
  "source_pages", "source_posts", "dest_pages",
  "dest_posts" }
}
\description{
  Gets modification dates for all source and dest files in
  a site
}

