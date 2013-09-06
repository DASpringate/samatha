\name{samatha}
\alias{samatha}
\title{Samatha: Runs an infinite loop, updating the site as necessary
This is the main command to update your site.  You can leave this runningg while you make edits to source files}
\usage{
  samatha(site, rss = TRUE, initial = FALSE)
}
\arguments{
  \item{site}{character absolute path to your Samatha site}

  \item{rss}{boolean build rss page?}

  \item{initial}{boolean if true, runs the whole engine
  once, rebuilding the whole site.  If false runs an
  infinite loop updating only where necessary}
}
\description{
  Samatha: Runs an infinite loop, updating the site as
  necessary This is the main command to update your site.
  You can leave this runningg while you make edits to
  source files
}

