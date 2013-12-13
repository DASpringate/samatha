\name{check.pagesPosts}
\alias{check.pagesPosts}
\title{check state of pages and posts}
\usage{
  check.pagesPosts(state.source, spp.ID, state.dest,
    dpp.ID)
}
\arguments{
  \item{state.source}{the modification dates of the source
  files}

  \item{spp.ID}{the actual file names}

  \item{state.dest}{the modification dates of the
  destination files}

  \item{dpp.ID}{the actual destination file names}
}
\value{
  logical which files need to be recompiled
}
\description{
  Checks the current state of pages and posts, i.e. whether
  they are compiled, or if the change date is newer than
  the compiled html
}

