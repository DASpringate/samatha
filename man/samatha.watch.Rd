\name{samatha.watch}
\alias{samatha.watch}
\title{Watch a directory for changes (additions, deletions & modifications).}
\usage{
  samatha.watch(path, callback, pattern = NULL,
    hash = TRUE)
}
\arguments{
  \item{path}{character vector of paths to watch.  Omit
  trailing backslash.}

  \item{pattern}{file pattern passed to \code{\link{dir}}}

  \item{callback}{function called everytime a change
  occurs.  It should have three parameters: added, deleted,
  modified, and should return TRUE to keep watching, or
  FALSE to stop.}

  \item{hash}{hashes are more accurate at detecting
  changes, but are slower for large files.  When FALSE,
  uses modification time stamps}
}
\description{
  This is a slightly edited version of Hadley's watch.R in
  testthat I modified it to look recursively within the
  given directory (line 50) Otherwise, this is his code!
}
\details{
  Use Ctrl + break (windows), Esc (mac gui) or Ctrl + C
  (command line) to stop the watcher.
}

