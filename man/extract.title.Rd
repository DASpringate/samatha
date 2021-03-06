\name{extract.title}
\alias{extract.title}
\title{reads a markdown post file and extracts the title
Title is taken as the first line in which the line underneath is a double underline
i.e. a h1 tag in markdown
n.b. the title must be in this format, not e.g. # This is a title}
\usage{
  extract.title(md.file)
}
\arguments{
  \item{md.file}{.md file of a post}
}
\value{
  character sting of the title of the post
}
\description{
  reads a markdown post file and extracts the title Title
  is taken as the first line in which the line underneath
  is a double underline i.e. a h1 tag in markdown n.b. the
  title must be in this format, not e.g. # This is a title
}
\seealso{
  extract.tags
}

