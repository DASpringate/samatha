\name{update.site}
\alias{update.site}
\title{Checks if source files were modified after the corresponding dest files
if :
 - layouts newer than any html files : rebuild everything - DONE
 - no corresponding html for source  OR source newer than html file : build
 - html files with no source : delete html
 - otherwise do nothing and return false
 todo - Error testing}
\usage{
  update.site(site, site.state, post.layout, tag.layout,
    fig.path)
}
\description{
  Checks if source files were modified after the
  corresponding dest files if : - layouts newer than any
  html files : rebuild everything - DONE - no corresponding
  html for source OR source newer than html file : build -
  html files with no source : delete html - otherwise do
  nothing and return false todo - Error testing
}

