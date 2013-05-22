Samatha v0.3
============

#### Copyright David Springate 2013 ([CC BY 3.0](creativecommons.org/licenses/by/3.0))
##### @datajujitsu


Samatha is an R package for quickly building _Github-ready_ static sites in R. It contains a simple, functional-style DSL for rendering HTML, an engine for compilation of static websites as you build them and a development web-server for viewing your sites of line before you deploy. 

*This project is still in the early stages of development. Feel free to contact me with any bugs/suggestions*

The Static site engine uses the Samatha DSL to build layout templates which it then combines with content to generate individual pages. 
There are two ways to build pages:

1. Pages are written entirely in the Samatha DSL and compiled with a layout file.
2. Blog posts can be written in .Rmd format, which is then converted to md using [knitr](http://yihui.name/knitr/) and then to html using [markdown](http://cran.r-project.org/web/packages/markdown/index.html). Posts are then rendered within the layout for that post. 

The Samatha engine:

* Recompiles pages written in the markup dsl to html 
* Converts all new or edited .Rmd posts to .html files.
* Automatically generates html files of post tags and their links to individual posts generated
* Automatically generates an RSS file at the top level of your site with global ant per item tags, content and full links to images etc.

Extra features:

* simple wrapper functions for including snippet files containing md or html/js (e.g. for external comments site code and analytics)
* Generates tags from the first line of all posts (starting with \%)
* Functions for building lists of tags and lists of posts

Sites are created with the following structure:

* __basename__
    - __template__ the source for your site
        - __layouts__ layout templates for pages, posts, rss and tags
        - __pages__ Content of pages built with the Samatha dsl
        - __posts__ Rmd files of blog posts
        - __resources__ html/js/md snippets
    - __basename__ the compiled site.  Copy the contents to a git repo to have a functioning site
        - css 
        - img plots from knitted Rmd are automatically placed here
        - pages
        - posts
        - tags

## Examples

My [personal blog](http://daspringate.github.io) was built using Samatha, [twitter bootstrap](http://twitter.github.io/bootstrap/) and [Github pages](http://pages.github.com/). I used the [Readable](http://bootswatch.com/readable/) theme. See [here](https://github.com/DASpringate/blog) for the file structure of a Samatha site.

## Install

You should be able to install the current version of Samatha with devtools:


```r
# check install_github()
require(devtools)
load_all(".")  # In the correct directory!
```

```
## Loading samatha
```


## Documentation

_I'm still working on it!_

* [Wiki](https://github.com/DASpringate/samatha/wiki/_pages)
* [API docs](https://github.com/DASpringate/samatha/tree/master/man)



