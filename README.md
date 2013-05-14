Samatha v0.3
============

#### Copyright David Springate 2013 ([CC BY 3.0](creativecommons.org/licenses/by/3.0))
##### @datajujitsu


Samatha is an R package for quickly building _Github-ready_ static sites in R. It contains a simple, functional-style DSL for rendering HTML, an engine for real-time compilation of static websites as you build them and a development web-server for viewing your sites of line before you deploy. 

*This project still in the early stages of development. Feel free to contact me with any bugs/suggestions*


The Static site engine uses the Samatha DSL to build layout templates which it then combines with content to generate individual pages. 
There are two ways to build pages:

1. Pages are written entirely in the Samatha DSL and compiled with a layout file.
2. Blog posts can be written in .Rmd format, which is then converted to md using [knitr](http://yihui.name/knitr/) and then to html using [markdown](http://cran.r-project.org/web/packages/markdown/index.html). Posts are then rendered within the layout for that post. 

When the engine is running:

* All Samatha pages are recompiled to html when a page is changed
* Any new or edited .Rmd posts are automatically converted to .html files.
* Files of tags and their links to individual posts are auomatically generated
* An RSS file is automatically generated and updated

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

I'm working on it!

* Wiki
* API docs

## The static site generator

##### Set up directories for a new site:

Before you start building pages , you need to build the directory structure of your site. Set `site` to the full path to your site or relative to the current position (Not using `~`, this is important for the markdown renderer later on).


```r
site <- "/home/david/testsite"
create.site.structure(site)
```


##### To render pages and posts

Pages are R files that are processed through a template file (also an R file). They must be located inside the `site/template/pages` directory. All page files should have the following variables:

1. `layout` The layout file you want the page to be processed using (all layout files should be kept in `site/template/layouts)
2. `title` A Character string for the title (Currently not used anywhere)
3. `page` A character string of the html of the body of the page

Here is an example of a very simple page file:


```r
# This gives a list of all posts in the site
layout <- "default.R"
title <- "home"

page <- content(m("h2", "My scribblings:"), html.postlist(site))
```


Pages are built using the render.page() function:


```r
render.page(site, "testpage.R")
```


They are automatically put in the correct place in the blog itself.  If the page file is in `site/template/pages`, the corresponding html file is put in the top level of `site/site/` (this is good for index files). If the page file is in `site/template/pages/pages`, the html file will be put in  `site/site/pages`

...much more to come...

##### To test your site in a development server

Currently I am using python to serve the files, but am working on an R development server based on Rook

To run the dev server (assuming you have Python installed):

```
cd path/to/mysite/mysite
python path/to/samatha/src/server.py
```
Then open your browser and point it to `http://localhost:8000/`

## The HTML DSL

The DSL is based on the [Hiccup](http://github.com/weavejester/hiccup) library for [Clojure](clojure.org).  You can use it to render html and generic xml.  The functions can be nested inside one another and are designed to be combined to easily build valid html in R.


##### The central function is `m() # for Markup`:


```r
m("p") # The first argument is the html tag:
```

```
## [1] "<p />"
```

```r
m("p", "This is a Sentence.", " So is this") # Any strings after form the content of the tag:
```

```
## [1] "<p>This is a Sentence. So is this</p>"
```

```r
m("span",  "bar", opts = list(class = "foo")) # The opts list defines html tag attributes
```

```
## [1] "<span class=\"foo\">bar</span>"
```

```r
m("span", opts = list(id = "foo", class = "bar"), "baz") 
```

```
## [1] "<span id=\"foo\" class=\"bar\">baz</span>"
```

```r
m("p", 
     "Goodbye", 
     m("strong", "cruel"), 
     "world")# Tags can be nested inside of other tags
```

```
## [1] "<p>Goodbye<strong>cruel</strong>world</p>"
```

```r
m("p#my-p", m("span.pretty", "hey")) # CSS-style shortcuts for ID and class
```

```
## [1] "<p id=\"my-p\"><span class=\"pretty\">hey</span></p>"
```

```r
m("p", m("script", "Do something evil", escape.html.p = TRUE)) # Escape a tag using escape.html.p = TRUE
```

```
## [1] "<p>&lt;script&gt;Do something evil&lt;/script&gt;</p>"
```


##### There are also wrappers to generate a range of common html elements...


```r
javascript.tag("Some javascript")  # To wrap the script string in script tags and a CDATA section
```

```
## [1] "<script>//<![CDATA[\nSome javascript\n//]]></script>"
```

```r
link.to("www.google.com", "Google")  # To wrap content in an HTML hyperlink with the supplied URL
```

```
## [1] "<a href=\"www.google.com\">Google</a>"
```

```r
# To wrap content in html hyperlink with the supplied email address.  If
# no content provided the email address is supplied as content:
mail.to("me@me.com")
```

```
## [1] "<a href=\"mailto:me@me.com\">me@me.com</a>"
```

```r
mail.to("me@me.com", "email me")
```

```
## [1] "<a href=\"mailto:me@me.com\">email me</a>"
```

```r
mail.to("me@me.com", "email me", subject = "you are great")
```

```
## [1] "<a href=\"mailto:me@me.com?Subject=you%20are%20great\">email me</a>"
```

```r
# To wrap a list of strings into an unordered list:
elements = list("apples", "oranges", "bananas")
unordered.list(elements)
```

```
## [1] "<ul><li>apples</li><li>oranges</li><li>bananas</li></ul>"
```

```r
ordered.list(elements)  # Ordered list
```

```
## [1] "<ol><li>apples</li><li>oranges</li><li>bananas</li></ol>"
```

```r
image.link("www.beautifulthings.com/12538675", opts = list(alt = "A lovely picture of something"))  # link to an image
```

```
## [1] "<img src=\"www.beautifulthings.com/12538675\" alt=\"A lovely picture of something\" />"
```

```r
head("My first page")
```

```
## [1] "My first page"
```

```r
body("Hello world!")
```

```
## Error: object 'Hello world!' of mode 'function' was not found
```


##### ...and functions to include css and js...


```r
cat(include.css(c("mysheeet.css", "sheet2.css", "sheet3.css")))
```

```
## <link type="text/css" href="mysheeet.css" rel="stylesheet" />
## <link type="text/css" href="sheet2.css" rel="stylesheet" />
## <link type="text/css" href="sheet3.css" rel="stylesheet" />
```

```r
cat(include.js(c("script1.js", "script2.js", "script3.js")))
```

```
## <script type="text/javascript" src="script1.js"></script>
## <script type="text/javascript" src="script2.js"></script>
## <script type="text/javascript" src="script3.js"></script>
```


##### Full pages with doctypes and outer html tags can be generated with the webdoc function


```r
webdoc("html5",
       head("My first page"),
       body("Hello world",
            unordered.list(elements)))
```

```
## Error: unused argument(s) (unordered.list(elements))
```



##### Still to be implemented:

* Functions for form elements
* Pretty printing




