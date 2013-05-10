Samatha
=======

#### Copyright David Springate 2013 ([CC BY 3.0](creativecommons.org/licenses/by/3.0))
##### @datajujitsu

Samatha is an R package containing a simple DSL for rendering HTML and an engine for building static websites. 

The DSL is based on the [Hiccup](http://github.com/weavejester/hiccup) library for [Clojure](clojure.org).  You can use it to render html and generic xml.  The functions can be nested inside one another and are designed to be combined to easily build valid html in R.

The Static site engine uses the Samatha DSL to build layout templates which it then combines with content for individual pages.  Pages can be written in .Rmd format, which is then converted to md using [knitr]() and then to html using [markdown](). Pages are then rendered within the layout for that page. 

When the engine is running, all Samatha pages are recompiled and any new .Rmd posts are automatically converted to .html files.


This project is in the very early stages of development! Feel free to contact me with any bugs/suggestions

## Install

You should be able to install the current version of Samatha with devtools:


```r
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

## The HTML DSL

##### The central function is `m() # for Markup`:


```r
m("p") # The first argument is the html tag:
```

```
## [1] "<p  />"
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
## [1] "<img  src=\"www.beautifulthings.com/12538675\" alt=\"A lovely picture of something\" />"
```

```r
head("My first page")
```

```
## [1] "<head  title=\"My first page\" />"
```

```r
body("Hello world!")
```

```
## [1] "<body>Hello world!</body>"
```


##### ...and functions to include css and js...


```r
cat(include.css(c("mysheeet.css", "sheet2.css", "sheet3.css")))
```

```
## <link  type="text/css" href="mysheeet.css" rel="stylesheet" />
## <link  type="text/css" href="sheet2.css" rel="stylesheet" />
## <link  type="text/css" href="sheet3.css" rel="stylesheet" />
```

```r
cat(include.js(c("script1.js", "script2.js", "script3.js")))
```

```
## <script  type="text/javascript" src="script1.js" />
## <script  type="text/javascript" src="script2.js" />
## <script  type="text/javascript" src="script3.js" />
```


##### Full pages with doctypes and outer html tags can be generated with the webdoc function


```r
webdoc("html5",
       head("My first page"),
       body("Hello world",
            unordered.list(elements)))
```

```
## [1] "<!DOCTYPE html><html><head  title=\"My first page\" /><body>Hello world<ul><li>apples</li><li>oranges</li><li>bananas</li></ul></body></html>"
```



##### Still to be implemented:

* Functions for form elements
* Pretty printing

## The static site generator

##### To setup the directories for a new site:


```r
site <- "testsite"
create.site.structure(site)
```


##### To render pages and posts

Pages can be build using the html dsl

```r
render.page(site, "testpage.R")
render.post(site, postname, layout = "default.R")
```


##### To test your site in a development server

Currently I am using python to serve the files, but am working on an R development server based on Rook

To run the dev server (assuming you have Python installed):

```
cd path/to/mysite/mysite
python path/to/samatha/src/server.py
```
Then open your browser and point it to `http://localhost:8000/`


