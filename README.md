Samatha
=======

#### Copyright David Sprigate 2013 ([CC BY 3.0](creativecommons.org/licenses/by/3.0))
##### @datajujitsu

Samatha is a package for rendering HTML in R. It is based on the [Hiccup](http://github.com/weavejester/hiccup) library for [Clojure](clojure.org).

Samatha can render html and generic xml to R strings.  The functions can be nested inside one another and are designed to be combined to easily build valid html in R.

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

## Syntax

##### The central function is html:


```r
html("p") # The first argument is the html tag:
```

```
## [1] "<p  />"
```

```r
html("p", "This is a Sentence.", " So is this") # Any strings after form the content of the tag:
```

```
## [1] "<p>This is a Sentence. So is this</p>"
```

```r
html("span",  "bar", opts = list(class = "foo")) # The opts list defines html tag attributes
```

```
## [1] "<span class=\"foo\">bar</span>"
```

```r
html("span", opts = list(id = "foo", class = "bar"), "baz") 
```

```
## [1] "<span id=\"foo\" class=\"bar\">baz</span>"
```

```r
html("p", 
     "Goodbye", 
     html("strong", "cruel"), 
     "world")# Tags can be nested inside of other tags
```

```
## [1] "<p>Goodbye<strong>cruel</strong>world</p>"
```

```r
html("p#my-p", html("span.pretty", "hey")) # CSS-style shortcuts for ID and class
```

```
## [1] "<p id=\"my-p\"><span class=\"pretty\">hey</span></p>"
```

```r
html("p", html("script", "Do something evil", escape.html.p = TRUE)) # Escape a tag using escape.html.p = TRUE
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


##### ...and functions to include css and js...


```r
include.css(c("mysheeet.css", "sheet2.css", "sheet3.css"))
```

```
## [1] "<link  type=\"text/css\" href=\"mysheeet.css\" rel=\"stylesheet\" />\n<link  type=\"text/css\" href=\"sheet2.css\" rel=\"stylesheet\" />\n<link  type=\"text/css\" href=\"sheet3.css\" rel=\"stylesheet\" />"
```

```r
include.js(c("script1.js", "script2.js", "script3.js"))
```

```
## [1] "<script  type=\"text/javascript\" src=\"script1.js\" />\n<script  type=\"text/javascript\" src=\"script2.js\" />\n<script  type=\"text/javascript\" src=\"script3.js\" />"
```


##### Still to be implemented:

* Functions for form elements
* Doctype boilerplate






