context("HTML generation")

test_that("Generic html function works",{
    expect_output(m("p"), "<p />")
    expect_equal(render.opts(list(id = "foo", class = "bar")), " id=\"foo\" class=\"bar\"")
    # Any strings after that become the content of the tag
    expect_equal(m("p", "This is a paragraph"), "<p>This is a paragraph</p>")
    # Tags can be nested inside of tags and everything ends up concatenated
    expect_equal(m("p", "Goodbye", m("strong", "cruel"), "world"), "<p>Goodbye<strong>cruel</strong>world</p>")
    # You can specify attributes by supplying a list after the content
    expect_equal(m("span",  "bar", opts = list(class = "foo")), "<span class=\"foo\">bar</span>")
    #' There are CSS-style shortcuts for setting ID and class
    expect_equal(m("p#my-p", m("span.pretty", "hey")), "<p id=\"my-p\"><span class=\"pretty\">hey</span></p>")
    expect_equal(m("span", opts = list(id = "foo", class = "bar"), "baz"), "<span id=\"foo\" class=\"bar\">baz</span>")
    # You can escape a string using the (escape-html) function
    expect_equal(m("p", m("script", "Do something evil", escape.html.p = TRUE)), "<p>&lt;script&gt;Do something evil&lt;/script&gt;</p>")
    # container tags have explicit ending tags even when empty
    expect_equal(m("div"), "<div></div>")
    expect_equal(m("div.span8"), "<div class=\"span8\"></div>")
})

test_that("html element wrappers work",{
    expect_equal(javascript.tag("Some javascript"), "<script>//<![CDATA[\nSome javascript\n//]]></script>")
    expect_equal(link.to("www.google.com", "Google"), "<a href=\"www.google.com\">Google</a>")
    expect_equal(mail.to("me@me.com"), "<a href=\"mailto:me@me.com\">me@me.com</a>")
    expect_equal(mail.to("me@me.com", "email me"), "<a href=\"mailto:me@me.com\">email me</a>")
    expect_equal(mail.to("me@me.com", "email me", subject = "you are great"), 
                 "<a href=\"mailto:me@me.com?Subject=you%20are%20great\">email me</a>")
    elements = list("apples", "oranges", "bananas")
    expect_equal(unordered.list(elements,
                                list.opts = list(class = "nav pills")), 
                 "<ul class=\"nav pills\"><li>apples</li><li>oranges</li><li>bananas</li></ul>")
    expect_equal(ordered.list(elements, element.opts = list(class = "blah")), 
                 "<ol><li class=\"blah\">apples</li><li class=\"blah\">oranges</li><li class=\"blah\">bananas</li></ol>")
    expect_equal(image.link("www.beautifulthings.com/12538675", opts = list(alt = "A lovely picture of something")), 
                 "<img  src=\"www.beautifulthings.com/12538675\" alt=\"A lovely picture of something\" />")
})
