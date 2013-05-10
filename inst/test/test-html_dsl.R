context("HTML generation")

test_that("Generic html function works",{
    expect_output(m("p"), "<p  />")
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
})

