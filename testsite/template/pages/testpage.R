layout <- "default.R"
title <- "home"

elements = list("apples", "oranges", "bananas")
page <- content(m("h1", "Hello world"),
                "This is a list of fruits:\n",
                unordered.list(elements))