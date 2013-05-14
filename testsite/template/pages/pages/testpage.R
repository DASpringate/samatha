layout <- "default.R"
title <- "testpage"

elements = list("apples", "oranges", "bananas", "mangos", "starfruit", "pears")
page <- content(m("h1", "Hello world"),
                "This is a list of fruits:\n",
                unordered.list(elements))