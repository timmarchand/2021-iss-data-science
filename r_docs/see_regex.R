library(stringr)

see <- function(rx, string = "abc ABC 123\t.!?\\(){}\n") str_view_all(string, rx)
