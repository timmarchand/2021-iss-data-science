## get the function from a source file
source("r_docs/grab_imdb.r")


# specify the imdb code
code <- "tt0141842"
# specify the number of seasons
seasons <- 1:6

# map the grab_imdb function
seasons %>% 
  map_dfr(~grab_imdb(code = code, season = .x))
