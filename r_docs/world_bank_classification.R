### World Bank country classifications

##<https://data.worldbank.org/country>

## cclumplink and stringr ----

## While pressing "c" drag capturing square over links
## Paste inside ""

clump <- "East Asia & Pacific	https://data.worldbank.org/region/east-asia-and-pacific?view=chart
Europe & Central Asia	https://data.worldbank.org/region/europe-and-central-asia?view=chart
Latin America & Caribbean	https://data.worldbank.org/region/latin-america-and-caribbean?view=chart
Middle East & North Africa	https://data.worldbank.org/region/middle-east-and-north-africa?view=chart
North America	https://data.worldbank.org/region/north-america?view=chart
South Asia	https://data.worldbank.org/region/south-asia?view=chart
Sub-Saharan Africa	https://data.worldbank.org/region/sub-saharan-africa?view=chart
High income	https://data.worldbank.org/income-level/high-income?view=chart
Low & middle income	https://data.worldbank.org/income-level/low-and-middle-income?view=chart
Low income	https://data.worldbank.org/income-level/low-income?view=chart
Lower middle income	https://data.worldbank.org/income-level/lower-middle-income?view=chart
Middle income	https://data.worldbank.org/income-level/middle-income?view=chart
Upper middle income	https://data.worldbank.org/income-level/upper-middle-income?view=chart
IBRD only	https://data.worldbank.org/region/ibrd-only?view=chart
IDA blend	https://data.worldbank.org/region/ida-blend?view=chart
IDA only	https://data.worldbank.org/region/ida-only?view=chart
Early-demographic dividend	https://data.worldbank.org/region/early-demographic-dividend?view=chart
Late-demographic dividend	https://data.worldbank.org/region/late-demographic-dividend?view=chart
Post-demographic dividend	https://data.worldbank.org/region/post-demographic-dividend?view=chart
Pre-demographic dividend	https://data.worldbank.org/region/pre-demographic-dividend?view=chart
Caribbean small states	https://data.worldbank.org/region/caribbean-small-states?view=chart
Other small states	https://data.worldbank.org/region/other-small-states?view=chart
Pacific island small states	https://data.worldbank.org/region/pacific-island-small-states?view=chart
Small states	https://data.worldbank.org/region/small-states?view=chart
Arab World	https://data.worldbank.org/region/arab-world?view=chart
Central Europe and the Baltics	https://data.worldbank.org/region/central-europe-and-the-baltics?view=chart
East Asia & Pacific (excluding high income)	https://data.worldbank.org/region/east-asia-and-pacific-excluding-high-income?view=chart
Euro area	https://data.worldbank.org/region/euro-area?view=chart
Europe & Central Asia (excluding high income)	https://data.worldbank.org/region/europe-and-central-asia-excluding-high-income?view=chart
European Union	https://data.worldbank.org/region/european-union?view=chart
Fragile and conflict affected situations	https://data.worldbank.org/region/fragile-and-conflict-affected-situations?view=chart
Heavily indebted poor countries (HIPC)	https://data.worldbank.org/region/heavily-indebted-poor-countries-hipc?view=chart
Latin America & Caribbean (excluding high income)	https://data.worldbank.org/region/latin-america-and-caribbean-excluding-high-income?view=chart
Least developed countries: UN classification	https://data.worldbank.org/region/least-developed-countries-un-classification?view=chart
Middle East & North Africa (excluding high income)	https://data.worldbank.org/region/middle-east-and-north-africa-excluding-high-income?view=chart
OECD members	https://data.worldbank.org/region/oecd-members?view=chart
Sub-Saharan Africa (excluding high income)	https://data.worldbank.org/region/sub-saharan-africa-excluding-high-income?view=chart
World	https://data.worldbank.org/region/world?view=chart
"

## extract course titles on text before https and clear empty strings
classification <- clump %>%
  str_extract_all("(.*)(?=https)") %>%
  unlist() %>%
  # remove empty strings from list
  .[. !=""] %>%
  # tidy up text
  str_squish()
classification

## split the string on text before https and clear empty strings
link <-  clump %>%
  str_split("(.*)(?=https)") %>%
  unlist() %>%
  # remove empty strings from list
  .[. !=""] %>%
  # tidy up text
  str_squish()
link

# how many classified by region?
# how many classified by income_level?
# how many classified by lending
# how many classified by demographic
# how many classified by small_states?
# how many classified by other?

type <- c(rep("region",7),
          rep("income_level",6),
          rep("lending",3),
          rep("demographic",4),
          rep("small_states", 4),
          rep("other", 14))
type

df <- tibble(type,classification,link)


# scraper way --------------------------------------------------------------

# Right click on one target link
# Choose Scrape similar...
# Click on Copy to Clipboard
# Click on Addins >> Paste as Tribble:

regions <- tibble::tribble(
                                    ~Link,                                              ~URL,
                    "East Asia & Pacific",        "/region/east-asia-and-pacific?view=chart",
                  "Europe & Central Asia",      "/region/europe-and-central-asia?view=chart",
              "Latin America & Caribbean",  "/region/latin-america-and-caribbean?view=chart",
             "Middle East & North Africa", "/region/middle-east-and-north-africa?view=chart",
                          "North America",                "/region/north-america?view=chart",
                             "South Asia",                   "/region/south-asia?view=chart",
                     "Sub-Saharan Africa",           "/region/sub-saharan-africa?view=chart"
             ) %>%
  mutate(type = "region")

income_levels <- tibble::tribble(
                                   ~Link,                                             ~URL,
                           "High income",           "/income-level/high-income?view=chart",
                   "Low & middle income", "/income-level/low-and-middle-income?view=chart",
                            "Low income",            "/income-level/low-income?view=chart",
                   "Lower middle income",   "/income-level/lower-middle-income?view=chart",
                         "Middle income",         "/income-level/middle-income?view=chart",
                   "Upper middle income",   "/income-level/upper-middle-income?view=chart"
                   ) %>%
  mutate(type = "income_level")

lending <- tibble::tribble(
                          ~Link,                           ~URL,
                    "IBRD only", "/region/ibrd-only?view=chart",
                    "IDA blend", "/region/ida-blend?view=chart",
                     "IDA only",  "/region/ida-only?view=chart"
                    ) %>%
  mutate(type = "lending")

demographic <- tibble::tribble(
                                             ~Link,                                            ~URL,
                      "Early-demographic dividend", "/region/early-demographic-dividend?view=chart",
                       "Late-demographic dividend",  "/region/late-demographic-dividend?view=chart",
                       "Post-demographic dividend",  "/region/post-demographic-dividend?view=chart",
                        "Pre-demographic dividend",   "/region/pre-demographic-dividend?view=chart"
                      ) %>%
  mutate(type = "demographic")

small_states <- tibble::tribble(
                                          ~Link,                                             ~URL,
                       "Caribbean small states",      "/region/caribbean-small-states?view=chart",
                           "Other small states",          "/region/other-small-states?view=chart",
                  "Pacific island small states", "/region/pacific-island-small-states?view=chart",
                                 "Small states",                "/region/small-states?view=chart"
                  ) %>%
  mutate(type = "small_states")

other <- tibble::tribble(
                                                          ~Link,                                                                    ~URL,
                                                   "Arab World",                                         "/region/arab-world?view=chart",
                               "Central Europe and the Baltics",                     "/region/central-europe-and-the-baltics?view=chart",
                  "East Asia & Pacific (excluding high income)",        "/region/east-asia-and-pacific-excluding-high-income?view=chart",
                                                    "Euro area",                                          "/region/euro-area?view=chart",
                "Europe & Central Asia (excluding high income)",      "/region/europe-and-central-asia-excluding-high-income?view=chart",
                                               "European Union",                                     "/region/european-union?view=chart",
                     "Fragile and conflict affected situations",           "/region/fragile-and-conflict-affected-situations?view=chart",
                       "Heavily indebted poor countries (HIPC)",               "/region/heavily-indebted-poor-countries-hipc?view=chart",
            "Latin America & Caribbean (excluding high income)",  "/region/latin-america-and-caribbean-excluding-high-income?view=chart",
                 "Least developed countries: UN classification",        "/region/least-developed-countries-un-classification?view=chart",
           "Middle East & North Africa (excluding high income)", "/region/middle-east-and-north-africa-excluding-high-income?view=chart",
                                                 "OECD members",                                       "/region/oecd-members?view=chart",
                   "Sub-Saharan Africa (excluding high income)",           "/region/sub-saharan-africa-excluding-high-income?view=chart",
                                                        "World",                                              "/region/world?view=chart"
           ) %>%
  mutate(type = "other")

dat <-
  bind_rows(regions,income_levels,lending,demographic,small_states,other) %>%
  set_names(c("classification", "link","type")) %>%
  mutate(link = paste0("https://data.worldbank.org",link)) %>%
  select(type,everything())

all_equal(dat,df)

# scrape countries --------------------------------------------------------

### use SelectorGadget to find CSS
country_css <- ""

## test on small sample
dat[1:3,] %>%
  mutate(country = map(link, ~read_html(.x)) %>%
           map(html_nodes,country_css)%>%
           map(html_text,trim = TRUE)) %>%
  select(-link) %>%
  unnest(cols = country)

## seems to work
## full set

world_bank_classification <-
  dat %>%
  mutate(country = map(link, ~read_html(.x)) %>%
           map(html_nodes,country_css)%>%
           map(html_text,trim = TRUE)) %>%
  select(-link) %>%
  unnest(cols = country)


world_bank_classification %>%
  filter(country == "Comoros")


## build a quick function

classify <- function(x){
  world_bank_classification %>%
    filter(country == {{x}})
}

classify("France")


