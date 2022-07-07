pacman::p_load(tidyverse,
               rvest, ## for webscraping
               datapasta) ## for dealing with pasted items


## REQUIRED CHROME EXTENSIONS
## SelectorGadget
## Linkclump
## Scraper

## Go to g-port, check out the syllabus page for Prof Inui
## <https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssrch.do>


# paste copied links from Linkclump for Prof Inui ----

## 1 using Linkclump and stringr ----

## While pressing "c" drag capturing square over links
## Paste inside ""

inui <- "社会科学のためのデータ分析	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U510601101&value(crclumcd)=2442017000
経済成長論	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U510701101&value(crclumcd)=2442017000
The Economic Development of Japan	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U510706601&value(crclumcd)=2442017000
Globalization, Economic Growth and Income Distribution	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U510801101&value(crclumcd)=2442017000
専門演習Ⅰ	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U511800103&value(crclumcd)=0900000001
専門演習Ⅱ	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U511801103&value(crclumcd)=0900000001
卒業論文・卒業演習	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U511900102&value(crclumcd)=0900000001"

length(inui)
## just one string, so needs splitting up

## extract course titles on text before https and clear empty strings
course <- inui %>%
  str_extract_all("(.*)(?=https)") %>%
  unlist() #%>%
  # remove empty strings from list
  .[. !=""] %>%
  # tidy up text
  str_squish()
course

## split the string on text before https and clear empty strings
link <-  inui %>%
  str_split("(.*)(?=https)") %>%
  unlist() %>%
  # remove empty strings from list
  .[. !=""] %>%
  # tidy up text
  str_squish()
link

## create tibble with professor name, course, and link to syllabus
inui <-  tibble(professor = "Inui",
                course,
                link)

## 2 using Linkclump and datapasta ----

## While pressing "c" drag capturing square over links
## Click on Addins >> Paste as Tribble:

tibble::tribble(
                                            ~社会科学のためのデータ分析, ~`https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U510601101&value(crclumcd)=2442017000`,
                                                   "経済成長論",  "https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U510701101&value(crclumcd)=2442017000",
                       "The Economic Development of Japan",  "https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U510706601&value(crclumcd)=2442017000",
  "Globalization, Economic Growth and Income Distribution",  "https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U510801101&value(crclumcd)=2442017000",
                                                   "専門演習Ⅰ",  "https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U511800103&value(crclumcd)=0900000001",
                                                   "専門演習Ⅱ",  "https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U511801103&value(crclumcd)=0900000001",
                                               "卒業論文・卒業演習",  "https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U511900102&value(crclumcd)=0900000001"
  )

## editing required! Need to add proper column names
## manually edit to make it look like this

inui2 <- tibble::tribble(
  ~course, ~link,
  "社会科学のためのデータ分析", "https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U510601101&value(crclumcd)=2442017000",
  "経済成長論",  "https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U510701101&value(crclumcd)=2442017000",
  "The Economic Development of Japan",  "https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U510706601&value(crclumcd)=2442017000",
  "Globalization, Economic Growth and Income Distribution",  "https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U510801101&value(crclumcd)=2442017000",
  "専門演習Ⅰ",  "https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U511800103&value(crclumcd)=0900000001",
  "専門演習Ⅱ",  "https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U511801103&value(crclumcd)=0900000001",
  "卒業論文・卒業演習",  "https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U511900102&value(crclumcd)=0900000001"
)

## add professor name, tidy up course name
inui2 <-
  inui2 %>%
  mutate(professor = "Inui", .before = course,
         course = str_squish(course))

## 3 using Scraper and datapasta ----

# Right click on one target link
# Choose Scrape similar...
# Click on Copy to Clipboard
# Click on Addins >> Paste as Tribble:

tibble::tribble(
                                                     ~Link,                                                                                                                    ~URL,
                                           "社会科学のためのデータ分析", "/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U510601101&value(crclumcd)=2442017000",
                                                   "経済成長論", "/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U510701101&value(crclumcd)=2442017000",
                       "The Economic Development of Japan", "/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U510706601&value(crclumcd)=2442017000",
  "Globalization, Economic Growth and Income Distribution", "/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U510801101&value(crclumcd)=2442017000",
                                                   "専門演習Ⅰ", "/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U511800103&value(crclumcd)=0900000001",
                                                   "専門演習Ⅱ", "/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U511801103&value(crclumcd)=0900000001",
                                               "卒業論文・卒業演習", "/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U511900102&value(crclumcd)=0900000001"
  )

# Editing required to match prefered column names!

inui3 <- tibble::tribble(
                                            ~course, ~link,
                                              "社会科学のためのデータ分析", "https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U510601101&value(crclumcd)=2442017000",
                                                   "経済成長論",  "https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U510701101&value(crclumcd)=2442017000",
                       "The Economic Development of Japan",  "https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U510706601&value(crclumcd)=2442017000",
  "Globalization, Economic Growth and Income Distribution",  "https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U510801101&value(crclumcd)=2442017000",
                                                   "専門演習Ⅰ",  "https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U511800103&value(crclumcd)=0900000001",
                                                   "専門演習Ⅱ",  "https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U511801103&value(crclumcd)=0900000001",
                                               "卒業論文・卒業演習",  "https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U511900102&value(crclumcd)=0900000001"
  )

## Add professor name, tidy up course, paste base link address

inui3 <-
  inui3 %>%
  mutate(professor = "Inui", .before = course,
         course = str_squish(course),
         link = paste0("https://g-port.univ.gakushuin.ac.jp/",link))


### check they are the same
all_equal(inui,inui2,inui3)


### scrape reading lists for Prof Inui's course ----
### We need author, year, title, publisher

### use SelectorGadget to find CSS

## author
author_css <- "tr:nth-child(23) tr:nth-child(4) .cell_border_y~ .cell_border_y+ .cell_border_y , .syllkmok tr:nth-child(12) .cell_border_y~ .cell_border_y+ .cell_border_y , .syllkmok tr:nth-child(4) .cell_border_y~ .cell_border_y+ .cell_border_y , tr:nth-child(23) tr:nth-child(12) .cell_border_y~ .cell_border_y+ .cell_border_y"
## year
year_css <- "tr:nth-child(23) tr:nth-child(16) .cell_border_y:nth-child(3) , tr:nth-child(23) tr:nth-child(8) .cell_border_y:nth-child(3) , .syllkmok tr:nth-child(16) .cell_border_y:nth-child(3) , .syllkmok tr:nth-child(8) .cell_border_y:nth-child(3)"
## title
title_css <- ".syllkmok tr:nth-child(4) .cell_border_y:nth-child(1) , .syllkmok tr:nth-child(12) .cell_border_y:nth-child(1) , tr:nth-child(23) tr:nth-child(12) .cell_border_y:nth-child(1) , tr:nth-child(23) tr:nth-child(4) .cell_border_y:nth-child(1)"
## publisher
publisher_css <- "tr:nth-child(23) tr:nth-child(16) .cell_border_y:nth-child(1) , .syllkmok tr:nth-child(8) .cell_border_y:nth-child(1) , .syllkmok tr:nth-child(16) .cell_border_y:nth-child(1) , tr:nth-child(23) tr:nth-child(8) .cell_border_y:nth-child(1)"

## test with one link

beta <-
inui2 %>%
  pull(link) %>%
  .[3]

html <- read_html(beta)
author <- html %>%
          html_nodes(author_css) %>%
          html_text(trim = TRUE)

year <- html %>%
    html_nodes(year_css) %>%
    html_text() %>%
    parse_number()

title<- html %>%
  html_nodes(title_css) %>%
  html_text(trim = TRUE)

publisher <- html %>%
  html_nodes(publisher_css) %>%
  html_text(trim = TRUE)

tibble(author,year,title,publisher)

## map for all links in inui df

reading <-
inui2 %>%
  mutate(
    author = map(link, ~read_html(.x)) %>%
      map(html_nodes, author_css) %>%
      map(html_text) %>%
      map(str_squish),
    year = map(link, ~read_html(.x)) %>%
      map(html_nodes, year_css) %>%
      map(html_text) %>%
      map(parse_number),
    title = map(link, ~read_html(.x)) %>%
      map(html_nodes, title_css) %>%
      map(html_text) %>%
      map(str_squish),
    publisher = map(link, ~read_html(.x)) %>%
      map(html_nodes, publisher_css) %>%
      map(html_text) %>%
      map(str_squish))

## this has now created 3 new list columns
## to see the details we need to unnest the list columns

reading %>%
  # remove link column as longer needed
  select(-link) %>%
  unnest(cols = c(author,year,title,publisher))

## CHALLENGE ----
## Get a full list of reading materials for ISS social science courses taught in English

## Grab the data for all ISS social science teachers
## filter for only those courses taught in English
## scrape the reading lists for each course taught in English
