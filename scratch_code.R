





test3 <- geocode_OSM("6701 church st douglasville ga",
                     as.sf=T,
                     details = T)
ex <- test3$display_name
ex

str_extract_all(ex,"([:blank:]?[:alnum:]+)*(?=,)")


parcels_tibble%>%
  filter(
    HOUSENUM == str_extract(ex,"[:graph:]+(?=,)"),
    ADDRESS == str_extract(ex,"(?<=,)([:blank:]?[:graph:]+[^,])*(?=,)")
  )
str_view_all(ex,"([:blank:]?[:graph:]+[^,])*(?=,)")
str_view_all(ex,"[:graph:]+(?=, )")

str_view_all(ex,"(?<=,)([:blank:]?[:graph:][^,]+)*(?=,)")
remotes::install_github("slu-openGIS/postmastr")

ex1 <- str_split(ex, ", ")%>%
  unlist

test3%>%
  st_drop_geometry()%>%
  .["display_name"]%>%
  str_split(., ", ")%>%
  unlist%>%
  .[1:2]%>%
  paste(.,collapse = " ")%>%
  str_replace(.,
              str_extract(.,
                          "[:alpha:]*$"),
              dict$standard[str_which(dict$input,
                                      paste0("^",
                                             str_extract(.,
                                                         "[:alpha:]*$"),
                                             "$")
              )]
  )

## randomized code testing
test_addresses <- read.delim(url("https://gist.githubusercontent.com/HeroicEric/1102788/raw/0bcb298bd75513a398bf353ce7162177350813c9/gistfile1.txt"),col.names = F)%>%unlist(.,use.names = F)
  
set.seed(78)
nums <- sample(seq_along(test_addresses),75,replace = F)

new_test <- geocode_OSM(test_addresses,
                        as.sf=T,
                        details = T)
new_test1 <- new_test%>%
  st_drop_geometry()%>%
  select(display_name)%>%
  pull

map(new_test1,function(x){
  cat(x,"\n")
  i <- str_which(str_split(x,", ")%>%
                   unlist,"^[:digit:]")[1]

  x%>%
    str_split(., ", ")%>%
    unlist%>%
    .[i:(i+1)]%>%
    paste(.,collapse = " ")%>%
    str_replace(.,
                str_extract(.,
                            "[:alpha:]*$"),
                dict$standard[str_which(dict$input,
                                        paste0("^",
                                               str_extract(.,
                                                           "[:alpha:]*$"),
                                               "$")
                )]
    )

})

## currently doesn't know what to do with addresses on "Hwy 5" or other roads with a number at the end

url <- parse_url("https://geocode.arcgis.com/arcgis/rest/services/World/GeocodeServer/findAddressCandidates?")

url <- "https://geocode.arcgis.com/arcgis/rest/services/World/GeocodeServer/findAddressCandidates?f=json&address=9419%20Hwy%205%20Douglasville%20GA"

install.packages("rjson")
library(rjson)

searchResult <- fromJSON(file = url(url))

searchResult%>%
  flatten_dfc()%>%
  st_as_sf(coords = c("x","y"),crs=.$wkid)

# Test
q <-  if_else(str_detect(x,"[dD]ouglasville"),
              x,
              paste0(x,", Douglas County, GA"))
  
  rest_url <- "https://geocode.arcgis.com/arcgis/rest/services/World/GeocodeServer/findAddressCandidates?f=json&address="
rest_url <- paste0(rest_url, str_replace_all(q," ","%20"))

searchResult <- fromJSON(file = url(rest_url))
closeAllConnections()
jur <- searchResult%>%
  flatten_dfc()%>%
  st_as_sf(coords = c("x","y"),crs=.$wkid)

# Looking to replace the full word but only if it is the full word. so if it is surrounded by spaces or the end of the string...
match(1,str_detect(str_to_title("gravenue avenue"), paste0(" ", dict$input)))

test <- str_to_title("gravenue ridge avenue")

str_which(test, paste0("\\b", dict$input, "\\b"))

placement <- str_locate(test,dict$input[str_which(test, paste0("\\b", dict$input, "\\b"))])

wordLoc <- apply(placement,2,max)

str_replace(test,str_sub(test, wordLoc[1],wordLoc[2]),dict$standard[str_which(dict$input,str_sub(test, wordLoc[1],wordLoc[2]))])


