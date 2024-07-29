# # Address Standardization
# library(tidyverse)
# library(sf)
# library(rvest)
# library(xml2)
# library(jsonlite)

# Tibble of address suffixes
dict <- read_html("https://pe.usps.com/text/pub28/28apc_002.htm")%>%
  html_element(".Basic_no_title")%>%
  html_table(header=T)%>%
  rename(
    "name" = `PrimaryStreet SuffixName`,
    "common" = `CommonlyUsed StreetSuffix orAbbreviation`,
    "standard" = `Postal ServiceStandardSuffixAbbreviation`
  )%>%
  mutate(
    across(
      .fns = str_to_title
    )
  )%>%
  pivot_longer(cols = c(name,common),names_to = "names",values_to = "input")%>%
  select(-names)%>%
  distinct%>%
  bind_rows(
    tibble(
      input = "Ga",
      standard = "Hwy"
    )
  )

# Geocoding addresses using ArcGIS REST API
geocode_arc_douglas <- function(address){
  x <- address
  q <-  if_else(str_detect(x,"[dD]ouglasville"), # This is because every address into the system will be from Douglasville, bc the whole point is that we're searching for addresses in Douglasville...
                # Not quite sure how to make it so you could search any address and still correct for incomplete addresses without a suggestion system, which I don't know how to implement
                x,
                paste0(x,", Douglas County, GA"))
  # This only works with addresses because of the "&address=" call in our url. You could alter it to take multiple or different inputs by changing the parameters, info at https://developers.arcgis.com/rest/geocode/api-reference/geocoding-find-address-candidates.htm
  
  rest_url <- "https://geocode.arcgis.com/arcgis/rest/services/World/GeocodeServer/findAddressCandidates?f=json&address="
  rest_url <- paste0(rest_url, str_replace_all(q," ","%20")) # I haven't run into other changes that would need to be implemented, but changing ' ' to '%20' is the only one I've really looked into.
  

  jur <- jsonlite::fromJSON(rest_url) %>%
    purrr::map(
      \(x) as_tibble(x)
    ) %>%
    purrr::list_cbind() %>%
    first()%>%
    tidyr::unnest(cols = c(spatialReference, candidates)) %>%
    tidyr::unnest(cols = c(location, extent))%>%
    dplyr::select(wkid, address, x, y)%>%
   sf::st_as_sf(coords = c("x","y"),crs=.$wkid)
  
  return(jur)
  
  }

# Replacing non-standard and non-abbreviated street suffixes with the standard abbreviation
# test <- str_to_title("gravenue ridge avenue")

suffix_rep <- function(x, dictionary = dict){ # Where 'x' is the address you're seaching and dict is a data frame with column "input" of all street suffixes and "standard" of the equivalent standard suffix. Standard suffixes need to be in both columns. 
  x <- str_to_title(x)
  placement <- str_locate(x, dictionary$input[str_which(x, paste0("\\b", dictionary$input, "\\b"))])

  wordLoc <- apply(placement,2,max)

  new_add <- str_replace(
    x,
    str_sub(x, wordLoc[1], wordLoc[2]),
    dictionary$standard[
      str_which(dictionary$input,
                paste0("\\b", str_sub(x, wordLoc[1], wordLoc[2]), "\\b")
                )
      ]
    )%>%
    str_replace(
      "Hwy-",
      "Hwy "
    )
  
  return(new_add)
  
}