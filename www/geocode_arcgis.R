# ArcGIS Geocode

geocode_arcgis <- function(address, flat = TRUE, spatial = FALSE) {
  x <- address
  # This only works with addresses because of the "&address=" call in our url. You could alter it to take multiple or different inputs by changing the parameters, info at https://developers.arcgis.com/rest/geocode/api-reference/geocoding-find-address-candidates.htm

  rest_url <- "https://geocode.arcgis.com/arcgis/rest/services/World/GeocodeServer/findAddressCandidates?f=json&address="
  rest_url <- paste0(rest_url, stringr::str_replace_all(x, " ", "%20")) # I haven't run into other changes that would need to be implemented, but changing ' ' to '%20' is the only one I've really looked into.

  jur_list <- jsonlite::fromJSON(rest_url)%>%
    append(
      list(data.frame("request"=x))
    )

  if (flat == FALSE) {
    return(jur_list)
  } else {
    jur <- jur_list %>%
      purrr::map(
        \(x) as_tibble(x)
      ) %>%
      purrr::list_cbind() %>%
      tidyr::unnest(cols = c(spatialReference, candidates)) %>%
      tidyr::unnest(cols = c(location, extent))
    if (spatial == TRUE) {
      wkid <- unique(jur$wkid)
      jur <- jur %>%
        sf::st_as_sf(coords = c("x", "y"), crs = wkid)
    }

    return(jur)
  }
}