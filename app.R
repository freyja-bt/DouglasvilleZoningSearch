#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(sf)
library(lubridate)
library(tmaptools)
library(tigris)
library(httr)
library(memoise)
library(rvest)
library(xml2)
library(jsonlite)

load("www/zoning_data.RData")
load("www/zoning_gis.RData")
source("www/address_standard.R")

# Read data from ArcGIS Online 
## Not using cache
# if(exists("request")==F){
# url <- parse_url("https://services8.arcgis.com/Dujj4RmEh5VlpyET/arcgis/rest/services")
# url$path <- paste(url$path, "CODGIS/FeatureServer/5/query", sep = "/")
# url$query <- list(
#     where = "DV_ZONING_ LIKE '%'",
#     outFields = "*",      
#     returnGeometry = "true",           
#     f = "geojson"
# )
# request <- build_url(url)
# }
# 
# parcel_data <- st_read(request)

## Using cache
if(file.exists("www/.rcache") &
  (read_html("https://services8.arcgis.com/Dujj4RmEh5VlpyET/ArcGIS/rest/services/CODGIS/FeatureServer/5")%>% ### NOTE: This address may need to be updated when the GIS service is hosted from douglasville instead of suburban
        html_element("body")%>%
        html_text2()%>%
        str_extract("(?<=Last Edit Date:)([:graph:]+[:blank:]?)*(?=\n\n)")%>%
        mdy_hms()
    )>=(file.info("www/.rcache")%>%
        .$mtime%>%
        ymd_hms())
   ){
    forget(geo_fun_m)
}


geo_fun <- function(x){
    if(exists("request")==F){
        url <- parse_url("https://services8.arcgis.com/Dujj4RmEh5VlpyET/arcgis/rest/services") ### NOTE: This address may need to be updated when the GIS service is hosted from douglasville instead of suburban
        url$path <- paste(url$path, "CODGIS/FeatureServer/5/query", sep = "/")
        url$query <- list(
            where = x,
            outFields = "*",      
            returnGeometry = "true",           
            f = "geojson"
        )
        request <- build_url(url)
    }
    
    return(st_read(request))
}

geo_fun_m <- memoise(geo_fun, cache = cache_filesystem("www/.rcache"))


parcel_data <- geo_fun_m(x="DV_ZONING_ LIKE '%'")

parcels_tibble <- parcel_data%>%
    st_drop_geometry()%>%
    as_tibble()%>%
    mutate(
        full_address = paste0(str_to_title(ADDRESS), ", ", str_to_title(CITY), ", ", STATE, " ", ZIPCODE)
    )%>%
    left_join(., zoning_dist, by = c("DV_ZONING_" = "abbrev"))%>%
    mutate(
        DV_ZONING1 = if_else(
            DV_ZONING1 != zoning_dist,
            paste0(DV_ZONING1,"*"),DV_ZONING1
        ),
        mismatch = if_else(
            DV_ZONING1 != zoning_dist,
            1,0
        )
    )

parcel_numbers <- parcels_tibble$PIN

addresses <- parcels_tibble$full_address%>%
    unique()

## city confirmation
if(exists("douglas_county_places")==F & exists("douglasco")==F){
ga_places <- places(state = "GA")
ga_counties <- counties(state = "GA")
douglasco <- ga_counties%>%
    filter(
        NAME == "Douglas"
    )
douglas_county_places <- ga_places%>%
    st_intersection(douglasco)
}



ui <- fluidPage(tags$style(type="text/css",".shiny-output-error { visibility: hidden; }",".shiny-output-error:before { visibility: hidden; }"),
                tags$style(type="text/css","#search { top: 50% !important;left: 50% !important;margin-top: -100px !important;margin-left: -250px 
                       !important; color: blue;font-size: 20px;font-style: italic;}"),  
                singleton(
                    tags$head(tags$script(src = "message-handler.js"))
                ),

    # Application title
    titlePanel("Douglasville Zoning Search"),


    sidebarLayout(
        sidebarPanel(
            textInput(
                inputId = "jurSearch",
                label = "Confirm Jurisdiction",
                placeholder = "Search Address"
            ),
            actionButton("jurSubmit", "Search"),
            textOutput(
                "jurAnswer"
            ),
            br(),
    
            selectizeInput(
                inputId = 'search1',
                label = 'Search by Address',
                choices = NULL,
                selected = NULL,
                multiple = TRUE, # allow for multiple inputs
                options = list(create = FALSE) # if TRUE, allows newly created inputs
            ),
            actionButton(
                "submit1",
                "Search"
            ),
            br(),
            helpText(""),
            selectizeInput(
                inputId = 'search2',
                label = 'Search by Parcel Number',
                choices = NULL,
                selected = NULL,
                multiple = TRUE, # allow for multiple inputs
                options = list(create = FALSE) # if TRUE, allows newly created inputs
            ),
            actionButton(
                "submit2",
                "Search",

            ),
            uiOutput("note"),
            helpText(
                "Address and Parcel Number searches only cover Douglasville. To verify jurisdiction, please use the 'Confirm Jurisdiction' search at the top."
            )
            
        ),

        # Show the table
        mainPanel(
            tableOutput('zone_table')
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  options(shiny.maxRequestSize=30*1024^2)
  
  updateSelectizeInput(session, 'search1', choices = addresses, server = TRUE)
  updateSelectizeInput(session, 'search2', choices = parcel_numbers, server = TRUE)
  
    # # Using OSM to check addresses within city
    # observeEvent(input$jurSubmit,{
    #     output$jurAnswer <- renderText({
    #         ""
    #     })
    #     jur <- geocode_OSM(
    #         q = if_else(str_detect(input$jurSearch,"[dD]ouglasville"),
    #                input$jurSearch,
    #                paste0(input$jurSearch,", Douglas County, Georgia, USA")),
    #         as.sf=T,
    #         details = T
    #         )
    #     
    #     
    #     
    #     
    #     if(is.null(jur)){
    #         cat("Search unsuccessful, trying again \n")
    #         jur1 <- geocode_OSM(
    #             q = if_else(str_detect(input$jurSearch,"[dD]ouglasville"),
    #                 paste0(str_extract(input$jurSearch,".*(?=[dD]ouglasville)"),
    #                         ", Douglas County, Georgia, USA"
    #                        ),
    #                 input$jurSearch
    #                 ),
    #             as.sf=T,
    #             details = T
    #         )
    #         if(is.null(jur1)){
    #             cat("Search unsuccessful \n")
    #             output$jurAnswer <- renderText({
    #                 "Please check spelling"
    #                 })
    #         }else{
    #             cat("Search successful, transforming data... \n")
    #             jurProj <- jur1%>%
    #                 st_transform(st_crs(douglasco))
    #     }
    #     }else{
    #         cat("Transforming data... \n")
    #         jurProj <- jur%>%
    #         st_transform(st_crs(douglasco))
    #     }
    #     
    #     
    #     cat("Coverage check... \n")
    #     countyTF <- length(st_covered_by(jurProj,douglasco)[[1]])>0
    #     placeTF <- length(st_covered_by(jurProj,douglas_county_places)[[1]])>0
    #     
    #     if(placeTF==T){
    #         cat("In a city \n")
    #         jurInt <- st_intersection(jurProj,douglas_county_places)
    #         output$jurAnswer <- renderText({
    #             paste("Within", jurInt$NAME)
    #         })
    #         cat(jurInt$NAME)
    # 
    #         cat("\n")
    #         
    #         
    #         if(jurInt$NAME == "Douglasville"){
    #           # cat("\nnice\n")
    #           # observe({
    #           #   req(input$jurSearch)
    #           prvSrch <- input$search1
    #           # 
    #           
    #           srch <- reactiveVal({
    #             # correct street suffix format and capital/lowercase
    #             adapt <- str_to_title(input$jurSearch)%>%
    #               str_replace(.,
    #                           str_extract(.,
    #                                       "[:alpha:]*$"),
    #                           dict$standard[str_which(dict$input,
    #                                                   paste0("^",
    #                                                          str_extract(.,
    #                                                                      "[:alpha:]*$"),
    #                                                          "$")
    #                                                   )]
    #                           )
    #             if(length(adapt)==0){
    #               adapt <- str_to_title(input$jurSearch)
    #               }
    #             adapt
    #             })
    #           
    #           cat(srch())
    #           cat("\n")
    #           # cat(addresses[str_detect(addresses,srch())==T])
    #           cat(sum(str_detect(addresses,srch())==T))
    #           cat("\n")
    #           
    #           observeEvent(input$jurSubmit,{
    #           output$zone_table <- renderTable(
    #             parcels_tibble%>%
    #               filter(
    #                 full_address %in% c(prvSrch,addresses[str_detect(addresses,srch())==T])
    #                 )%>%
    #               select(
    #                 "Parcel Number" = PIN,
    #                 "Address" = ADDRESS,
    #                 "Zoning Code" = DV_ZONING_,
    #                 "Zoning Description" = DV_ZONING1)
    #             )
    # 
    # 
    #           updateSelectizeInput(session, 'search1', selected = c(prvSrch, addresses[str_detect(addresses,srch())==T]), choices = addresses, server = TRUE)
    #           })
    #           # })
    #           
    #           # cat(prvSrch)
    #           # cat("\n")
    #           # cat(srchIndx())
    #           # cat("\n")
    #           # cat(addresses[srchIndx()])
    #           # cat("\n")
    #           
    #           # updateSelectizeInput(session, 'search1', selected = c(prvSrch, addresses[srchIndx()]), options = list(create = FALSE))
    #           
    #           
    #           
    #           
    #         # }else{
    #         #   cat("ope \n")
    #         }
    #         
    #     }else if(countyTF==T){
    #         cat("within county \n")
    #         output$jurAnswer <- renderText({
    #             "Within Douglas County jurisdiction"
    #         }) 
    #     }else{
    #         cat("Outside Douglas County \n")
    #         output$jurAnswer <- renderText({
    #             "Not in Douglas County. If suspect, check spelling and try again."
    #         })
    #     }
    #         
    # })
  
  # Using ArcGIS REST API to check addresses within city
  observeEvent(input$jurSubmit,{
    output$jurAnswer <- renderText({
      ""
      })
    
    jur <- geocode_arc_douglas(input$jurSearch)
    
    if(jur$address == "Douglas County, Georgia"){
      cat("Search unsuccessful")
      output$jurAnswer <- renderText({
        "Entry is either spelled incorrectly or not in Douglas County."
        })
      }else{
        cat("Coverage check... \n")
          
        jurProj <- st_transform(jur, st_crs(douglasco))
        countyTF <- length(st_covered_by(jurProj,douglasco)[[1]])>0
        placeTF <- length(st_covered_by(jurProj,douglas_county_places)[[1]])>0
          
        if(placeTF==T){
          cat("In a city \n")
          jurInt <- st_intersection(jurProj,douglas_county_places)
          output$jurAnswer <- renderText({
            paste("Within", jurInt$NAME)
            })
          cat(jurInt$NAME, "\n")
          
          if(jurInt$NAME == "Douglasville"){
            # cat("\nnice\n")
            
            prvSrch <- input$search1
              
            srch <- reactiveVal({
              # correct street suffix format and capital/lowercase
                
              adapt <- suffix_rep(jur$address)%>%
                str_extract(., ".*(?=(, Douglasville))")
              # if(length(adapt)==0){
              #   adapt <- str_to_title(input$jurSearch)
              #   }
              # adapt
              adapt
              })
              
            cat(srch(),"\n")
            
            cat(sum(str_detect(addresses,srch())==T),"\n")
              
              
            observeEvent(input$jurSubmit,{
              output$zone_table <- renderTable(
                parcels_tibble%>%
                  filter(
                    full_address %in% c(prvSrch,addresses[str_detect(addresses,srch())==T])
                    )%>%
                  select(
                    "Parcel Number" = PIN,
                    "Address" = ADDRESS,
                    "Zoning Code" = DV_ZONING_,
                    "Zoning Description" = DV_ZONING1)
                )
              
              updateSelectizeInput(session, 'search1', selected = c(prvSrch, addresses[str_detect(addresses,srch())==T]), choices = addresses, server = TRUE)
              
            })
            }
          
          }else if(countyTF==T){
            cat("within county \n")
            output$jurAnswer <- renderText({
              "Within Douglas County's jurisdiction"
              }) 
            }else{
              cat("Outside Douglas County \n")
              output$jurAnswer <- renderText({
                "Not in Douglas County or misspelled"
              })
            }
        }
    })
  
  # Zoning Check by Address
  zone1 <- eventReactive(input$submit1,{
    parcels_tibble%>%
      filter(
        full_address %in% input$search1
        )%>%
      select(
        "Parcel Number" = PIN, 
        "Address" = ADDRESS, 
        "Zoning Code" = DV_ZONING_,
        "Zoning Description" = DV_ZONING1)
    })
  
  zone2 <- eventReactive(input$submit2,{
    parcels_tibble%>%
      filter(
        PIN %in% input$search2
        )%>%
      select(
        "Parcel Number" = PIN, 
        "Address" = ADDRESS, 
        "Zoning Code" = DV_ZONING_,
        "Zoning Description" = DV_ZONING1)
    })
  
  observeEvent(input$submit1,{
    output$zone_table <- renderTable(zone1())
    if(sum(str_detect(zone1()$`Zoning Description`,"\\*"),na.rm=T)>0){
      output$note <- renderUI({
        em("*The Zoning Description in the provided data does not match the key in UDO")
        })
      }else{
        output$note <- renderUI({
          
        })
      }
    })
  
  observeEvent(input$submit2,{
    output$zone_table <- renderTable(zone2())
    if(sum(str_detect(zone2()$`Zoning Description`,"\\*"),na.rm=T)>0){
      output$note <- renderUI({
        em("*The Zoning Description in the provided data does not match the key in UDO")
        })
      }else{
        output$note <- renderUI({
          
        })
      }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
