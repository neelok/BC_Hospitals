library(shiny)

library(dplyr)

library(leaflet)

library(DT)

shinyServer(function(input, output) {
  # Import Data and clean it
  
  load("cleanData.Rdata")
  Lat <-  df$LATITUDE
  Long <-  df$LONGITUDE
 
  
  # new column for the popup label
  # HEALTH_AUTHORITY
  # HOSPITAL
  # SERVICES
  # PHONE_NUMBER
  # WEBSITE
  # WHEELCHAIR_ACCESSIBLE
  # HOURS
  # ADDRESS
  # CITY
  # POSTAL_CODE
  # LATITUDE
  # LONGITUDE
  
  bb_data <- mutate(df, cntnt=paste0('<strong>Name: </strong>',HOSPITAL,
                                          '<br><strong>City:</strong> ', CITY,
                                          '<br><strong>Phone:</strong> ', PHONE_NUMBER,
                                          '<br><strong>Address:</strong> ',ADDRESS,
                                          '<br><strong>Postal Code:</strong> ',POSTAL_CODE,
                                          '<br><strong>Hours:</strong> ',HOURS,
                                          '<br><strong>Website:</strong> ',WEBSITE)) 

  # create a color paletter for category type in the data file
  
  pal <- colorFactor(pal = c("red", "orange", "yellow", "black", 
                             "blue", "brown", "violet", "purple"), 
                     domain = c("BC Children's Hospital", 
                                "Fraser Health Authority", 
                                "Interior Health Authority",
                                "Vancouver Coastal Health",
                                "Island Health",
                                "Northern Health Authority",
                                "BC Women's Hospital & Health Centre",
                                "Providence Health Care Society"
                                ))
  
  # [1] "BC Children's Hospital"              "Fraser Health Authority"            
  # [3] "Interior Health Authority"           "Vancouver Coastal Health"           
  # [5] "Island Health"                       "Northern Health Authority"          
  # [7] "BC Women's Hospital & Health Centre" "Providence Health Care Society" 
  
  
  
   
  # create the leaflet map  
  output$bbmap <- renderLeaflet({
      leaflet(bb_data) %>% 
      addCircles(lng = ~Long, lat = ~Lat) %>% 
      addTiles() %>%
      addCircleMarkers(data = bb_data, lat =  ~Lat, lng =~Long, 
                       radius = 3, popup = ~as.character(cntnt), 
                       color = ~pal(HEALTH_AUTHORITY),
                       stroke = FALSE, fillOpacity = 0.8)%>%
      addLegend(pal=pal, values=bb_data$HEALTH_AUTHORITY,opacity=1, na.label = "Not Available")%>%
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="ME",
        onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
        })
  
  #create a data object to display data
  
  output$data <-DT::renderDataTable(datatable(
      df[,c(-7, -3)]
  ))

  
})
