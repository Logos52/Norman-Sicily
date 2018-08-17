#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

server<-function(input, output, session) {
  
  ## Interactive Map ###########################################
  
  # Create the map
  
  # pal <- colorFactor(
  #   palette = 'Dark2',
  #   domain = norman$Province
  # )
  colorpal <- reactive({
    input$colors})
 bins <- c(1, 4, 10, 20, 35, 50)
 
 # colorpal <- reactive({
 #   colorBin(palette=input$colors, domain = shape2$joindata.Count, bins=bins)
 # })
  #qpal <- colorBin("YlOrRd", domain = shape2$joindata.Count, bins=bins)
  labels2 <- paste("Province:", shape2$region, "<br>", 
                   "Count:", shape2$joindata.Count)%>% 
    lapply(htmltools::HTML)
  
  output$map <- renderLeaflet({
  #   leaflet() %>%
  #     setView(14, 37.5, 8) %>% 
  #     addTiles() %>%
  #     addCircles(data=norman, lng=norman$Longitude, lat=norman$Latitude, weight = 6,
  #                popup = paste("Name:", norman$Name, "<br>",
  #                              "Province:", norman$Province, "<br>",
  #                              "Order:", norman$Order, "<br>",
  #                              "Dedications:", norman$Dedication, "<br>",
  #                              "Seismic Classification:", norman$Seismic), 
  #                label = ~as.character(Name), color = ~pal(Province)) %>%
  #                addLegend("bottomleft", pal=pal, values = norman$Province, title = "Province", layerId="colorLegend")
  # })
  # 
  # output$map2 <- renderLeaflet({
    leaflet(data = shape2) %>%
    setView(14, 37.5, 8
            ) %>% 
    addTiles() 
  })
  
  proxy <- leafletProxy("map", data = shape2)
  
  observe({
   orderBy <- input$mapstyle
   qpal<-colorpal()
   if(orderBy == "Points") {
     pal <- colorFactor(palette = qpal,
       domain = norman$Province)
    proxy %>% clearControls() %>%
      clearShapes()  %>%
      addPolygons(fill=T, fillColor = ~pal(norman$Province), weight=1,
                  color="black",
                  opacity = 1,
                  dashArray = "1",
                  fillOpacity = 0.1) %>%
       addCircleMarkers(data=norman, lng=norman$Longitude, lat=norman$Latitude, radius = 5, weight=5, color="red",
                  opacity = 0.9, label = ~as.character(Name),
                  popup = paste("Name:", norman$Name, "<br>",
                  "Province:", norman$Province, "<br>",
                  "Order:", norman$Order, "<br>",
                  "Dedications:", norman$Dedication, "<br>",
                  "Seismic Classification:", norman$Seismic))
      # addMarkers(data=norman, lng=norman$Longitude, lat=norman$Latitude,
      #            popup = paste("Name:", norman$Name, "<br>",
      #                          "Province:", norman$Province, "<br>",
      #                          "Order:", norman$Order, "<br>",
      #                          "Dedications:", norman$Dedication, "<br>",
      #                          "Seismic Classification:", norman$Seismic),
      #            label = ~as.character(Name))
   }
    
    if(orderBy == "Chloro") {
      qpal <- colorpal()
      pal <- colorBin(palette=qpal, domain = shape2$joindata.Count, bins=bins)
    
      proxy %>% clearControls() %>%
        clearShapes() %>%
        clearMarkers() %>%
        addPolygons(fill=T, fillColor = ~pal(joindata.Count), weight = 1,
                    opacity = 1,
                    color = "black",
                    dashArray = "1",
                    fillOpacity = 0.7,
                    highlight = highlightOptions(
                      weight = 3,
                      color = "white",
                      dashArray = "",
                      fillOpacity = 0.7,
                      bringToFront = TRUE),
                    label = labels2,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "12px",
                      direction = "auto")) %>%
        addLegend(pal = pal, values = ~joindata.Count, opacity = 0.7, title = "Count",
                  position = "bottomright")
    }
   
   if(orderBy == "Seismic") {
     qpal <- colorpal()
     factpal <- colorFactor(palette=qpal, shape$seismic)
     
     labels <- paste("Province:", shape$NAME_2, "<br>", 
                     "Commune:", shape$NAME_3, "<br>",
                     "Seismic Zone:", shape$test.seismic)%>% 
       lapply(htmltools::HTML)
     proxy %>% clearControls() %>%
       clearShapes()%>%clearMarkers()
     
     leafletProxy("map", data = shape) %>%
       setView(14, 37.5, 8) %>% 
       addTiles() %>%
       addPolygons(fill=T, fillColor = ~factpal(test.seismic), weight = 1,
                   opacity = 1,
                   color = "black",
                   dashArray = "1",
                   fillOpacity = 0.7,
                   highlight = highlightOptions(
                     weight = 3,
                     color = "white",
                     dashArray = "",
                     fillOpacity = 0.7,
                     bringToFront = TRUE),
                   label = labels,
                   labelOptions = labelOptions(
                     style = list("font-weight" = "normal", padding = "3px 8px"),
                     textsize = "12px",
                     direction = "auto")) %>% addLegend(pal = factpal, values = ~test.seismic, opacity = 0.7, title = "Seismic",
                     position = "bottomright")
   }
  })

  observe({
    if (is.null(input$goto))
      return()
    isolate({
      map3 <- leafletProxy("map")
      map3 %>% clearPopups()
      dist <- 0.01
      lat <- input$goto$lat
      lng <- input$goto$lng
      map3 %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    })
  })
    
  output$cleantable <- DT::renderDataTable({
    df <- cleantable %>% mutate(Action = paste('<a class="go-map" href="" data-lat="', Latitude, '" data-long="', Longitude, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    action <- DT::dataTableAjax(session, df)
    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  })
}


