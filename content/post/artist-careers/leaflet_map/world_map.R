world_map <- function(.nodes, .artists){
  # nodes <- .ranked_nodes %>% filter(year == .year) %>%
  #     select(name_exhplace, type_exhplace, venue_percentile, lat, lon)
  
  
  
  museums <- .nodes %>%
    dplyr::filter(type_exhplace == "Museum")
  galleries <- nodes %>%
    dplyr::filter(type_exhplace == "Gallery")
  nonprofits <- .nodes %>%
    dplyr::filter(type_exhplace == "Non-Profit")
  collectors <- .nodes %>%
    dplyr::filter(type_exhplace == "Collector")
  
  pal <- leaflet::colorNumeric(palette = "Reds", domain = c(0,100), reverse = F)
  
  
  # artists <- .artists %>%
  #   dplyr::semi_join(.career_paths, by = c("id"="artist_id")) %>%
  #   dplyr::filter(!is.na(lat_bp), !is.na(lon_bp))
  

  m <- leaflet::leaflet(width = "100%",
                        options = leaflet::leafletOptions(minZoom = 1.3)
  ) %>%
    # Add the OSM, CartoDB and Esri tiles
    leaflet::addTiles(group = "OSM") %>%
    leaflet::addProviderTiles("CartoDB", group = "Carto") %>%
    leaflet::addProviderTiles("Esri", group = "Esri") %>%
    
    # add artist points
    leaflet::addCircleMarkers(data = .artists, radius = 1.25,
                              lng = ~ lon_bp, lat = ~lat_bp,
                              #~ pal(venue_percentile),
                              label = ~ name,
                              #popup = ~ paste0("<b>", name_exhplace,"</b>","<br/>", type_exhplace),
                              group = "Artist") %>%
    
    # add museum points
    leaflet::addCircleMarkers(data = museums, radius = 1.25,
                              lng = ~ lon, lat = ~lat,
                              color = ~ pal(venue_percentile),
                              label = ~ name_exhplace,
                              #popup = ~ paste0("<b>", name_exhplace,"</b>","<br/>", type_exhplace),
                              group = "Museum") %>%
    # add gallery points
    leaflet::addCircleMarkers(data = galleries, radius = 1.25,
                              lng = ~ lon, lat = ~lat,
                              color = ~ pal(venue_percentile),
                              label = ~ name_exhplace,
                              #popup = ~ paste0("<b>", name_exhplace,"</b>","<br/>", type_exhplace),
                              group = "Gallery") %>%
    # add non profit points
    leaflet::addCircleMarkers(data = nonprofits, radius = 1.25,
                              lng = ~ lon, lat = ~lat,
                              color = ~ pal(venue_percentile),
                              label = ~ name_exhplace,
                              #popup = ~ paste0("<b>", name_exhplace,"</b>","<br/>", type_exhplace),
                              group = "Non-Profit") %>%
    # add collector points
    leaflet::addCircleMarkers(data = collectors, radius = 1.25,
                              color = ~ pal(venue_percentile),
                              lng = ~ lon, lat = ~lat,
                              label = ~ name_exhplace,
                              #popup = ~ paste0("<b>", name_exhplace,"</b>","<br/>", type_exhplace),
                              group = "Collector") %>%
    
    leaflet::addLayersControl(
      # Use addLayersControl to allow users to toggle between basemaps
      baseGroups = c("OSM", "Carto", "Esri"),
      # toggle between types of exhibition places
      overlayGroups = c("Artist", "Museum", "Gallery", "Non-Profit", "Collector"),
      position = "bottomleft"
    ) %>%
    
    leaflet.extras::addResetMapButton() %>%
    leaflet.extras::addSearchOSM() %>%
    
    # # Make each type of venues searchable
    # leaflet.extras::addSearchFeatures(
    # targetGroups = c("Artist"),#"Museum", "Gallery", "Non-Profit", "Collector")
    # #Set the search zoom level to 18
    # options = leaflet.extras::searchFeaturesOptions(zoom = 18)
    # ) %>%
    
    leaflet::addLegend(title = "Percentile", pal = pal, values = c(0:100),
                       position = "bottomright", opacity = 0.7) %>%
    
    leaflet::setView(lat = 18, lng = 8, zoom = 1.55)
  
  return(m)
}