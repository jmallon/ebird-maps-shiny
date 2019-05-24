
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggvis)
library(rgdal)
library(rgeos)
library(magrittr)
library(dplyr)
library(RColorBrewer)
library(data.table)
library(maptools)

shinyServer(function(input, output, session) {

# Show progress bar while loading everything ------------------------------

  progress <- shiny::Progress$new()
  progress$set(message="Loading maps/data", value=0)

# USA plot --------------------------------------------------------------

  us <- readOGR("data/us.geojson", "OGRGeoJSON")
  us <- us[!us$STATEFP %in% c("02", "15", "72"),]

  us_aea <- spTransform(us, CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))

  map <- ggplot2::fortify(us_aea, region="GEOID")

  droughts <- read.csv("data/dm_export_county_20141223.csv")
  droughts$id <- sprintf("%05d", as.numeric(as.character(droughts$FIPS)))
  droughts$total <- with(droughts, (D0+D1+D2+D3+D4)/5)

  map_d <- merge(map, droughts, all.x=TRUE)

  ramp <- colorRampPalette(c("white", brewer.pal(n=9, name="YlOrRd")), space="Lab")

  map_d$fill_col <- as.character(cut(map_d$total, seq(0,100,10), include.lowest=TRUE, labels=ramp(10)))
  map_d$fill_col <- ifelse(is.na(map_d$fill_col), "#FFFFFF", map_d$fill_col)

  drought_values <- function(x) {
    if(is.null(x) | !(x$id %in% droughts$id)) return(NULL)
    y <- droughts %>% filter(id==x$id) %>% select(1,3,4,6:10)
    sprintf("<table width='100%%'>%s</table>",
            paste0("<tr><td style='text-align:left'>", names(y),
                   ":</td><td style='text-align:right'>", format(y), collapse="</td></tr>"))
  }

  map_d %>%
    group_by(group, id) %>%
    ggvis(~long, ~lat) %>%
    layer_paths(fill:=~fill_col, strokeOpacity := 0.5, strokeWidth := 0.25) %>%
    add_tooltip(drought_values, "hover") %>%
    hide_legend("fill") %>%
    hide_axis("x") %>% hide_axis("y") %>%
    set_options(width=900, height=600, keep_aspect=TRUE) %>%
    bind_shiny("drought")

# World plot --------------------------------------------------------------

  world <- readOGR("data/ne_50m_admin_0_countries.geojson", layer="OGRGeoJSON")
  world <- world[!world$iso_a3 %in% c("ATA"),]
  world <- spTransform(world, CRS("+proj=wintri"))

  map_w <- ggplot2::fortify(world, region="iso_a3")

  launch_sites <- rbindlist(lapply(ogrListLayers("data/launch-sites.kml")[c(1:2,4:9)], function(layer) {
    tmp <- readOGR("data/launch-sites.kml", layer)
    places <- data.table(coordinates(tmp)[,1:2], as.character(tmp$Name))
  }))

  setnames(launch_sites, colnames(launch_sites), c("lon", "lat", "name"))
  coordinates(launch_sites) <-  ~lon+lat
  launch_sites <- as.data.frame(SpatialPointsDataFrame(spTransform(
    SpatialPoints(launch_sites, CRS("+proj=longlat")), CRS("+proj=wintri")),
    launch_sites@data))

  map_w %>%
    group_by(group, id) %>%
    ggvis(~long, ~lat) %>%
    layer_paths(fill:="#252525", stroke:="white", strokeOpacity:=0.5, strokeWidth:=0.25) %>%
    layer_points(data=launch_sites, stroke:="white", x=~lon, y=~lat, fill:="#cb181d", size:=25, fillOpacity:=0.5, strokeWidth:=0.25) %>%
    hide_legend("fill") %>%
    hide_axis("x") %>% hide_axis("y") %>%
    set_options(width=900, height=500, keep_aspect=TRUE) %>%
    bind_shiny("launch")

# Turn off progress bar ---------------------------------------------------

  progress$close()

})
