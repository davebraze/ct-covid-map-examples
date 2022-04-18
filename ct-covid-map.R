library(httr)
library(XML)
library(here)
library(fs)
library(purrr)
library(sf)
library(RSocrata)

library(stringr)
library(lubridate)
library(dplyr)

library(plotly)
library(leaflet)

source(here("locals.R"))

##################################################
## Intro to GIS and Spatial Analysis            ##
## https://mgimond.github.io/Spatial/index.html ##
##################################################

#######################
## load up map files ##
#######################

##### GIS data for CT is available here:
## http://magic.lib.uconn.edu/connecticut_data.html
##
## URL for town shape files is here: http://magic.lib.uconn.edu/magic_2/vector/37800/townct_37800_0000_2010_s100_census_1_shp.zip
##
## These shapefiles include outlines for CT towns in the form of multipolygons. It turns out that these won't work with leaflet. So, I've used the geojson files below for leaflet maps.
D.shape <-
    sf::st_read(here("02-shapefiles/CT/townct_37800_0000_2010_s100_census_1_shp/townct_37800_0000_2010_s100_census_1_shp/nad83",
                           "townct_37800_0000_2010_s100_census_1_shp_nad83_feet.shp")) %>%
    filter(NAME10 != "County subdivisions not defined") %>%
    mutate(LAT = as.numeric(INTPTLAT10),
           LON = as.numeric(INTPTLON10))

##### Read geojson file including outlines of connecticut towns.
## File downloaded from here: https://github.com/HandsOnDataViz/geodata-hartford-ct
## Also see readme.txt in 01-geojson/.
D.geojson <- st_read(here("01-geojson", "ct-towns.geojson"))

##### read data previously extracted from CTDPH pdf files
covid.pdf <- readRDS(file=here("03-other-source-data", "pdf-reports.rds"))

##### scrape town/county data from wikipedia

########### FIXME: STASH THIS TABLE LOCALLY AND USE THAT UNLESS WP PAGE IS UPDATED #########

if (FALSE) {
    url <- "https://en.wikipedia.org/wiki/List_of_towns_in_Connecticut"
    town.info <- GET(url) %>%
        htmlParse() %>%
        readHTMLTable(header=TRUE, which=2, skip=170) %>%
        janitor::clean_names() %>%
        select(-c(form_ofgovernment, native_americanname)) %>%
        rename(year.est = dateestablished,
               land.area.sq.miles = land_area_square_miles,
               pop.2010 = population_in_2010_1,
               pop.2020 = population_in_2020_1,
               council.of.governments = council_of_governments) %>%
        mutate(pop.2010 = as.integer(str_remove(pop.2010, ",")),
               land.area.sq.miles = as.numeric(land.area.sq.miles),
               county = str_replace(county, "County", "Co."),
               pop.2010.bin = cut(pop.2010,
                                  breaks=c(0, 5000, 15000, 35000, 75000, Inf),
                                  labels=c("less than 5,000", "5k, <15k", "15k, <35k",
                                           "35k, <75k", "75,000 or more"),
                                  ordered_result=TRUE))

    saveRDS(town.info, file = here("03-other-source-data", "town-info.rds"))

} else {

    town.info <- readRDS(file = here("03-other-source-data", "town-info.rds"))

}

##################################################
## Use the Socrata API to access state DPH data ##
##################################################

socrata.app.token <- Sys.getenv("SOCRATA_APP_TOKEN_CTCOVID19")

##### cases and deaths by town

## Upon reviewing the data provided by CT on Nov. 1 2020, there
## have been some changes to the variables in the Town data file. Need to sort that out.
## Metadata for the Town dataset can be found here
## https://data.ct.gov/Health-and-Human-Services/COVID-19-Tests-Cases-and-Deaths-By-Town-/28fr-iqnx
## OR https://data.ct.gov/resource/28fr-iqnx

ct.covid <- read.socrata("https://data.ct.gov/resource/28fr-iqnx.json",
                         app_token=socrata.app.token) %>%
    rename(Town = town,
           town.cases = towntotalcases) %>%
    mutate(across(starts_with("town", ignore.case=FALSE), as.integer),
           across(starts_with("people", ignore.case=FALSE), as.integer),
           across(starts_with("number", ignore.case=FALSE), as.integer),
           Date = as.Date(lastupdatedate)) %>%
    left_join(town.info, by=c("Town" = "name")) %>%
    mutate(town.cases.10k = (10000/pop.2010)*town.cases,
           town.deaths.10k = (10000/pop.2010)*towntotaldeaths) %>%
    ## left_join(D.shape, by=c("Town" = "NAME10")) %>%
    group_by(Town) %>%
    slice_max(Date, n=11) %>%           # Use 11 day window bcz need test count on 10 days.
    mutate(ending.date = max(Date),
           positive.sum = diff(range(numberofpositives)),
           tests.sum = diff(range(numberoftests)),
           tests.10k = tests.sum*(10000/pop.2010),
           town.positivity = positive.sum/tests.sum*100) %>%
    filter(Date == ending.date)

######## Merge covid data with alternative sets of town geometries.
######## In addition to the multi/polygon difference, there
######## also seems to be a diff in the CRS defaults encoded
######## in the two sets of geometries.

##### Basic data with multipolygons for town boundaries.
D.shp <-
    D.shape %>%
    left_join(ct.covid, by=c("NAME10" = "Town")) %>%
    select("Town" = "NAME10", town.positivity, pop.2010, tests.10k, geometry)

##### Basic data with simple polygons for town boundaries.
D.gj <-
    D.geojson %>%
    left_join(ct.covid, by=c("name" = "Town")) %>%
    select("Town" = "name", town.positivity, pop.2010, tests.10k, geometry)

##### constants for ggplot

## file names/types
today <- strftime(today(), "%Y%m%d-")
fig.path <- here("figures")
ftype  <- "png"
## layout
width <- 7
height <- 7
units <- "in"
dpi <- 300

###########################
## choropleth via ggplot ##
###########################

## ggplot::geom_sf
breaks.0 <- c(0,2,4,6,8,10,12,14,16,18,20)
shade.0 <- max(D.shp$town.positivity)*.5

map.positivity.cap <- paste("Ten Day Average Covid-19 Test Positivity for each Connecticut Town.",
                            "Test Positivity is the percentage of tests administered in a town",
                            "that had a positive result.")

map.positivity <-
    D.shp %>%
    ## select(town.positivity, geometry, Town, pop.2010, tests.10k) %>%
    ggplot() +
    geom_sf(aes(fill=town.positivity,
                geometry=geometry),
            color="white", size=.33) +
    scale_color_manual(values=c("black", "white")) +
    viridis::scale_fill_viridis(option="magma",
                                breaks=breaks.0,
                                labels=breaks.0) +
    guides(fill=guide_colorbar(title="Test Positivity (%)",
                               title.vjust=1)) +
    labs(title=paste("10 Day Average Covid-19 Test Positivity\nin Connecticut Towns\nfor period ending",
                     format(max(ct.covid$Date), "%B %d, %Y"))) +
    xlab(NULL) + ylab(NULL)

ggsave(filename=fs::path_ext_set(paste0(today, "map-positivity"), ftype),
       plot=map.positivity,
       path=fig.path,
       device=ftype,
       width=width, height=height,
       units=units,
       dpi=dpi)

#########################################
## interactive choropleth with plot_ly ##
#########################################

## Here are a couple of tutorials that may
## help make better maps, although they're a bit dated:
## • https://blog.cpsievert.me/2018/03/30/visualizing-geo-spatial-data-with-sf-and-plotly/
## • https://plotly-r.com/maps.html

## use geojson instead of shapefile

map.positivity.plotly  <-
    D.gj %>%
    mutate(text=paste0("<b>", Town, "</b>",
                       "\nTest Pos: ", formatC(town.positivity, format="f", digits=2), "%",
                       "\nPopulation: ", formatC(pop.2010, format="d"),
                       "\nTests/10k/day: ", formatC(tests.10k/10, format="f", digits=2))) %>%
    plot_ly(hoverinfo='text',
            hoveron='fills',
            text=~text) %>%
    add_sf(
        split=~Town,                  # hover text seems determined by split variable
        color=~town.positivity,
        colors="magma",                 # set colorscale
        alpha=1,
        stroke=I("grey90")
    ) %>%
    config(displayModeBar = FALSE) %>%
    layout(hovermode = "closest",
           hoverlabel = list(bgcolor="darkgreen",
                             bordercolor="black",
                             font=list(color="white", family = "sans-serif", size=15)
                             ),
           title = list(text = paste("<b>10 Day Average Covid-19 Test Positivity\n",
                                     "in Connecticut Towns\nfor period ending",
                                     format(max(ct.covid$Date), "%B %d, %Y"), "</b>"),
                        x = 0,
                        xanchor = "left",
                        yanchor = "top",
                        margin = list(t=100),
                        pad = list(b=10, l=10, r=10, t=100))) %>%
    colorbar(title = "<b>Test Positivity (%)</b>") %>%
    hide_legend()                       # plot doesn't show w/o this line

#####################################################################################
## Color scales built in to plotly are included in the RColorBrewer package.       ##
## Any of the strings in brewer.pal.info can be used to set a plotly color scale.  ##
## library("RColorBrewer")                                                         ##
## brewer.pal.info                                                                 ##
#####################################################################################



#########################################
## interactive choropleth with leaflet ##
#########################################

## Use geojson with leaflet, not shapefiles. Shapefiles don't work with leaflet.

pal <- colorNumeric("magma", NULL)

map.positivity.leaflet <-
    D.gj %>%
    mutate(text=map(paste(
                     paste0("<b>", Town, "</b>"),
                     paste0("Test Pos: ", formatC(town.positivity, format="f", digits=2), "%"),
                     paste0("Population: ", formatC(pop.2010, format="d")),
                     paste0("Tests/10k/day: ", formatC(tests.10k/10, format="f", digits=2)),
                     sep="<br>"),
                     htmltools::HTML)) %>%
    leaflet() %>%
    setView(lng=-72.8, lat=41.5, zoom=9) %>%
    addPolygons(color="lightgrey",      # stroke color
                stroke = TRUE,
                weight = 1.5,           # stroke
                fillColor = ~pal(town.positivity),
                fillOpacity = 1,
                label = ~text,
                labelOptions = labelOptions(textsize = "15px", ## could also set "font-size" in 'style' arg
                                            style = list( # add custom CSS
                                                "background-color" = "darkgreen",
                                                "color" = "white" # font color
                                            ))) %>%
    addLegend(
        "bottomright",
        pal = pal,
        values = ~town.positivity,
        title = "<b>Test Positivity (%)</b>",
        opacity = 1,
    )

## leafletOptions()
## tileOptions()
## previewColors(colorNumeric("magma", 1:20), 1:20)


if(FALSE) {
    ## check for unused packages
    p <- tibble(pkgs=funspotr::spot_pkgs(here("ct-covid-map.R")))
    f <- funspotr::spot_funs(here("ct-covid-map.R"))
    dplyr::anti_join(p,f, by="pkgs")
}
