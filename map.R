library(leaflet)
library(rgdal)
library(HK80)
library(jsonlite)
library(htmltools)
library(stringr)
library(colorRamps)
library(magrittr)
library(htmlwidgets)

setwd("***INSERT PATH HERE***/dce_2019")

##----- Shapefile-----
dcca2019 <- readOGR("./DCCA2019_Shapefile/")

dcca_poly <-  list()
for (i in 1:length(dcca2019)) {
  dcca_poly[[i]] <-  data.frame(t(apply(dcca2019[i, ]@polygons[[1]]@Polygons[[1]]@coords, 1,
                                        function(x) rev(unlist(HK1980GRID_TO_WGS84GEO(x[2], x[1]))))))
}
dcca2019 <- SpatialPolygons(sapply(1:length(dcca2019$CACODE),
                                   function(x) Polygons(list(Polygon(dcca_poly[[x]])), dcca2019$CACODE[x])))

##----- Process data -----
raw <- fromJSON("https://dce2019.thestandnews.com/data/all.json", flatten = TRUE)

# extract and organise useful info from json
result <- raw$candidates %$% data.frame(code = str_extract(resources.slug, "[A-Z]{1}[0-9]{2}"),
                                        constituency = constituency.name,
                                        candidate = number,
                                        camp = ifelse(camp == "民主", "yellow", 
                                                      ifelse(camp == "建制", "blue", "undetermined")),
                                        votes = result2019.voteCount,
                                        won = result2019.isWon, stringsAsFactors = FALSE)

# create constituency/district code reference list
constituencies <- raw$constituencies %$% data.frame(district_code = district.officialId,
                                                    district = district.name,
                                                    constituency_code = officialId,
                                                    constiuency = name)

# calculate ratio of each constituency
by_constituency <- sapply(names(dcca2019), function(x) 
  sum(result %$% votes[code == x & camp == "yellow"]) /
    sum(result %$% votes[code == x])) %>%
  data.frame(code = names(.), 
             constituency = sapply(names(.), function(x) unique(result %$% constituency[code == x])),
             ratio = ., stringsAsFactors = FALSE)

# create map popup message
by_constituency$popup <- sapply(by_constituency$code, function(x)
  paste0("<strong>", constituencies$district[constituencies$constituency_code == x], " - ",
         by_constituency$constituency[by_constituency$code == x], "（", x, "）</strong>",
         "<br/>民主派支持者比例：", round(by_constituency$ratio[by_constituency$code == x] * 100, 2), "%",
         "<br/>當選議員：", raw$constituencies$newCouncillor.name[raw$constituencies$officialId == x],
         "（", raw$constituencies$newCouncillor.camp[raw$constituencies$officialId == x], "）"))

##----- Draw map -----
# create polygon files of each layer
layered_dcca <- lapply(unique(constituencies$district_code), function(x) {
  by_district <- constituencies %$% constituency_code[district_code == x]
  dcca_poly <- list()
  n <- 1
  for(i in 1:length(dcca2019)) {
    if(dcca2019@polygons[[i]]@ID %in% by_district) {
      dcca_poly[[n]] <- data.frame(longitude = dcca2019@polygons[[i]]@Polygons[[1]]@coords[, 1], 
                                   latitude = dcca2019@polygons[[i]]@Polygons[[1]]@coords[, 2])
      n <- n + 1
    }
  }
  SpatialPolygons(sapply(1:length(by_district), function(y) Polygons(list(Polygon(dcca_poly[[y]])), by_district[y])))
})

m <- leaflet() %>% addTiles() %>% addProviderTiles(providers$CartoDB.Positron)

pal <- colorBin(colorRamps::blue2yellow(10), by_constituency$ratio * 100)

for (x in layered_dcca) {
  district_results <- by_constituency[by_constituency$code %in% names(x),]
  m %<>% addPolygons(group = constituencies$district[constituencies$constituency_code 
                                                     %in% x@polygons[[1]]@ID],
                     data = x, fillColor = pal(district_results$ratio * 100),
                     label = lapply(district_results$popup, HTML),
                     labelOptions = labelOptions(
                       style = list("font-weight" = "normal"),
                       textsize = "14px"),
                    fillOpacity = .6,
                     weight = 3, stroke = TRUE, color = "grey", dashArray = 3,
                     highlightOptions = highlightOptions(color = "white",
                                                         weight = 2,
                                                         fillOpacity = 0.8,
                                                         bringToFront = TRUE))
}

m %>% addLegend("bottomright", pal = pal, values = by_constituency$ratio, title = "民主派支持者比例",
                labFormat = labelFormat(suffix = '%'), opacity = 0.7) %>%
  addLayersControl(overlayGroups = unique(constituencies$district), 
                   options = layersControlOptions(collapsed = FALSE), position = "bottomleft") %>%
  saveWidget(., file="dce_2019.html")
