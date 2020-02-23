library(leaflet)
library(rgdal)
library(HK80)
library(jsonlite)
library(htmltools)
library(stringr)
library(colorRamps)
library(magrittr)
library(htmlwidgets)

setwd("./dce_2019")

##----- Shapefile-----
dcca2019 <- readOGR("./DCCA2019_Shapefile/")

# convert coordinate system into WGS84 format
dcca_poly <-  list()
for (i in 1:length(dcca2019)) {
  dcca_poly[[i]] <-  data.frame(t(apply(dcca2019[i, ]@polygons[[1]]@Polygons[[1]]@coords, 1,
                                        function(x) rev(unlist(HK1980GRID_TO_WGS84GEO(x[2], x[1]))))))
}
dcca2019 <- SpatialPolygons(sapply(1:length(dcca2019$CACODE),
                                   function(x) Polygons(list(Polygon(dcca_poly[[x]])), dcca2019$CACODE[x])))

# fix overlapping issue of T02, T03 & G25
for (i in 1:length(dcca2019)) { 
  if (dcca2019@polygons[[i]]@ID == "T01") t01 <- i
  if (dcca2019@polygons[[i]]@ID == "T02") t02 <- i
  if (dcca2019@polygons[[i]]@ID == "T03") t03 <- i
  if (dcca2019@polygons[[i]]@ID == "G24") g24 <- i
  if (dcca2019@polygons[[i]]@ID == "G25") g25 <- i
}

dcca2019@polygons[[t01]] <- Polygons(list(Polygon(cbind(dcca2019@polygons[[t02]]@Polygons[[1]]@coords[, 1], 
                                                        dcca2019@polygons[[t02]]@Polygons[[1]]@coords[, 2]),
                                                  hole = TRUE),
                                          Polygon(cbind(dcca2019@polygons[[t03]]@Polygons[[1]]@coords[, 1], 
                                                        dcca2019@polygons[[t03]]@Polygons[[1]]@coords[, 2]),
                                                  hole = TRUE),
                                          Polygon(cbind(dcca2019@polygons[[t01]]@Polygons[[1]]@coords[, 1],
                                                        dcca2019@polygons[[t01]]@Polygons[[1]]@coords[, 2]))), "T01")

dcca2019@polygons[[g24]] <- Polygons(list(Polygon(cbind(dcca2019@polygons[[g25]]@Polygons[[1]]@coords[, 1], 
                                                        dcca2019@polygons[[g25]]@Polygons[[1]]@coords[, 2]),
                                                  hole = TRUE),
                                          Polygon(cbind(dcca2019@polygons[[g24]]@Polygons[[1]]@coords[, 1],
                                                        dcca2019@polygons[[g24]]@Polygons[[1]]@coords[, 2]))), "G24")

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

# calculate pro-dem vote ratio of each constituency
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
# create polygon layers by district
layered_dcca <- lapply(unique(constituencies$district_code), function(x) {
  by_district <- constituencies %$% constituency_code[district_code == x]
  dcca_poly <- list()
  n <- 1
  for(i in 1:length(dcca2019)) {
    if (dcca2019@polygons[[i]]@ID %in% by_district) {
      dcca_poly[[n]] <- dcca2019@polygons[[i]]
      n <- n + 1
    }
  }
  SpatialPolygons(dcca_poly)
})

m <- leaflet() %>% addTiles() %>% addProviderTiles(providers$CartoDB.Positron)

pal <- colorBin(blue2yellow(10), by_constituency$ratio * 100)

for (x in layered_dcca) {
  district_results <- by_constituency[by_constituency$code %in% names(x), ]
  m %<>% addPolygons(group = constituencies$district[constituencies$constituency_code 
                                                     %in% x@polygons[[1]]@ID],
                     data = x, fillColor = pal(district_results$ratio * 100),
                     label = lapply(district_results$popup, HTML),
                     labelOptions = labelOptions(style = list("font-weight" = "normal"),
                                                 textsize = "14px"),
                     fillOpacity = .6, weight = 3, stroke = TRUE, color = "grey", dashArray = 3,
                     highlightOptions = highlightOptions(color = "white",
                                                         weight = 2,
                                                         fillOpacity = 0.8,
                                                         bringToFront = TRUE))
}

m %>% addLegend("bottomright", pal = pal, values = by_constituency$ratio, title = "民主派支持者比例",
                labFormat = labelFormat(suffix = '%'), opacity = 0.7) %>%
  addLayersControl(overlayGroups = unique(constituencies$district), 
                   options = layersControlOptions(collapsed = FALSE), position = "bottomleft") %>%
  saveWidget(file="dce_2019.html")
