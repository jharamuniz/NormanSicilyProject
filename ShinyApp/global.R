library(leaflet)
library(RColorBrewer)
library(scales)
library(plyr)
library(lattice)
library(dplyr)
library(shiny)
library(maps)
library(maptools)
library(rgeos)
library(rgdal)
library(sp)

# Define UI for application that draws a histogram
#setwd("~/Documents/Montclair State University/NormanSicily")

#setwd("~/Google Drive File Stream/My Drive/MSU/Norman Sicily Project/NSP/ShinyApps/InteractiveMap")
norman<-read.csv("Places(Edit).csv", sep=",", header=T)


completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

#Remove rows with missing coordinate location
norman <- completeFun(norman, "Longitude")
#row.names(norman) <- norman$Name
cleantable <- norman %>%
  select(
    Latitude = Latitude,
    Longitude = Longitude,
    Monastery = Name,
    Seismic = Seismic,
    Comune = Comune,
    Province = Province,
    Order = Order,
    Gender = Gender,
    Rank = Rank,
    Dedications = Dedication
  )

shape2 <- readOGR(dsn="ITA_adm", layer="ITA_adm2")
shape2 <-subset(shape2, shape2$NAME_1==c("Sicily"))

shape2@data$NAME_2<-droplevels(shape2@data$NAME_2)
shape2$NAME_2<-revalue(shape2$NAME_2, c("Syracuse"="Siracusa"))

shape2@data<-dplyr::rename(shape2@data, region = NAME_2)

quobyprov <- norman %>% group_by(region=Province) %>% dplyr::summarize(Count=n())
quobyprov$region<-as.character(quobyprov$region)

joindata<-inner_join(shape2@data, quobyprov, by="region")
shape2 <- spCbind(shape2, joindata$Count)

shape <- readOGR(dsn="ITA_adm", layer="ITA_adm3")
shape<-subset(shape, shape$NAME_1==c("Sicily"))

classif<-read.csv("SeismicClass2015.csv", sep=",", header=T)

classif$NAME_3<-droplevels(classif$NAME_3)
shape@data$NAME_3<-droplevels(shape@data$NAME_3)

test<-inner_join(shape@data, classif, by="NAME_3")

shape <- spCbind(shape, test$seismic)