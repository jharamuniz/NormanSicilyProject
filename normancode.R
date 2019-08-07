#Change to the directory you will be using (where the file is saved)
setwd("~/Documents/Montclair State University/NormanSicily")

#Loads the CSV file into the workspace
norman<-read.csv("Places(Edit).csv", sep=",", header=T)

#Number of NA values under Longitude (matched with Latitude)
sum(is.na(norman$Longitude))

#Function to remove rows with missing numerical data from desired columns
completeFun <- function(data, desiredCols) 
{
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

#Remove missing values from Longitude
norman<-completeFun(norman, "Longitude")

#Reset the index numbers
rownames(norman) <- seq(length=nrow(norman))

###### MAP BUILDING ##########
if (!require("ggmap")) install.packages("ggmap")
#Dataframe of Italy
italy <- map_data("italy")

if (!require("dplyr")) install.packages("dplyr")
#Filters through the italy data frame to select the provinces of Sicily
sicily<-filter(italy, region %in% c("Agrigento", "Catania", "Palermo",
                                    "Caltanissetta", "Siracusa", "Messina", "Enna",
                                    "Adrano", "Trapani", "Ragusa"))


if (!require("ggplot2")) install.packages("ggplot2")
if (!require("RColorBrewer")) install.packages("RColorBrewer")
#Creates basemap of Sicily (greyscale)
mapgrey <- ggplot(data = sicily, mapping = aes(x = long, y = lat, group = group)) +
  geom_polygon(color = "black", fill = "gray33") +
  coord_fixed(1.3) + 
  xlim(12,15.8) + 
  ylim(36.5,38.9) +
  theme_bw()
mapgrey

if (!require("plyr")) install.packages("plyr")
#Data used for adding the province names on the map
provLabels <- data.frame(long = c(13.43908, 14.15968, 14.88015, 14.45513, 
                                  14.89305, 13.51284, 14.62863, 15.06266, 12.71009),
                         lat = c(37.46851, 37.24038, 37.44926, 37.64421,
                                 38.02137, 37.90601, 36.89423, 37.04551, 37.87673),
                         province = c("Agrigento","Caltanissetta","Catania","Enna","Messina",
                                      "Palermo","Ragusa","Siracusa","Trapani"))

#Code to add the labels to map
addnames <- geom_text(data=provLabels, aes(x=long, y=lat, label=province), 
                      size=1.7, inherit.aes = FALSE)

#Color coded version of the map of sicily (with labels)
mapcol <- ggplot() + geom_polygon(data = sicily, aes(x=long, y = lat,
                                                     fill = region, group = group), 
                                  color="grey", size=0.3) +
  scale_fill_brewer(palette="Pastel1") +
  coord_fixed(1.3) +
  xlim(12,15.8) + 
  ylim(36.5,38.9) +
  guides(fill=FALSE) +
  theme_bw() + addnames
mapcol 

#Monastery location overlayed on map
monmap <- mapcol + 
  geom_point(norman, mapping = aes(x = Longitude, y = Latitude, text=Name)) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Monasteries of Sicily") +
  theme(plot.title = element_text(hjust = 0.5))
monmap

###### QUESTIONABLE DATA ##########

#Map of Monasteries by Province
mapcol + geom_point(norman, mapping = aes(x = Longitude, y = Latitude, 
                                          color=Province), size=1.7) +
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("Monasteries by Province")

#Gives indeces of observations with questions marks under variable "Province"
if (!require("base")) install.packages("base")
grep("\\?", norman$Province)
norman[grep("\\?", norman$Province),c("Name", "Province")]

#Map of monasteries with unknown provinces (some overlap)
mapcol + 
  geom_point(norman[grep("\\?", norman$Province),], 
             mapping = aes(x = Longitude, y = Latitude, color=Province), size=1.7) +
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("Unknown Province Locations")

#Monasteries by Order
mapcol + 
  geom_point(norman, mapping = aes(x = Longitude, y = Latitude, colour = Order)) +
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("Monasteries by Order")

#Gives indexes of observations with questions marks under variable "Order"
grep("\\?", norman$Order)
norman[grep("\\?", norman$Order),c("Name", "Order")]
#Two with unknown order (Blanks)
subset(norman, Order=="")

#Map of Monasteries by Order (Facet-Wrapped)
mapgrey + geom_point(norman, mapping = aes(x = Longitude, y = Latitude, 
                                           colour = Order), inherit.aes = FALSE) +
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("Monasteries by Order") +
  facet_wrap(~Order) +
  guides(colour=FALSE) 


###### CHOROPLETH MAPS #######
#Bar Graph of Monastery Frequency by Province
ggplot(data = norman) + geom_bar(aes(x=Province)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("Monastery Frequency by Province")

#Reorganize the data to obtain province counts
quobyprov <- norman %>% 
  dplyr::group_by(region=Province) %>% 
  dplyr::summarize(Count=n())
quobyprov
#Change region type to type character
quobyprov$region<-as.character(quobyprov$region)

#Join the data together by "region"
joindata <- dplyr::inner_join(sicily, quobyprov, by = "region")

#Choropleth map of monasteries by province
mapgrey + geom_polygon(data = joindata, aes(fill = cut_number(joindata$Count,5)), color = "white") +
  labs("Count", fill="Density") +
  scale_fill_brewer(palette="PuBu")+
  geom_polygon(color = "black", fill = NA) +
  ggtitle("Monastery Density by Provinces") + addnames

#Bar Graph of Monastery frequency by Order
ggplot(data = norman) + geom_bar(aes(x=Order)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("Monastery Count by Order")

#Reorganize the data to obtain province & order counts
quobyorder <- norman %>% 
  dplyr::group_by(region=Province, Order=Order) %>% 
  dplyr::summarize(Count=n())
#Change order type to type character
quobyorder$Order <- as.character(quobyorder$Order)

#Join the data together by "region"
joindata2 <- dplyr::inner_join(sicily, quobyorder, by = "region") 

#Choropleth map of monasteries by province (facet-wrapped by Order)
mapgrey + geom_polygon(data = joindata2, 
                       aes(fill = cut_width(joindata2$Count,2, boundary = 1)), 
                       color = "white") +
  labs(fill="Density")+
  scale_fill_brewer(palette="Blues")+
  geom_polygon(color = "black", fill = NA) +
  ggtitle("Monastery Density by Order") +
  facet_wrap(~Order)


#Closer look at the Basilian order (with points)
mapgrey + geom_polygon(data = joindata2[joindata2$Order == "Basilian",], 
                       aes(fill = cut_width(joindata2[joindata2$Order == "Basilian",]$Count,2, boundary = 1)), 
                       color = "white") +
  labs(fill="Density")+
  scale_fill_brewer(palette="Blues")+
  geom_polygon(color = "black", fill = NA) +
  geom_point(norman[norman$Order=="Basilian",], mapping=aes(x=Longitude, y=Latitude), 
             color = "red", inherit.aes = FALSE) +
  ggtitle("Monastery Density of Basilian") 

#Closer look at the Benedictine order (with points)
mapgrey + geom_polygon(data = joindata2[joindata2$Order == "Benedictine",], 
                       aes(fill = cut_width(joindata2[joindata2$Order == "Benedictine",]$Count,2, boundary = 1)), 
                       color = "white") +
  labs(fill="Density")+
  scale_fill_brewer(palette="Blues")+
  geom_polygon(color = "black", fill = NA) +
  geom_point(norman[norman$Order=="Benedictine",], mapping=aes(x=Longitude, y=Latitude), 
             color="red", inherit.aes = FALSE) +
  ggtitle("Monastery Density of Benedictine") 


##### GROUPING DEDICATIONS ######
#Bar graph of dedications
ggplot(data = norman) + 
  geom_bar(mapping = aes(x = Dedication))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Reorganize the data to obtain dedication counts
dedcount <- norman %>% 
  dplyr::group_by(Dedication) %>% 
  dplyr::summarize(Count = n())
dedcount

#Indices with multiple dedications
grep("&", norman$Dedication) #indices 63, 64, 76, 113
norman[grep("&", norman$Dedication), c("Name", "Dedication"),]
dim(norman) #138 x 17

#Separates the monasteries with multiple counts into individual observations
if (!require("tidyr")) install.packages("tidyr")
normanEdit<-tidyr::separate_rows(norman, Dedication, sep="&\\s+")
dim(normanEdit) #144 x 17   

#Grouping the Dedications
DedGroup<-matrix(nrow=nrow(normanEdit), ncol=1)
num<-nrow(normanEdit)

for(i in 1:num){
  if (grepl("the Virgin Mary", normanEdit$Dedication[i])) {
    DedGroup[i] <- "the Virgin Mary"
  }
  else if (grepl("Christ", normanEdit$Dedication[i]) & 
           normanEdit$Dedication[i] != "Saint Christopher") {
    DedGroup[i] <- "Christ"
  }
  else if (grepl("the Holy", normanEdit$Dedication[i])) {
    DedGroup[i] <- "Holy"
  }
  else if (normanEdit$Dedication[i]=="") {
    DedGroup[i] <- "Unknown"
  }
  else {
    DedGroup[i] <- "Saint"}
}

#Change DedGroup to type factor
normanEdit$DedGroup <- as.factor(DedGroup)
table(DedGroup)

#Add the DedGroup column to the orginal dataset (removing the extra indeces)
norman$DedGroup <- as.factor(DedGroup[-c(64,66,79,80,118,119)])

dim(norman)
dim(normanEdit)

#Map of the monastery dedication 
mapcol + 
  geom_point(normanEdit, mapping = aes(x = Longitude, y = Latitude, color = DedGroup)) +
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("Monasteries by Dedication")+
  facet_wrap(~DedGroup)