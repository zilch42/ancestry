library(tidyverse)
library(readxl)
library(raster)
library(ggmap)
library(rgdal)
library(rgeos)
library(maptools)
library(ggrepel)



# read in ancestry data
census <- as.tibble(read.csv("./Census/2016 Census GCP State Suburbs for SA/2016Census_G08_SA_SSC.csv"))
# read in list of metro Adelaide suburbs (source: Wikipedia)
Subs <- as.tibble(read.csv("./Census/Metro Suburbs.csv",header=FALSE))
# read in ABS region definitions (SSC codes) and clean up
SSC <- as.tibble(read_excel("./Census/Metadata/2016Census_geog_desc_1st_and_2nd_release.xlsx", sheet = "2016_ASGS_Non-ABS_Structures"))
SSC <- SSC %>% filter(ASGS_Structure == 'SSC' & ASGS_Code_2016  > 40000 & ASGS_Code_2016 < 50000)
SSC$Census_Name_2016 <- gsub("\\s\\(SA\\)", "", SSC$Census_Name_2016)



# clean up  ancestry data
census <- census %>% dplyr::select(-ends_with("_OS"), -ends_with("_Aus"),-ends_with("_Aust"), -ends_with("_stated"), -ends_with("_NS")) %>%
  left_join(dplyr::select(SSC,"Census_Name_2016", "Census_Code_2016"),by=c("SSC_CODE_2016"="Census_Code_2016")) %>%
  rename(Suburb = Census_Name_2016)
names(census) <- gsub(x = names(census), pattern = "_Tot_Resp", replacement = "")
census <- census %>% mutate(Suburb=factor(Suburb))
# remove empty rows like Adelaide airport and tiny suburbs
census <- filter(census,Suburb %in% Subs$V1,Tot_P>200)

# calculate ancestry proportions
propD <- dplyr::select(census,3:31)/census$Tot_P
propD2 <- dplyr::select(census,2:33)/census$Tot_P
row.names(propD) <- census$Suburb
topHits <- as.tibble(cbind(Suburb = rownames(propD)[apply(propD,2,which.max)],Ancestory = colnames(propD),Proportion=apply(propD,2,max)))
topHits$Proportion <- as.numeric(topHits$Proportion)
topHits$Suburb <- toupper(topHits$Suburb)
top12 <- head(topHits[order(-topHits$Proportion),], n = 12)


# calculate suburb proportions
topSuburb <- colnames(propD)[max.col(propD,ties.method="first")]
propD <- propD %>% mutate(HighestProportion=do.call(pmax, (.)))
propD <- cbind(propD,topSuburb) 
propD[order(propD$HighestProportion),]


# function for converting to Title Case
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

# load shape file
# https://data.sa.gov.au/data/dataset/suburb-boundaries
adelaide <- shapefile("./Census/Suburbs_shp/Suburbs.shp")
metroAdel <- subset(adelaide,adelaide$SUBURB %in% toupper(Subs$V1))
metroAdel.df <- broom::tidy(metroAdel,region="SUBURB")
topAdel <- subset(adelaide,adelaide$SUBURB %in% top12$Suburb)
topAdel.df <- broom::tidy(topAdel,region="SUBURB")
topAdel.df$id <- sapply(tolower(topAdel.df$id),simpleCap)

# prep legend
top12$Suburb <- sapply(tolower(top12$Suburb),simpleCap)
top12 <- top12 %>% mutate(Concat = paste(top12$Suburb, top12$Ancestory ,sep=" - "))
topAdel.df <- left_join(topAdel.df,dplyr::select(top12,1,4), by=c("id"="Suburb"))

# get centroids of top suburbs
topCenters <- topAdel.df %>% group_by(Concat) %>% nest()

longMean <- function(data){
  mean(data$long)
}
latMean <- function(data){
  mean(data$lat)
}
topCenters <- topCenters %>% mutate(aveLong = map(data,longMean), aveLat = map(data,latMean))
topCenters <- dplyr::select(topCenters,-2)
topCenters$aveLong <- unlist(topCenters$aveLong)
topCenters$aveLat <- unlist(topCenters$aveLat)
topCenters <- left_join(topCenters,dplyr::select(top12,Concat,Suburb,Ancestory),by="Concat")

# plot map
theme <- theme(axis.title=element_blank())
map <- ggplot() +
  geom_polygon(data = metroAdel.df, aes(x = long, y = lat, group = group),color = 'gray', fill='white', size = .2)+
  geom_polygon(data = topAdel.df, aes(x = long, y = lat, group = group, fill=Concat),color = 'gray', size = .2)+
  coord_map(xlim=c(138.1,139), ylim = c(-35.5,-34.5)) + scale_fill_brewer(palette = "Paired") + theme +
  geom_text_repel(data = topCenters, aes(x = aveLong, y = aveLat, label = Ancestory), 
                  size = 3, box.padding = unit(2, 'lines'), force = 0.5, max.iter = 4000)
map$labels$fill <- "Suburbs"
print(map)

