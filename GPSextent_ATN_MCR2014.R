library(data.table) #fread
library(dplyr)
library(ggplot2)


if(Sys.info()[7]=="rachaelorben") {
  datadir<-'/Users/rachaelorben/Box/DASHCAMS/data/'
}

Files<-list.files(paste0(datadir,"/2014_MCR/2014/GPS_TDR_DATA_SYNC/"),
                  pattern = ".csv",full.names = TRUE)

DAT<-NULL
for (j in 1:length(Files)){
  dat<-fread(file=Files[j],stringsAsFactors=FALSE,sep = ",",fill=TRUE) 
  dat<-dat%>%filter(is.na(LatDegree)==FALSE)
  dat<-dat%>%select(BirdID,LatDegree,LongDegree,ObsDepth,CalDepth)
  DAT<-rbind(DAT,dat)
}
DAT$species<-sapply(strsplit(DAT$BirdID, split='_', fixed=TRUE), function(x) (x[1]))

min(DAT$LatDegree)
max(DAT$LatDegree)

min(DAT$LongDegree)
max(DAT$LongDegree)

DAT.d<-DAT%>%filter(is.na(ObsDepth)==FALSE)
max(DAT.d$CalDepth)

ggplot()+
  geom_path(data=DAT, aes(x=LongDegree,y=LatDegree, group=BirdID, color=species))+
  facet_wrap(~species)


