library(ggplot2)
library(lubridate)
library(dplyr)
library(data.table) #rename
library(stringr)
#library(argosfilter)
library(R.utils)
library(tidyr)


# Link to local Box Sync folder ---- 
#To find user/computer specific username use: Sys.getenv("LOGNAME")
#change path to ornitela_ftp on box server

args = commandArgs(trailingOnly=TRUE)

datadir<-args[1] #/Users/rachaelorben/Box/DASHCAMS/data/ornitela_ftp/
deplydir<-args[2] #/Users/rachaelorben/Box/DASHCAMS/data/Field Data/
savedir<-args[3] #/Users/rachaelorben/Box/DASHCAMS/zTagStatus/

#if(Sys.info()[7]=="rachaelorben") {usrdir<-'/Users/rachaelorben/Box/DASHCAMS/';fold<-"data/ornitela_last24h/"}
#if(Sys.info()[7]=="tragabigzanda") {usrdir<-'/Users/tragabigzanda/Box Sync/DASHCAMS/';fold<-"data/ornitela_last24h/"}


#  Pulls in deployment matrix ---------------------------------------------
deploy_matrix<-read.csv(paste0(deplydir,"DASHCAMS_Deployment_Field_Data.csv"))
str(deploy_matrix)
deploy_matrix<-deploy_matrix%>%select(Bird_ID,TagSerialNumber,Project_ID,DeploymentStartDatetime)%>%
  filter(is.na(TagSerialNumber)==FALSE)
deploy_matrix$DeploymentStartDatetime<-mdy_hm(deploy_matrix$DeploymentStartDatetime)

# Find file names with data -------------------------------------------
my_files <- fileSnapshot(path=datadir)
Files<-rownames(my_files$info[1])[which(my_files$info[1] > 309)] #selects files with >309 bytes (1 header row)

# Cycles through data files to find tags active in last week------------
sel_files<-NULL
for (i in 1:length(Files)){
  nL <- countLines(paste0(datadir,Files[i]))
  df <- read.csv(paste0(datadir,Files[i]), header=FALSE, skip=nL-1)
  df$V2<-ymd_hms(df$V2)
  today<-Sys.time()
  if(df$V2[1]>today-604800){sel_files<-c(sel_files,Files[i])} #selects files with a last date within 7 days of today
}

# Cycles through selected data files ----------------------------------------------
Birds<-NULL 
for (i in 1:length(sel_files)){
  dat<-read.csv(file = paste0(datadir,sel_files[i]),sep = ",") #could switch to fread to be quicker...
  dat<-rename(dat,lat="Latitude")
  dat<-rename(dat,lon="Longitude")
  dat<-rename(dat,alt="MSL_altitude_m")
  dat[is.na(dat)==TRUE]<-NA
  
  fileN<-Files[i]
  A<-sapply(strsplit(Files[i], split='_', fixed=TRUE), function(x) (x[1]))
  dat$tagID<-sapply(strsplit(A, split='.', fixed=TRUE), function(x) (x[1]))
  
  deply_sel<-deploy_matrix[deploy_matrix$TagSerialNumber==dat$tagID[1],]
  n<-nrow(deply_sel)
  if(n==0) next #if the tag isn't in the deployment matrix is will be skipped - important for skipping testing tags etc. 
  deply_sel<-deply_sel[n,]
  dat$Project_ID<-deply_sel$Project_ID
  
  dat$datetime<-ymd_hms(dat$UTC_timestamp)
  dat$oid<-1:nrow(dat)
  
  today<-Sys.time()
  dat_sel<-dat[dat$datetime>(today-604800),]
  Birds<-rbind(Birds,dat_sel)
}

str(Birds)

#remove duplicate GPS points 
Birds<-Birds%>%group_by(device_id,datetime)%>%
  distinct(device_id,datetime, .keep_all = TRUE)%>%
  arrange(datetime)

# quick summary of the bird data
sumDat<-Birds%>%group_by(Project_ID,tagID)%>%
    summarise(minDt=min(datetime),
              maxDt=max(datetime),
              maxDepth=max(depth_m,na.rm=TRUE),
              uDepth=mean(depth_m,na.rm=TRUE),
              n_GPS=n_distinct(lat),
              uBat=mean(U_bat_mV,na.rm=TRUE),
              uTemp=mean(ext_temperature_C,na.rm=TRUE),
              uBond=mean(conductivity_mS.cm,na.rm=TRUE))%>%
    mutate(dur_hr=round(maxDt-minDt,2)) 

dat_info<-Birds%>%
  group_by(tagID,datatype) %>%
  summarise(no_rows = length(datatype))

wide_dat<-dat_info%>%pivot_wider(names_from = datatype, values_from = no_rows)
SUMDAT<-left_join(sumDat,wide_dat,by="tagID")

SUMDAT[SUMDAT==-Inf]<-NA
write.csv(SUMDAT,paste0(savedir,"/1WeekStats_",date(today),".csv"))

# error checking plots ----------------------------------------------------
dt<-Sys.time()

IDs<-unique(Birds$device_id)
for (i in 1:length(IDs)){
  birdy<-Birds[Birds$device_id==IDs[i],]
  Mdt<-min(birdy$datetime)
  
  labs <- data.frame(variable = c("a", "b","c","d","e"), 
                     title_wd = c("Depth", "Temp","Lat","Bat","Solar"), 
                     y = c(0,10,40,80,110),
                     dt=c(rep(Mdt-10000,5))) # vertical position for labels
  
temp_plot<-ggplot()+
  #depth=blue
  geom_point(data=birdy%>%filter(is.na(depth_m)==FALSE)%>%filter(depth_m!=0),
             aes(x=datetime,y=-depth_m),size=.01, color="blue")+
  #lat=black
  geom_point(data=birdy,aes(x=datetime,y=lat),size=.01, color="black")+
  #temperature=purple
  geom_point(data=birdy%>%filter(ext_temperature_C<100)%>%filter(ext_temperature_C>0),
             aes(x=datetime,y=ext_temperature_C),size=.01,color="purple")+
  #conductivity=orange
  #geom_point(data=birdy%>%filter(is.na(conductivity_mS.cm)==FALSE),
  #           aes(x=datetime,y=log(conductivity_mS.cm)),size=.01,color="orange")+
  #battery=red
  geom_path(data=birdy%>%filter(is.na(lat)==FALSE),aes(x=datetime,y=bat_soc_pct),color="grey")+
  geom_point(data=birdy%>%filter(is.na(lat)==FALSE),aes(x=datetime,y=bat_soc_pct),color="red",size=.01)+
  #solar=yellow
  geom_point(data=birdy%>%filter(is.na(lat)==FALSE),aes(x=datetime,y=solar_I_mA+101),color="goldenrod2",size=.01)+
  scale_x_datetime(date_labels = "%b %d") +
  geom_text(data = labs, angle = 90, size=2,# add rotated text near y-axis
            aes(x = dt, y = y, label = title_wd, color = title_wd)) +
  scale_color_manual(values=c("red","blue" ,"black","goldenrod2","purple")) +
  ylab("")+ # hide default y-axis label
  theme(legend.position = "none")+
  guides(color = guide_legend(override.aes = list(size = 5)))
ggsave(temp_plot,filename = paste0(savedir,birdy$Project_ID[1],"_",IDs[i],"_AllDataStreams.png"),
       height=4,width=8,device = "png")
}

#map location, color by date
w2hr<-map_data('world')

Birds_gps<-Birds%>%filter(lat!=0)%>%filter(is.na(lat)==FALSE)
Birds_gps$device_id<-as.factor(Birds_gps$device_id)

IDs<-unique(Birds_gps$Project_ID)
for (i in 1:length(IDs)){
  birdies<-Birds_gps[Birds_gps$Project_ID==IDs[i],]
  y_min<-min(birdies$lat)-.25
  y_max<-max(birdies$lat)+.25
  
  x_min<-min(birdies$lon)-.25
  x_max<-max(birdies$lon)+.25
  
temp_plot<-ggplot()+
  geom_polygon(data=w2hr,aes(long,lat,group=group),fill="grey70",color="grey60",size=0.1)+
  geom_path(data=birdies,aes(x=lon,y=lat, group=device_id))+
  geom_point(data=birdies,aes(x=lon,y=lat, color=device_id), size=.1)+
  xlab("Longitude")+
  ylab("Latitude")+
  coord_fixed(ratio=1.7,xlim = c(x_min,x_max),ylim=c(y_min,y_max))+
  theme_bw()+
  theme(legend.title = element_blank(),
        legend.text = element_text(size=5))+
  guides(color = guide_legend(override.aes = list(size = 2)))
ggsave(temp_plot,filename = paste0(savedir,"1wk_map_",IDs[i],".png"),height=4,width=8,device = "png")
}
