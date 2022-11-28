library(ggplot2)
library(lubridate)
library(dplyr)
library(data.table) #rename
library(stringr)


# Axcelerometer calibrations
# Garde B, Wilson RP, Fell A, Cole N, Tatayah V, Holton MD, Rose KAR, Metcalfe RS, 
# Robotka H, Wikelski M, Tremblay F, Whelan S, Elliott KH, Shepard ELC (2021) 
# Ecological inference using data from accelerometers needs careful protocols. 
# Methods in Ecology and Evolution:13.

if(Sys.info()[7]=="rachaelorben") {
  usrdir<-"/Users/rachaelorben/Box/DASHCAMS/data/Calibration_Data_Accel/"
  datdir<-'/Users/rachaelorben/Box/DASHCAMS/data/ornitela_ftp_data'
  deplymatrix<-'/Users/rachaelorben/Box/DASHCAMS/data/Field Data/DASHCAMS_Deployment_Field_Data.csv'
}

if(Sys.info()[7]=="alexapiggot") { #adjust this to be the name of your computer when you run "Sys.info()[7]"
  usrdir<-"/Users/rachaelorben/Box/DASHCAMS/data/Calibration_Data_Accel/"
  datdir<-'/Users/rachaelorben/Box/DASHCAMS/data/ornitela_ftp_data'
  deplymatrix<-'/Users/rachaelorben/Box/DASHCAMS/data/Field Data/DASHCAMS_Deployment_Field_Data.csv'
}

# find calibration data ----------------------------------------------------------
# this section uses the file "Accel_Cal_Index.csv" that is stored in BOX. 
# You need to update it with the logger information and calibration dates before running the code.

# The below uses the calibration date to make a download file name so you can find the data. The method kind of works, 
# but only if the data was uploaded the day of calibration or the day after. This hasn't always been the case. There is probably a better way to find the data you want. 
# See the "TagStatus_1wk.R" for a different way to find the more recent data. That code could be repurposed here to make a wider date window. 

cal_idx<-read.csv(paste0(usrdir,"Accel_Cal_Index.csv"))
cal_idx<-cal_idx%>%filter(calprocessed_yn=="n")%>%select(-calprocessed_yn) #filters out IDs that need the calibrarion data processed.

cal_idx$Start_UTC<-mdy_hm(cal_idx$cal_start_datetime_local)-(cal_idx$utc_offset*3600) #hour before
cal_idx$End_UTC<-mdy_hm(cal_idx$cal_end_datetime_local)-(cal_idx$utc_offset*3600) #hour after
names(cal_idx)
cal_idx$dt_ch<-paste0(year(cal_idx$Start_UTC),
                      str_pad(month(cal_idx$Start_UTC),2,pad=0),
                      str_pad(day(cal_idx$Start_UTC),2,pad=0))
cal_idx$dt_ch2<-paste0(year(cal_idx$Start_UTC),
                      str_pad(month(cal_idx$Start_UTC),2,pad=0),
                      str_pad(day(cal_idx$Start_UTC)+1,2,pad=0))
cal_idx$file1<-paste0(cal_idx$tag_id,"_GPRS_",cal_idx$dt_ch) #finds transmissions the day of calibration
cal_idx$file2<-paste0(cal_idx$tag_id,"_GPRS_",cal_idx$dt_ch2) #finds transmissions the day after of calibration

Files<-list.files(datdir,pattern = ".csv",full.names = TRUE)

calib_dat<-NULL
for (j in 1:nrow(cal_idx)){
  
  c<-Files[str_detect(Files, cal_idx$file1[j], negate = FALSE)]
  b<-Files[str_detect(Files, cal_idx$file2[j], negate = FALSE)]
  a<-rbind(c,b)
  
  if (length(a)<1){print (paste0("missing! ",cal_idx$file1[j]))}
  if (length(a)<1) next
    
  for(i in 1:length(a)){
    dat<-fread(file=a[i],stringsAsFactors=FALSE,sep = ",",fill=TRUE) 
    calib_dat<-bind_rows(calib_dat,dat)
  }
}

str(calib_dat)
calib_dat$UTC_datetime<-ymd_hms(calib_dat$UTC_datetime)
calib_dat$date<-date(calib_dat$UTC_datetime)

caldat<-NULL
for (j in 1:nrow(cal_idx)){
  tag<-calib_dat%>%filter(device_id==cal_idx$tag_id[j])%>%
    filter(UTC_datetime>cal_idx$Start_UTC[j]-860000)%>%
    filter(UTC_datetime<cal_idx$End_UTC[j]+36000)
  caldat<-rbind(caldat,tag)
}

rm(calib_dat)
caldat$X<-caldat$acc_x; caldat$Y<-caldat$acc_y; caldat$Z<-caldat$acc_z; #renames columns

#vectoral_sum for calibration
caldat$vectoral_sum<-(caldat$X^2+caldat$Y^2+caldat$Z^2)^0.5


# find beginning and end of calibration data chunck _approx! --------------
dt<-Sys.Date()

IDs<-unique(caldat$device_id)
caldat$oid=1:nrow(caldat)

calib.info<-data.frame()

quartz() #windows()
for (j in 1:length(IDs)){
  
  B1<-caldat%>%filter(device_id==IDs[j])%>%filter(vectoral_sum>.8)
  if(nrow(B1)<10) {print(paste0("no data for ", IDs[j]))}
  if(nrow(B1)<10) next
  print("If the calibration segments aren't visiable click the two points very close together")
  
  x=1:nrow(B1)
  y=B1$vectoral_sum
  p1<-plot(x=x, y=y,col="blue", pch=20)
  print(j)
  print(IDs[j])
  print("Click on Beginning of Axy calibration segment (in quartz press esc to advance)")
  beg_pt<-identify(x=x,y=y)
  print("Click on End of Axy calibration segment")
  end_pt<-identify(x=x,y=y)
  print("*****************************")
  
  calib.info.1tag<-cbind(TagID=IDs[j],beg_pt,end_pt)
  calib.info<-rbind(calib.info, calib.info.1tag)
}

calib.info
write.csv(calib.info,paste0(usrdir,"/CalibrationChunck_",dt,".csv"))


calib.info$diff<-abs(calib.info$end_pt-calib.info$beg_pt)

calib.info<-calib.info%>%filter(diff<300) #removes tags without calibration data

# Finds the 6 calibration segments ----------------------------------------
# you may want to change this to save the output for each tag separately so that it is easier to find those data. 

calib.info.SixSegs<-data.frame()
calib.dat<-data.frame()
tIDs<-unique(calib.info$TagID)

quartz()
for (i in 1:length(tIDs)){
  B1<-caldat%>%filter(device_id==tIDs[i])
  Idx<-calib.info%>%filter(TagID==tIDs[i])
  calDat<-B1[Idx$beg:Idx$end_pt,]
  
  x=1:nrow(calDat)
  y=calDat$vectoral_sum
  p1<-plot(x=x, y=y,col="blue", pch=20)
  
  Cdat<-data.frame(row=1)
  Cdat$TagID<-IDs[i]
  
  print("1 Beginning X1"); Cdat$beg_X1<-identify(x=x,y=y)
  print("End Point"); Cdat$end_X1<-identify(x=x,y=y)
  
  print("2 Beginning X2"); Cdat$beg_X2<-identify(x=x,y=y)
  print("End Point"); Cdat$end_X2<-identify(x=x,y=y)
  
  print("3 Beginning Z1"); Cdat$beg_Z1<-identify(x=x,y=y)
  print("End Point"); Cdat$end_Z1<-identify(x=x,y=y)
  
  print("4 Beginning Z2"); Cdat$beg_Z2<-identify(x=x,y=y)
  print("End Point"); Cdat$end_Z2<-identify(x=x,y=y)
  
  print("5 Beginning Y1"); Cdat$beg_Y1<-identify(x=x,y=y)
  print("End Point"); Cdat$end_Y1<-identify(x=x,y=y)
  
  print("6 Beginning Y2"); Cdat$beg_Y2<-identify(x=x,y=y)
  print("End Point"); Cdat$end_Y2<-identify(x=x,y=y)
  
  calDat$x<-1:nrow(calDat)
  
  calDat$calib.seg<-NA
  calDat$calib.seg[Cdat$beg_X1:Cdat$end_X1]<-"X1"
  calDat$calib.seg[Cdat$beg_X2:Cdat$end_X2]<-"X2"
  calDat$calib.seg[Cdat$beg_Z1:Cdat$end_Z1]<-"Z1"
  calDat$calib.seg[Cdat$beg_Z2:Cdat$end_Z2]<-"Z2"
  calDat$calib.seg[Cdat$beg_Y1:Cdat$end_Y1]<-"Y1"
  calDat$calib.seg[Cdat$beg_Y2:Cdat$end_Y2]<-"Y2"
  
  
  Csegs<-ggplot()+
    geom_point(data=calDat,aes(x=UTC_datetime,y=vectoral_sum, group=calib.seg, color=calib.seg))+
    ylim(.9,2.1)
  Csegs
  ggsave(Csegs,
         filename = paste0(usrdir,"/Calib_Segments_",IDs[i],"_",dt,".png"),
         width = 10,height=10)  


  A<-menu(c("Yes", "No"), title="Do you want to use this this?")
  if(A==1){Cdat$quality<-"good"}
  if(A==2){Cdat$quality<-"poor"}
  
  calib.info.SixSegs<-rbind(calib.info.SixSegs, Cdat) #this is what you would need to save - I think
  #saveRDS(alib.info.SixSegs,paste0(usrdir,"/CalibrationProcessing/",dt,"_",tIDs[i],"_Calibration_SegID_data.csv")) #maybe something like this
  calib.dat<-rbind(calib.dat, calDat)
}


#write.csv(calib.dat,paste0(usrdir,"/CalibrationProcessing/Calibration_SegID_data_",dt,".csv"))
saveRDS(calib.dat,paste0(usrdir,"/CalibrationProcessing/",dt,"_Calibration_SegID_data.rda"))


# Correct calibration data segment and plot -------------------------------
Files<-list.files(datdir,pattern = "_Calibration_SegID_data.csv",full.names = TRUE) #this could be adjusted to pull in all the loggers

for (i in 1:length(Files)){

calib.dat<-readRDS(Files[i])
names(calib.dat)

corrections<-calib.dat%>%filter(is.na(calib.seg)==FALSE)%>%
  group_by(TagID, calib.seg)%>%
  summarise(n_values=n(),
            uVecSum=mean(vectoral_sum), 
            min=min(X),
            max=max(X))

corrections$axis<-str_remove(corrections$calib.seg, "[12]")
corrections$seg<-str_remove(corrections$calib.seg, "[XYZ]")
#this needs to be finished and results saved

}







