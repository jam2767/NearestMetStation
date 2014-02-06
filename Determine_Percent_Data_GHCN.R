# Determine Percent Data
Determine_Percent_Data_GHCN <- function(st.id,
                                        year.start.pheno,
                                        year.end.pheno,
                                        data.type,
                                        completeness.required){

  dir.create(paste("Met_Data_Point/",st.id,sep=""))
  
  downloadURL = paste("ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/all/",sep = "")
  file_name = paste(st.id,".dly",sep ="")
  
  if (data.type =="point"){
    try(download.file(paste(downloadURL,file_name,sep=""), 
                      paste("Met_Data_Point/",st.id,"/",file_name,sep="")),silent=TRUE)
  }else{
    try(download.file(paste(downloadURL,file_name,sep=""), 
                      paste("Met_Data_Slope/",USAF,"/",file_name,sep="")),silent=TRUE)
  } 
  
  
  
  if (data.type =="point"){
    files=dir(paste("Met_Data_Point","/",USAF,sep=""),"*.op.gz",full.names=T)
  }else{
    files=dir(paste("Met_Data_Slope","/",USAF,sep=""),"*.op.gz",full.names=T)
  }
  
  daily_widths = c(5,1,1,1)
  widths = c(11,4,2,4,rep(daily_widths,31))
  columns = c("ID","YEAR","MONTH","ELEMENT","VAL01","MF01","QF01","SF01",
              "VAL02","MF02","QF02","SF02",
              "VAL03","MF03","QF03","SF03",
              "VAL04","MF04","QF04","SF04",
              "VAL05","MF05","QF05","SF05",
              "VAL06","MF06","QF06","SF06",
              "VAL07","MF07","QF07","SF07",
              "VAL08","MF08","QF08","SF08",
              "VAL09","MF09","QF09","SF09",
              "VAL10","MF10","QF10","SF10",
              "VAL11","MF11","QF11","SF11",
              "VAL12","MF12","QF12","SF12",
              "VAL13","MF13","QF13","SF13",
              "VAL14","MF14","QF14","SF14",
              "VAL15","MF15","QF15","SF15",
              "VAL16","MF16","QF16","SF16",
              "VAL17","MF17","QF17","SF17",
              "VAL18","MF18","QF18","SF18",
              "VAL19","MF19","QF19","SF19",
              "VAL20","MF20","QF20","SF20",
              "VAL21","MF21","QF21","SF21",
              "VAL22","MF22","QF22","SF22",
              "VAL23","MF23","QF23","SF23",
              "VAL24","MF24","QF24","SF24",
              "VAL25","MF25","QF25","SF25",
              "VAL26","MF26","QF26","SF26",
              "VAL27","MF27","QF27","SF27",
              "VAL28","MF28","QF28","SF28",
              "VAL29","MF29","QF29","SF29",
              "VAL30","MF30","QF30","SF30",
              "VAL31","MF31","QF31","SF31")
  
  # Open GHCN Data
  ghcn_data = read.fwf(paste("Met_Data_Point/",st.id,"/",file_name,sep=""),
                       widths,header=FALSE,col.names=columns)
  
  # Throw out everything but Tmax, Tmin, Precip
  ghcn_data = subset(ghcn_data,ghcn_data$ELEMENT == "PRCP" | ghcn_data$ELEMENT == "TMAX" | ghcn_data$ELEMENT == "TMAX")
  
  # Throw out unnesccesary years
  ghcn_data = subset(ghcn_data, ghcn_data$YEAR >= year.start.pheno & ghcn_data$YEAR <= year.end.pheno )
  
  # Throw out ID and flags
  cols_to_keep = c("YEAR","MONTH","ELEMENT","VAL01",
                   "VAL02","VAL03","VAL04","VAL05","VAL06","VAL07","VAL08","VAL09",
                   "VAL10","VAL11","VAL12","VAL13","VAL14","VAL15","VAL16","VAL17",
                   "VAL18","VAL19","VAL20","VAL21","VAL22","VAL23","VAL24","VAL25",
                   "VAL26","VAL27","VAL28","VAL29","VAL30","VAL31")
  ghcn_data =ghcn_data[,cols_to_keep] 
  
  # change -9999 to NA
  ghcn_data[ghcn_data==-9999] = NA
  
  # 
  start_year=min(substr(files,nchar(files)-9,nchar(files)-6))
  end_year=max(substr(files,nchar(files)-9,nchar(files)-6))
  alldates=data.frame(fdate=seq(from=as.Date(paste(start_year,"-01-01",sep="")), 
                                to=as.Date(paste(end_year,"-12-31",sep="")), by=1))
  
  stn=matrix()
  tmin=matrix()
  tmax=matrix()
  ppt=matrix()
  fdate=matrix()
  
  for (tmpfile in files){
    #
    # There is more data in this dataset we can extract later as we need it.
    #
    #
    tmpstring<-grep("MAX",readLines(gzfile(tmpfile)),value=TRUE,invert=TRUE)
    stn<-c(stn,as.numeric(as.character(substring(tmpstring,1,5))))
    tmax<-c(tmax,as.numeric(as.character(substring(tmpstring,103,108))))
    tmin<-c(tmin,as.numeric(as.character(substring(tmpstring,111,116))))
    ppt<-c(ppt,as.numeric(as.character(substring(tmpstring,119,123))))
    fdate<-c(fdate,as.Date(yearmoda<-substring(tmpstring,15,22),
                           "%Y%m%d")) }
  
  stn<-as.numeric(stn)
  ppt<-as.numeric(ppt)
  tmax<-as.numeric(tmax)
  tmin<-as.numeric(tmin)
  fdate<-as.Date(as.numeric(fdate), origin="1970-01-01")
  forcing=data.frame(stn=stn,ppt=ppt,tmax=tmax,tmin=tmin,
                     fdate=as.Date(fdate))
  forcing=na.omit(forcing)
  forcing=merge(alldates,forcing,all=TRUE)
  forcing$ppt_mm <- forcing$ppt*25.4
  forcing$tmax_C <- round((forcing$tmax-32)*5/9, digits=2)
  forcing$tmin_C <- round((forcing$tmin-32)*5/9, digits=2)
  forcing$ppt_mm[forcing$ppt_mm > 999]=0.0
  
  percent_data = nrow(na.omit(forcing))/nrow(forcing)
  
  if (percent_data >= completeness.required){
    if (data.type =="point"){
      write.csv(file = paste("Formatted_Met_Data_Point/",USAF), forcing, row.names=FALSE)
    }else{
      write.csv(file = paste("Formatted_Met_Data_Slope/",USAF), forcing, row.names=FALSE)
    }
  }
  
  return(percent_data)
}


