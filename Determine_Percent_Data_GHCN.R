# Determine Percent Data
Determine_Percent_Data_GHCN <- function(st.id,
                                        year.start.pheno,
                                        year.end.pheno,
                                        data.type,
                                        completeness.required){
  
  dir.create(paste("Met_Data_Point/",st.id,sep=""))
  
  year_st = year.start.pheno
  year_end = year.end.pheno
  
  year_list = year_st:year_end
  
  for (YEAR in year_list) {
    
    downloadURL = paste("ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/all/",sep = "")
    file_name = paste(st.id,".dly",sep ="")
    
    if (data.type =="point"){
      try(download.file(paste(downloadURL,file_name,sep=""), 
                        paste("Met_Data_Point/",st.id,"/",file_name,sep="")),silent=TRUE)
    }else{
      try(download.file(paste(downloadURL,file_name,sep=""), 
                        paste("Met_Data_Slope/",USAF,"/",file_name,sep="")),silent=TRUE)
    } 
  }
  
  
  if (data.type =="point"){
    files=dir(paste("Met_Data_Point","/",USAF,sep=""),"*.op.gz",full.names=T)
  }else{
    files=dir(paste("Met_Data_Slope","/",USAF,sep=""),"*.op.gz",full.names=T)
  }
  
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
    tmpstring<-grep("MAX",readLines( gzfile(tmpfile)),value=TRUE,invert=TRUE)
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

