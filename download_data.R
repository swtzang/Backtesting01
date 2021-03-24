### This built-in function code pulls stock price data for the specified ticker and duration.
### The data is pulled from Google Finance.

# http://www.traderji.com/equities/38117-google-finance.html
# http://www.calculatinginvestor.com/2013/09/19/downloading-batch-returns/

#################################################
#   Pull daily Price data from Google Finance   #
#################################################

daily_price_data = function(symbol,noDays){
  
  #symbol = "AXISBANK" 
  #noDays = 1 # Google finance can go back upto 15 days for 1-minute data.
  interval = 86400 #Seconds #86400 in case of data per day #60 in case of intra-day data
  dirPath = paste(paste(getwd(),"/",sep=""), "_data/", sep="") 
  fileName = paste(dirPath,symbol,".csv",sep="") 
  
  # this code below will download multiple symbols in one go.
  #download.file(paste("http://www.google.com/finance/getprices?q=",symbol,"&x=NSE&i=",interval,"&p=",noDays,"Y&f=d,o,h,l,c,v,t",sep=""), fileName,method="libcurl",quiet=TRUE) 
  
  # this code below will download one symbol at a  time.
  download.file(paste("http://www.google.com/finance/getprices?q=",symbol,"&x=NSE&i=",interval,"&p=",noDays,"Y&f=d,o,h,l,c,v,t",sep=""), fileName,quiet = TRUE) 
  unix2POSIXct <- function (time) structure(time, class = c("POSIXt", "POSIXct")) 
  
  fileName1 = paste(dirPath,symbol,".csv",sep="") 
  
  data_raw = read.table(fileName1,sep=",",col.names=c("DATE1","CLOSE","HIGH","LOW","OPEN","VOLUME"),fill=TRUE)  
  
  data =tail(data_raw, nrow(data_raw) - 7)
  colnames(data) = c("DATE1","CLOSE","HIGH","LOW","OPEN","VOLUME")
  
  dd = numeric(nrow(data))
  tt = numeric(nrow(data))
  
  condition  = (substr(as.vector((data$DATE1)),1,1) == "a") 
  
  for (i in 1:nrow(data))
  { 
    if(condition[i])
    { 
      tempDate = unix2POSIXct(as.numeric(substr(as.vector((data$DATE1[i])),2,nchar(as.vector((data$DATE1[i]))))))
      dd[i] = as.numeric(format(tempDate,format="%Y%m%d")) 
      tt[i] = as.numeric(format(tempDate,format="%H%M")) 
    } else { 
      tempDate1 = tempDate + as.numeric(as.vector(data$DATE1[i]))*interval 
      dd[i] = as.numeric(format(tempDate1,format="%Y%m%d")) 
      tt[i] = as.numeric(format(tempDate1,format="%H%M")) 
    } 
    
    data$DATE = dd
    data$TIME = tt
    
  }
  
  data1=as.data.frame(data) 
  data1=(data1[data1$TIME>915 & data1$TIME<=1530,]) 
  
  finalData = data.frame(DATE=as.vector(data1$DATE),TIME=as.vector(data1$TIME),CLOSE=as.vector(data1$CLOSE),HIGH=as.vector(data1$HIGH),LOW=as.vector(data1$LOW),OPEN=as.vector(data1$OPEN),VOLUME=as.vector(data1$VOLUME)) 
  finalData = data.frame(DATE=data1$DATE,TIME=data1$TIME,CLOSE=data1$CLOSE,HIGH=data1$HIGH,LOW=data1$LOW,OPEN=data1$OPEN,VOLUME=data1$VOLUME) 
  
  write.csv(finalData,file=fileName,row.names=FALSE)
  
  
  
}


#################################################
# Pull Intraday Price data from Google Finance  #
#################################################

intraday_price_data = function(symbol,noDays){
  
  #symbol = "AXISBANK" 
  #noDays = 1 # Google finance can go back upto 15 days for 1-minute data.
  interval = 60 #Seconds #86400 in case of data per day #60 in case of intra-day data
  dirPath = paste(getwd(),"/",sep="") 
  fileName = paste(dirPath,symbol,".csv",sep="") 
  
  # this code below will download multiple symbols in one go.
  #download.file(paste("http://www.google.com/finance/getprices?q=",symbol,"&x=NSE&i=",interval,"&p=",noDays,"Y&f=d,o,h,l,c,v,t",sep=""), fileName,method="libcurl",quiet=TRUE) 
  
  download.file(paste("http://www.google.com/finance/getprices?q=",symbol,"&x=NSE&i=",interval,"&p=",noDays,"d&f=d,o,h,l,c,v,t",sep=""), fileName, quiet = TRUE) 
  unix2POSIXct <- function (time) structure(time, class = c("POSIXt", "POSIXct")) 
  data_raw = read.table(fileName,sep=",",col.names=c("DATE1","CLOSE","HIGH","LOW","OPEN","VOLUME"),fill=TRUE)  
  
  
  data =tail(data_raw, nrow(data_raw) - 7)
  colnames(data) = c("DATE1","CLOSE","HIGH","LOW","OPEN","VOLUME")
  
  dd = numeric(nrow(data))
  tt = numeric(nrow(data))
  
  condition  = (substr(as.vector((data$DATE1)),1,1) == "a") 
  
  for (i in 1:nrow(data))
  { 
    if(condition[i])
    { 
      tempDate = unix2POSIXct(as.numeric(substr(as.vector((data$DATE1[i])),2,nchar(as.vector((data$DATE1[i]))))))
      dd[i] = as.numeric(format(tempDate,format="%Y%m%d")) 
      tt[i] = as.numeric(format(tempDate,format="%H%M")) 
    } else { 
      tempDate1 = tempDate + as.numeric(as.vector(data$DATE1[i]))*interval 
      dd[i] = as.numeric(format(tempDate1,format="%Y%m%d")) 
      tt[i] = as.numeric(format(tempDate1,format="%H%M")) 
    } 
    
    data$DATE = dd
    data$TIME = tt
    
  }
  
  data1=as.data.frame(data) 
  data1=(data1[data1$TIME>915 & data1$TIME<=1530,]) 
  
  finalData = data.frame(DATE=as.vector(data1$DATE),TIME=as.vector(data1$TIME),CLOSE=as.vector(data1$CLOSE),HIGH=as.vector(data1$HIGH),LOW=as.vector(data1$LOW),OPEN=as.vector(data1$OPEN),VOLUME=as.vector(data1$VOLUME)) 
  finalData = data.frame(DATE=data1$DATE,TIME=data1$TIME,CLOSE=data1$CLOSE,HIGH=data1$HIGH,LOW=data1$LOW,OPEN=data1$OPEN,VOLUME=data1$VOLUME) 
  
  write.csv(finalData,file=fileName,row.names=FALSE)
  
}
