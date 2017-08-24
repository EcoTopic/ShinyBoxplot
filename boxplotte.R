library("RMySQL", "DBI")


plotGraph <-function(dat,xlabels,ylabels,caption,soil,i,ymin,ymax){
  boxplot(dat$cropYield ~ dat$fertiliser, data=dat, ylim=c(ymin,ymax), col="blue",cex.axis=1.5,xaxt='n', family="Helvetica")
  # x axis 
  axis(1, at=seq(1,length(xlabels)), labels=xlabels, cex.axis=1.5)
  if (i=="medium"){
    mtext(side=1, "kg N/ha", line = 3, cex=1.5)
  }
  #y axis
  #mtext(side=2, ylabels, line = 2.5, cex=1)
  if (i=="early"){
    mtext(side=2, ylabels, line = 3, cex=1.5)
  }
  #title
  #mtext(side=3, paste(caption,soil,i, sep=' '), line = 2, cex=2)
  mtext(side=3, paste(i), line = 2, cex=1.5)
  
}

plotGraph2 <-function(dat,xlabels,ylabels,soil,i,ymin,ymax){
  #boxplot(dat$cropYield ~ dat$cultivar, data=dat, ylim=c(ymin,ymax), col="blue",xaxt='n',xlab="cultivar",ylab=ylabels, cex.axis=0.5, cex.lab=0.5)
  boxplot(dat$cropYield ~ dat$cultivar, data=dat, ylim=c(ymin,ymax), col="blue",xaxt='n',xlab="cultivar",ylab=ylabels, cex.axis=1, cex.lab=1.5, family="Helvetica")
  # x axis 
  if (length(xlabels)==4){
    axis(1, at=seq(1,length(xlabels)), labels=xlabels, cex.axis=.7)
  }
  else{
    axis(1, at=seq(1,length(xlabels)), labels=xlabels, cex.axis=1.0)
  }
  #mtext(side=1, "cultivar", line = 3, cex=1)
  #y axis
  #mtext(side=2, ylabels, line = 2.5, cex=1)
  #title
  #mtext(side=3, paste(soil,i, sep=' '), line = 2, cex=1)
  #title(xlab="cultivar",ylab=ylabels)
}

fetchData<-function(crop,country,metGrid,soilSelect,seasonSelect){
  con<-dbConnect(MySQL(),
                 # user='cropdata',
                 # password='!@Cr0pd@ta~!',
                 # host='localhost',
                 user='root',
                 password='cedarberg',
                 host='127.0.0.1',
                 dbname='cropdata')
  if(crop=="maize" | crop=="sorghum"| (crop=="bean" && country=="RSA")){
    statement<-paste('SELECT metGrid,cultivar,fertiliser,soil,year,rainClass,cropYield FROM ',crop,country,' WHERE metGrid="',metGrid,'" AND soil=',soilSelect,' AND rainClass="',seasonSelect,'";',sep='')
  }
  else{
  statement<-paste('SELECT metGrid,cultivar,soil,year,rainClass,cropYield FROM ',crop,country,' WHERE metGrid="',metGrid,'" AND soil=',soilSelect,' AND rainClass="',seasonSelect,'";',sep='')
  }#print(statement)
  query <- dbSendQuery(con,statement)
  dat <- dbFetch(query, n=-1)
  dbClearResult(query)
  #dat<-dbGetQuery(con,statement)
  dbDisconnect(con)   
  return(dat)
}

listOfMetFiles<-function(){
    con<-dbConnect(MySQL(),
                   # user='cropdata',
                   # password='!@Cr0pd@ta~!',
                   # host='localhost',
                   user='root',
                   password='cedarberg',
                   host='127.0.0.1',
                   dbname='cropdata')
#  query <- dbSendQuery(con,'select distinct metGrid from (select metGrid from maizeRSA union select metGrid from maizeBOT union select metGrid from maizeKENYA union select metGrid from maizeMAUR union select metGrid from maizeZWE) t;')
  query <- dbSendQuery(con,'select DISTINCT metGrid from metGrids;')
  metList= dbFetch(query, n=-1)
  dbClearResult(query)
  dbDisconnect(con)
  return(metList[[1]])
}

killDbConnections <- function () {
  
  all_cons <- dbListConnections(MySQL())
  
  #print(all_cons)
  
  for(con in all_cons)
    +  dbDisconnect(con)
  
  #print(paste(length(all_cons), " connections killed."))
  
}