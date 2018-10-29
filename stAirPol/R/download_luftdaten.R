#' Write luftdaten to mysqlite database
#'
#' @param db mysqlite database
#' @param end enddate, e.g. "2019-12-31"
#' @param source folder with csv files
#' @param save boolean, should database be backuped monthly, 2: save daily
#' @param folder folder with database for backups
#' @param dbname character, file name of database (for backup)
#' @param verbose print more information
#'
#' @author Volker Schmid
#'
#' @return no return
#' @export
LD_sql<-function(db, end = Sys.Date()-1, source="/media/schmid/local/data/luftdaten/", save=TRUE,
                 folder="./",dbname="db/luftdaten",verbose=FALSE)
{
  starttime=proc.time()[3]
  x=as.Date(paste(dbReadTable(db,"lastdate")))
  end<-as.Date(end)
  while (x<end)
  {
    x<-x+1
    cat(paste0("Processing ",x,"..."))
    files<-list.files(paste0(source,"/",x))
    files<-files[grep(pattern = ".csv",files)]
    for (file in files)
    {
      temp<-paste0(source,"/",x,"/",file)
      temp<-read.csv(temp,sep=";",header=TRUE)
      if(dim(temp)[1]>0)
      {
        sensortype=as.character(unique(temp$sensor_type))
        sensorid=unique(temp$sensor_id)
        if(is.na(sensorid)|sensorid<1)break
        temp$lat[is.na(temp$lat)]=0
        temp$lon[is.na(temp$lon)]=0

        loc=unique(cbind(temp$lat,temp$lon,temp$location))
        loclength=dim(loc)[1]
        if (loclength==0)break
        locid=rep(NA,loclength)
        for (j in 1:loclength)
        {
          sql=paste0("select * from locid where lat='",loc[j,1],"' AND lon='",loc[j,2],"' AND location='",loc[j,3],"' AND sensor_id='",sensorid,"'")
          quera<-dbGetQuery(db,sql)
          if (dim(quera)[1]==0)
          {
            locid[j]<-dbGetQuery(db,"select max(id) from locid")[1,1]+1
            if (is.na(locid[j]))locid[j]=1
            temp0<-data.frame("id"=locid[j],"sensor_id"=sensorid,"sensor_type"=sensortype,
                              "location"=loc[j,3],"lat"=loc[j,1],"lon"=loc[j,2])
            dbWriteTable(db,"locid",temp0,append=TRUE)
          }
          else
          {
            locid[j]=quera$id
          }
        }

        n<-dim(temp)[1]
        if (loclength==1)
        {
          lloc<-rep(1,loclength)
        }
        else
        {
          lloc<-unlist(lapply(1:n,function(i){for (j in 1:loclength)if (all(temp[i,c(4,5,3)]==loc[j,]))return(j)}))

        }
        locid<-locid[lloc]

        quera<-dbGetQuery(db,paste0("select max(id) from messungen"))[1,1]
        if (is.na(quera))quera=0
        temp0<-data.frame("id"=quera+(1:n), "locid"=locid, "timestamp"=as.character(temp$timestamp))
        dbWriteTable(db,"messungen",temp0,append=TRUE)

        nam<-names(temp)
        temp<-temp[,-which(names(temp)=="sensor_id")]
        temp<-temp[,-which(names(temp)=="sensor_type")]
        temp<-temp[,-which(names(temp)=="location")]
        temp<-temp[,-which(names(temp)=="lat")]
        temp<-temp[,-which(names(temp)=="lon")]
        temp<-temp[,-which(names(temp)=="timestamp")]


        dbWriteTable(db,sensortype,temp,append=TRUE)

        if(0){
          if(any(nam=="P1"))
          {
            P1<-temp$P1
            if(is.null(P1))P1<-rep(NA,n)
            durP1<-temp$durP1
            if(is.null(durP1))durP1<-rep(NA,n)
            ratioP1<-temp$ratioP1
            if(is.null(ratioP1))ratioP1<-rep(NA,n)
            P2<-temp$P2
            if(is.null(P2))P2<-rep(NA,n)
            durP2<-temp$durP2
            if(is.null(durP2))durP2<-rep(NA,n)
            ratioP2<-temp$ratioP2
            if(is.null(ratioP2))ratioP2<-rep(NA,n)
            temp0<-data.frame("id"=quera+(1:n), "P1"=P1, "durP1"=durP1, "ratioP1"=ratioP1, "P2"=P2, "durP2"=durP2, "ratioP2"=ratioP2)
            dbWriteTable(db,"P",temp0,append=TRUE)
          }

          if(any(nam=="temperature"))
          {
            temperature<-temp$temperature
            if(is.null(temperature))temperature<-rep(NA,n)
            humidity<-temp$humidity
            if(is.null(humidity))humidity<-rep(NA,n)
            temp0<-data.frame("id"=quera+(1:n), "temperature"=temperature, "humidity"=humidity)
            dbWriteTable(db,"temphum",temp0,append=TRUE)
          }

          if(any(nam=="pressure"))
          {
            pressure<-temp$pressure
            if(is.null(pressure))pressure<-rep(NA,n)
            altitude<-temp$altitude
            if(is.null(altitude))altitude<-rep(NA,n)
            pressure_sealevel<-temp$pressure_sealevel
            if(is.null(pressure_sealevel))pressure_sealevel<-rep(NA,n)
            temp0<-data.frame("id"=quera+(1:n), "pressure"=pressure, "altitude"=altitude, "pressure_sealevel"=pressure_sealevel)
            dbWriteTable(db,"pressure",temp0,append=TRUE)
          }

          for (i in nam)
          {
            if (!any(i==c("sensor_id","sensor_type", "location", "lat", "lon", "timestamp", "pressure", "altitude",
                          "pressure_sealevel", "temperature", "humidity", "P1", "durP1", "ratioP1", "P2", "durP2", "ratioP2","X")))
              print(c(file,i))
          }
        }
      }
    }
    timing<-round(proc.time()[3]-starttime)
    starttime<-proc.time()[3]
    m<-floor(timing/60)
    s<-round(timing%%60)
    h<-floor(m/60)
    m<-m%%60
    timing<-paste(s)
    if (h>0|m>0)timing<-paste0(m,":",timing)
    if (h>0)timing<-paste0(h,":",timing)

    cat(paste(" finished. Processing time:",timing,"\n"))
    dbWriteTable(db,"lastdate",data.frame("lastdate"=paste(x)),overwrite=TRUE)
    if (save)
    {
      if ((save<2&format(x+1,"%d")=="01")|save==2)
        fs::file_copy(paste0(folder,"/",dbname,".sqlite"), paste0(folder,"/",dbname,".",x,".sqlite"))
    }

  }
}




#' Download csv files from archive.liftdaten.info
#'
#' @param folder folder to store csv
#' @param start start date, e.g. "2015-10-01"
#' @param end end data, e.g. Sys.Date()
#'
#' @import curl
#' @author Volker Schmid
#' @return files are stored in folder/
#' @export
LD_download<-function(folder, start="2015-10-01",end=Sys.Date(), sensortype="")
{
  x=as.Date(start)
  end<-as.Date(end)
  while (x<=end)
  {
    print(x)
    dir.create(paste0(folder,"/",x))
    con<-curl::curl(paste0("http://archive.luftdaten.info/",x))
    test<-try({open(con)})
    if(is.null(test))
    {
      mode=2
      temp=readLines(con, n = 4)
      if (temp[2]=="<html>")
      {
        mode=7
        temp=readLines(con, n = 7)
      }
      while (TRUE){
        out <- readLines(con, n = 1)
        if (out=="</pre><hr></body>")break
        out<-strsplit(out,">")[[1]][mode]
        out<-strsplit(out,"</a")[[1]]
        dl<-TRUE
        if (sensortype!="")if (length(grep("dht",out))==0)
          dl<-FALSE
        if (out=="measurements.txt")break
        dest<-paste0(folder,"/",x,"/",out)
        out<-paste0("http://archive.luftdaten.info/",x,"/",out)
        if(dl)print(out)
        if(dl)curl::curl_download(out, dest)
        Sys.sleep(time = runif(1,0.07,0.19))
      }
    }
    close(con)
    x<-x+1
  }
}


#' Initialize Database
#'
#' @param name name of database
#' @param start start date, e.g. "2018-03-01"
#' @param verbose print additional information
#'
#' @export
#' @import DBI RSQLite
initializeDB<-function(name,start,verbose=FALSE)
{
  start<-as.character(as.Date(start)-1 )
  name<-paste0(getwd(),"/",name,".sqlite")
  if (verbose)cat(paste0("Initializing database ",name,"; lastdate set to ", start, "\n"))
  db <- dbConnect(SQLite(), name)
  dbWriteTable(db,"lastdate",data.frame("lastdate"=start),overwrite=TRUE)
  temp0<-data.frame("id"=integer(),"sensor_id"=integer(),"sensor_type"=character(),
                    "location"=integer(),"lat"=double(),"lon"=double())
  dbWriteTable(db,"locid",temp0,append=TRUE)
  temp0<-data.frame("id"=integer(), "locid"=integer(), "timestamp"=as.Date(character()))
  dbWriteTable(db,"messungen",temp0,append=TRUE)
  dbDisconnect(db)
}



#' Write luftdaten to mysqlite database
#'
#' @param start start date, e.g. "2016-10-01"
#' @param end enddate, e.g. "2019-12-31"
#' @param source folder with csv files
#' @param sensortype vector with sensortypes to process
#' @param dbname character, file name of database (for backup)
#' @param verbose print more information
#'
#' @import RSQLite DBI
#' @author Volker Schmid
#' @return no return
#' @export
LD_sql2<-function(start, end=Sys.Date()-1, source="/media/schmid/local/data/luftdaten/",
                  sensortype=c(), dbname="db/luftdaten",verbose=FALSE)
{
  starttime=proc.time()[3]
  x=as.Date(start)
  end<-as.Date(end)
  while (x<=end)
  {
    cat(paste0("Processing ",x,"..."))
    files<-list.files(paste0(source,"/",x))
    files<-files[grep(pattern = ".csv",files)]
    if (length(sensortype)>0){
      f0<-c()
      for (s in sensortype)f0<-c(f0,files[grep(pattern = s,files)])
      files<-f0
    }

    for (file in files)
    {
      if (verbose)print(file)
      temp<-paste0(source,"/",x,"/",file)
      temp<-read.csv(temp,sep=";",header=TRUE)
      if(dim(temp)[1]>0)
      {
        sensor_type=as.character(unique(temp$sensor_type))
        db<-dbConnect(SQLite(),paste0(dbname,sensor_type,".sqlite"))
        test<-array(0,c(0,1))
        try(test<-dbGetQuery(db,paste0("SELECT * from processed where file='",file,"'")),silent=TRUE)
        if (dim(test)[1]>0){
          dbDisconnect(db)
          if (verbose)print("already done!")
        }
        else
        {
          #if (verbose)print("let's do it!")
          sensorid=unique(temp$sensor_id)
          if(is.na(sensorid)|sensorid<1)break
          temp$lat[is.na(temp$lat)]=0
          temp$lon[is.na(temp$lon)]=0

          loc=unique(cbind(temp$lat,temp$lon,temp$location))
          loclength=dim(loc)[1]
          if (loclength==0)break
          locid=rep(NA,loclength)
          for (j in 1:loclength)
          {
            sql=paste0("select * from locid where lat='",loc[j,1],"' AND lon='",loc[j,2],"' AND location='",loc[j,3],"' AND sensor_id='",sensorid,"'")
            test<-try(quera<-dbGetQuery(db,sql),silent=TRUE)
            if (class(test)=="try-error")
            {
              temp0<-data.frame("id"=1,"sensor_id"=sensorid,"sensor_type"=sensor_type,
                                "location"=loc[j,3],"lat"=loc[j,1],"lon"=loc[j,2])
              dbWriteTable(db,"locid",temp0)
              locid[j]=1
            }
            else{
              if (dim(quera)[1]==0)
              {
                locid[j]<-dbGetQuery(db,"select max(id) from locid")[1,1]+1
                if (is.na(locid[j]))locid[j]=1
                temp0<-data.frame("id"=locid[j],"sensor_id"=sensorid,"sensor_type"=sensor_type,
                                  "location"=loc[j,3],"lat"=loc[j,1],"lon"=loc[j,2])
                dbWriteTable(db,"locid",temp0,append=TRUE)
              }
              else
              {
                locid[j]=quera$id
              }
            }
          }

          n<-dim(temp)[1]
          if (loclength==1)
          {
            lloc<-rep(1,loclength)
          }
          else
          {
            lloc<-unlist(lapply(1:n,function(i){for (j in 1:loclength)if (all(temp[i,c(4,5,3)]==loc[j,]))return(j)}))
          }
          locid<-locid[lloc]

          nam<-names(temp)
          temp<-temp[,-which(names(temp)=="sensor_id")]
          temp<-temp[,-which(names(temp)=="sensor_type")]
          temp<-temp[,-which(names(temp)=="location")]
          temp<-temp[,-which(names(temp)=="lat")]
          temp<-temp[,-which(names(temp)=="lon")]
          if (any(nam=="X"))temp<-temp[,-which(names(temp)=="X")]

          if (!is.null(dim(temp)))
          {
            temp$locid <- locid
            dbWriteTable(db,"data",temp,append=TRUE)
          }

          dbWriteTable(db,"processed",data.frame("file"=file),append=TRUE)

          dbDisconnect(db)
        }
      }
    }
    x<-x+1
    timing<-round(proc.time()[3]-starttime)
    starttime<-proc.time()[3]
    m<-floor(timing/60)
    s<-round(timing%%60)
    h<-floor(m/60)
    m<-m%%60
    if (m<10)m<-paste0("0",m)
    if (s<10)s<-paste0("0",s)
    timing<-paste0(h,":",m,":",s)
    cat(paste("finished. Processing time:",timing,"\n"))

  }
}
