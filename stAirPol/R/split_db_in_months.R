
#' Split databases into monthly databases
#'
#' @param path filepath where the informations should be stored
#'
#' @export
#' @author Volker Schmid
#' @import RSQLite
#' @examples
#' split_db_in_months(path = './')
split_db_in_months <- function(path) {
  files <- list.files(path, full.names = TRUE, include.dirs = FALSE)
  files <- files[!grepl('/data', files)]
  for (f in files)
  {
    db<-dbConnect(SQLite(),f)
    dbListTables(db)
    print(f)
    fields<-dbListFields(db, "data")
    locid<-dbReadTable(db,"locid")
    res<-dbSendQuery(db,"SELECT * from data")
    temp<-dbFetch(res,50000)
    tialt<-""

    while(dim(temp)[1]>0)
    {
      time<-strsplit(temp$timestamp,"-")
      time<-matrix(unlist(time),ncol=dim(temp)[1])
      time<-t(time[1:2,])
      ti<-unique(time)
      if (!assertthat::are_equal(ti,tialt))print(ti)
      tialt<-ti
      for (i in 1:dim(ti)[1])
      {
        w<-(time[,1]==ti[i,1])&(time[,2]==ti[i,2])
        temp0<-temp[w,]
        for (field in c("P1","P2","temperature","humidity","pressure"))
        {
          if (any(names(temp0)==field))
          {
            w0<-which(names(temp0)==field)
            db0<-dbConnect(SQLite(),paste0(path, "/data/",field,"-",ti[i,1],"-",ti[i,2],".sqlite"))
            w1<-which(!is.na(as.numeric(temp0[,w0])))
            temp2<-data.frame("timestamp"=temp0$timestamp[w1], "locid"=temp0$locid[w1],"value"=temp0[w1,w0])
            dbWriteTable(db0,"data",temp2,append=TRUE)
            locid2<-unique(temp2$locid)
            for (l in locid2)
            {
              temp2<-try(dbGetQuery(db0, paste0("SELECT * from locid where id=",l)),silent=TRUE)

              if (class(temp2)=="try-error")
              {
                locid0<-locid[which(locid$id==l),]
                dbWriteTable(db0,"locid",locid0)
              }
              else
              {
                if (dim(temp2)[1]==0)
                {
                  locid0<-locid[which(locid$id==l),]
                  dbWriteTable(db0,"locid",locid0, append=TRUE)
                }
              }
            }
            dbDisconnect(db0)
          }
        }
      }
      temp<-dbFetch(res,10000)
    }
  }
  if(0)
  {
    res<-dbSendQuery(db,"SELECT * from processed")
    temp<-dbFetch(res,10000)
    while(dim(temp)[1]>0)
    {
      time<-strsplit(temp$file,"-")
      time<-matrix(unlist(time),nrow=3)
      time<-t(time[-3,])
      ti<-unique(time)
      for (i in 1:dim(ti)[1])
      {
        w<-(time[,1]==ti[i,1])&(time[,2]==ti[i,2])
        temp0<-temp[w,]
        db0<-dbConnect(SQLite(),paste0("db/DHT22-",ti[1],"-",ti[2],".sqlite"))
        dbWriteTable(db0,"processed",temp0,append=TRUE)
        dbDisconnect(db0)
      }

      temp<-dbFetch(res,10000)
    }

  }
}
