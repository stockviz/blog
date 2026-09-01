#!/usr/bin/env Rscript
# MCX liquidity and continuity audit for the planned commodity sleeve.
# A sleeve is admitted only after this audit; no currency data is assumed.
options(stringsAsFactors=FALSE, scipen=100)
args<-commandArgs(FALSE); h<-grep("^--file=",args,value=TRUE); OUT<-if(length(h))dirname(normalizePath(sub("^--file=","",h),mustWork=FALSE)) else getwd()
library(RODBC)
source(Sys.getenv("STOCKVIZ_CONFIG",unset="/mnt/hollandC/StockViz/R/config.r"))
con<-odbcDriverConnect(sprintf("Driver={ODBC Driver 17 for SQL Server};Server=%s;Database=%s;Uid=%s;Pwd=%s;",ldbserver,ldbname,ldbuser,ldbpassword),case="nochange",believeNRows=TRUE)
cands<-c("GOLD","SILVER","CRUDEOIL","NATURALGAS","COPPER")
out<-list()
for (cm in cands) {
 q<-sprintf("select contract, expiry_series, expiry, time_stamp, px_close, otype from BHAV_COM_MCX where contract='%s' and OTYPE in ('FUTCOM','XX') order by time_stamp",cm)
 d<-tryCatch(sqlQuery(con,q),error=function(e)NULL)
 if(!is.data.frame(d)||!nrow(d)){out[[cm]]<-data.frame(Contract=cm,Rows=0);next}
 d$time_stamp<-as.Date(d$time_stamp);d$expiry<-as.Date(d$expiry);d<-d[order(d$time_stamp,d$expiry),];fr<-d[d$expiry_series==0 & is.finite(d$px_close)&d$px_close>0,]
 fr<-fr[!duplicated(fr$time_stamp,fromLast=TRUE),];r<-fr$px_close/c(NA,head(fr$px_close,-1))-1;zr<-is.finite(r) & r==0;rr<-rle(zr);max_stale<-if(any(rr$values))max(rr$lengths[rr$values]) else 0
 out[[cm]]<-data.frame(Contract=cm,Rows=nrow(d),FrontRows=nrow(fr),First=min(fr$time_stamp),Last=max(fr$time_stamp),PreObs=sum(fr$time_stamp<=as.Date("2019-12-31")),PostObs=sum(fr$time_stamp>=as.Date("2020-05-01")),ZeroReturnDays=sum(zr,na.rm=TRUE),MaxStaleRun=max_stale,AbsRetOver15Pct=sum(abs(r)>0.15,na.rm=TRUE),MaxAbsRet=max(abs(r),na.rm=TRUE),stringsAsFactors=FALSE)
}
odbcClose(con);screen<-do.call(rbind,out);write.csv(screen,file.path(OUT,"screening.csv"),row.names=FALSE);cat(sprintf("Screened %d contracts; wrote screening.csv\n",nrow(screen)));print(screen)
