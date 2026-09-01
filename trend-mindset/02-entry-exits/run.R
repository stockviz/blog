#!/usr/bin/env Rscript
# Controlled rule-entry versus random-entry test with identical exits.
options(stringsAsFactors=FALSE, scipen=100)
args <- commandArgs(FALSE); hit <- grep("^--file=", args, value=TRUE); OUT <- if(length(hit)) dirname(normalizePath(sub("^--file=","",hit),mustWork=FALSE)) else getwd()
DRAG <- 0.0025; PRE_END <- as.Date("2019-12-31"); POST_START <- as.Date("2020-05-01")
metrics <- function(r) { r<-r[is.finite(r)]; if(length(r)<2) return(c(N=length(r),CAGR=NA,Sharpe=NA,MaxDD=NA,Turnover=NA)); eq<-cumprod(1+r); dd<-eq/cummax(eq)-1; c(N=length(r),CAGR=eq[length(eq)]^(252/length(r))-1,Sharpe=mean(r)/sd(r)*sqrt(252),MaxDD=-min(dd),Turnover=NA) }
run <- function(path, instrument, seed) {
 x<-read.csv(path); d<-as.Date(x$date); p<-as.numeric(x$price); ok<-is.finite(p)&p>0; d<-d[ok];p<-p[ok]; r<-c(NA,p[-1]/p[-length(p)]-1); n<-length(p)
 ema<-rep(NA,n); for(i in 50:n) ema[i]<-mean(p[(i-49):i]); breakout<-rep(NA,n); for(i in 21:n) breakout[i]<-p[i]>max(p[(i-20):(i-1)])
 set.seed(seed); random_entry<-rbinom(n,1,0.5); signals<-list(EMA=as.numeric(p>ema),Breakout=as.numeric(breakout),Random=random_entry)
 ans<-list()
 for(nm in names(signals)) { desired<-signals[[nm]]; desired[!is.finite(desired)]<-0; pos<-c(0,desired[-n]); # trailing exit: same 3% stop from entry peak
 peak<-p[1]; active<-0; held<-numeric(n); for(i in seq_len(n)){ if(pos[i]>0){peak<-max(peak,p[i]); if(p[i]<peak*0.97) active<-0} else active<-0; if(active==0 && pos[i]>0) active<-1; held[i]<-active }
 tr<-abs(held-c(0,held[-n])); net<-held*r-DRAG*tr; net[!is.finite(net)]<-0
 for(w in c("pre","post","full")){keep<-if(w=="pre")d<=PRE_END else if(w=="post")d>=POST_START else rep(TRUE,n); m<-metrics(net[keep]);m["Turnover"]<-mean(tr[keep],na.rm=TRUE);ans[[length(ans)+1]]<-data.frame(Window=w,Instrument=instrument,Seed=seed,Rule=nm,t(m))}
 }
 do.call(rbind,ans)
}
res<-list(); for(nm in c("NIFTY","SELECT")) for(seed in c(11L,29L,71L,101L,211L)){f<-if(nm=="NIFTY")"/mnt/data/blog/turbulence/simple/daily_NIFTY.csv" else "/mnt/data/blog/turbulence/simple/daily_SELECT.csv";res[[length(res)+1]]<-run(f,nm,seed)}
out<-do.call(rbind,res);write.csv(out,file.path(OUT,"entry_metrics.csv"),row.names=FALSE);write.csv(aggregate(cbind(CAGR,Sharpe,MaxDD,Turnover)~Instrument+Rule+Window,out,mean),file.path(OUT,"entry_summary.csv"),row.names=FALSE);cat(sprintf("Wrote %d rows across %d seeds; drag=%.4f\n",nrow(out),5,DRAG))
