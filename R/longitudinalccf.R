#' longitudinalccf
#'
#' A conceptual extension to the ccf() function. Computes corrleation coeffeicients for various leads and lags between an x and y variable, for longitudinal data were observations are nested within an 'id' variable.
#'
#' @param n Number of lead/lags to compute up to
#' @param y Y variable
#' @param x X variable
#' @param id id variable, unique across pts.
#' @param data data.frame containing all data
#' @param outputdata If TRUE, a data.frame containing all lead and lag variables appended to the right
#' @param singlecor If TRUE, will output the lead/lag correlations for the specified n separately
#' @param plot If TRUE, a plot of all correlations will be drawn
#' @param onlylead If TRUE, only leads will be estimated, no lags
#' @param main Main title for plot, if plot==TRUE
#' @param sub Sub title for plot, if plot==TRUE
#' @param xlab X-axis label for plot, if plot==TRUE
#' @param ylab Y-axis label for plot, if plot==TRUE
#' @param yscale Y scale for plot, if plot==TRUE
#'
#' @return data.frame with new data and/or a plot of correllation coefficients.
#' @export
#'
#' @examples
#' #get<-function(){
#' #id<-sort(rep(1:200, 50))
#' #time<-rep(1:50,200)
#' #main<-(rnorm(10000, 7.4,2.2))
#' #effect<-time*.85
#' #y<-main + effect
#' #return(data.frame(id,time,y))
#' #}
#' #df <- get()
#' #longitudinalccf(n = 15,
#' #                y = y,
#' #                 x = time,
#' #                 id = id,
#' #                 data = df,
#' #                 plot=TRUE,
#' #                 outputdata=TRUE,
#' #                 singlecor=FALSE,
#' #                 onlylead=FALSE)
longitudinalccf<-function(n,y,x,id,data,
               outputdata=FALSE,
               singlecor=FALSE,
               plot=TRUE,
               onlylead=FALSE,
               main=NULL,
               sub=NULL,
               xlab=NULL,
               ylab=NULL,
               yscale=FALSE){
  if(is.null(main)) {
    main = paste("Corrlations between", deparse(substitute(x)), "&", deparse(substitute(y)))
  }
   if(is.null(sub)) {
    sub = paste("This includes a range of", n, "leads and lags")
   }
   if(is.null(xlab)) {
    xlab = "Number of Leads or Lags"
   }
   if(is.null(ylab)) {
    ylab = "Correlation Coefficient"
  }
  fullcor=TRUE
  numof<-1:n
single<-function(n,y,x,id,data){
attach(data)
xvar<-x


move<-n
leadstart<-(1+move)

leadery<-y[leadstart:length(y)]
leaderid<-id[leadstart:length(id)]

lagery<-y[1:(length(y)-move)]
lagerid<-id[1:(length(id)-move)]

fill <- rep(NA, move)

ylead<-c(leadery,fill)
idlead<-c(leaderid,fill)

ylag<-c(fill,lagery)
idlag<-c(fill,lagerid)

datas<-data.frame(y,x,id,ylead,idlead,ylag,idlag)

detach(data)

datas$ylead[datas$idlead != datas$id]<-NA
datas$ylag[datas$idlag != datas$id]<-NA
clean<-data.frame(id,x,y,datas$ylead,datas$ylag)
colnames(clean)[which(colnames(clean) == 'datas.ylead')] <- paste("lead",n)
colnames(clean)[which(colnames(clean) == 'datas.ylag')] <- paste("lag",n)

cleaner<-clean


NormCorr<-cor(cleaner[2],cleaner[3],use="pairwise.complete.obs")
LagCorr<-cor(cleaner[2],cleaner[5],use="pairwise.complete.obs")
LeadCorr<-cor(cleaner[2],cleaner[4],use="pairwise.complete.obs")
Leadresult<-c(n,LeadCorr)
Lagresult<-c(-n,LagCorr)
result<-rbind(Lagresult,Leadresult)
return(result)

}
newfunc<-function(n){
single(n,y,x,id,data)
}
if (singlecor==TRUE) singleCors<<-newfunc(n)
if (fullcor==TRUE){
exactresult <- do.call(rbind,lapply(numof,newfunc))
zerocor<-c(0,cor(x,y,use="pairwise.complete.obs"))
fullresults<<-rbind(zerocor,exactresult)
if (onlylead==TRUE) {
fullresults<<-subset(fullresults, fullresults[, 1] > -1)}

}
if (fullcor!=TRUE){
  exactresult <- do.call(rbind,lapply(numof,newfunc))
  zerocor<-c(0,cor(x,y,use="pairwise.complete.obs"))
  fullresults<-rbind(zerocor,exactresult)
  if (onlylead==TRUE) {
    fullresults<-subset(fullresults, fullresults[, 1] > -1)}
  return(fullresults)
}
if (plot==TRUE & yscale!=FALSE){
plot(fullresults[,1], fullresults[,2], type="h",
     main=main,
     sub=sub,
     xlab=xlab,
     ylab=ylab,
     ylim=yscale
     )
}
  else if  (plot==TRUE & yscale==FALSE){
    plot(fullresults[,1], fullresults[,2], type="h",
         main=main,
         sub=sub,
         xlab=xlab,
         ylab=ylab)
}

if (outputdata==TRUE) {
output<-function(n,y,x,id,data){
  attach(data)
  xvar<-x


  move<-n
  leadstart<-(1+move)

  leadery<-y[leadstart:length(y)]
  leaderid<-id[leadstart:length(id)]

  lagery<-y[1:(length(y)-move)]
  lagerid<-id[1:(length(id)-move)]

  fill <- rep(NA, move)

  ylead<-c(leadery,fill)
  idlead<-c(leaderid,fill)

  ylag<-c(fill,lagery)
  idlag<-c(fill,lagerid)

  datas<-data.frame(y,x,id,ylead,idlead,ylag,idlag)

  detach(data)

  datas$ylead[datas$idlead != datas$id]<-NA
  datas$ylag[datas$idlag != datas$id]<-NA
  clean<-data.frame(id,x,y,datas$ylead,datas$ylag)
  colnames(clean)[which(colnames(clean) == 'datas.ylead')] <- paste("lead",n)
  colnames(clean)[which(colnames(clean) == 'datas.ylag')] <- paste("lag",n)

  cleaner<-clean
}
colbind<-function(n){
  output(n,y,x,id,data)
}
bigtable <- do.call(cbind,lapply(numof,colbind))
return(data.frame(id,x,y,bigtable))
}
}
