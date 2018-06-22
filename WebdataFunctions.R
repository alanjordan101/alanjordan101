# Define Functions
CountBox1<-function(varr) {
  counts <- table(varr)  
  percc1 <- counts[1]/sum(counts)
  percc2 <- counts[2]/sum(counts)
  percc2a <-paste(formatC(percc2*100,format="f",digits=1),"%",sep="")
  percc1a <-paste(formatC(percc1*100,format="f",digits=1),"%",sep="")
  box<-data.frame(t(rbind(format(counts[2],big.mark=","),percc2a, format(counts[1], big.mark=","), percc1a)))
  colnames(box)<-c(  "Male \nCounts", "Male \nPercent", "Female \nCounts", "Female \nPercent")
  rownames(box)<-NULL
  out<-list(box, counts)	
  return(out)
}




CountBox2<-function(varr,vn,adult=1) {
  if (adult==1) {	counts <- table(set$sex,varr) } else {	counts <- table(adol$sex,varr) }
  labs<-colnames(counts)
  if(sum(counts[1,],na.rm=TRUE)==0){percc1 <- counts[1,]} else {percc1 <- counts[1,]/sum(counts[1,])}
  if(sum(counts[2,],na.rm=TRUE)==0){percc2 <- counts[2,]} else {percc2 <- counts[2,]/sum(counts[2,])}
  percc2a <-paste(formatC(percc2*100,format="f", digits=1),"%",sep="")
  percc1a <-paste(formatC(percc1*100,format="f", digits=1),"%",sep="")
  counts2<-format(counts, big.mark=",")
  box<-data.frame(t(rbind(labs,counts2[2,],percc2a, counts2[1,], percc1a)))
  colnames(box)<-c( vn, "Male \nCounts", "Male \nPercent", "Female \nCounts", "Female \nPercent")
  rownames(box)<-NULL
  out<-list(box, counts, counts2)	
  return(out)
}




PercBox1<-function(vars, labs, data=set) {
  msum<-apply(subset(data,sex2==1)[,vars],2,FUN=sum, na.rm=TRUE)
  if(sum(msum)==0) {mmean=msum} else {  mmean<-apply(subset(data,sex2==1)[,vars],2,FUN=mean, na.rm=TRUE)}
  fsum<-apply(subset(data,sex2==2)[,vars],2,FUN=sum, na.rm=TRUE)
  if(sum(fsum)==0) {fmean=fsum} else {  fmean<-apply(subset(data,sex2==2)[,vars],2,FUN=mean, na.rm=TRUE)}
  bt<-cbind(mmean,fmean)*100
  msum<-formatC(msum, format='f', digits=0, big.mark=",")
  fsum<-formatC(fsum, format='f', digits=0, big.mark=",")
  
  bt1<-paste(round(mmean*100,1),"%", sep="")
  bt2<-paste(round(fmean*100,1),"%", sep="")
  bt3<-cbind(bt1,bt2)
  box<-cbind(labs,format(msum,big.mark=","),bt1,format(fsum,big.mark=","),bt2)
  colnames(box)<-c( "Behavior", "Male Counts", "Male Percent", "Female Counts", "Female Percent")
  rownames(box)<-NULL
  
  if ( table(data$sex)[1]==0 & table(data$sex)[2] >0 ) { 
    box<- box[,c(1,2,3)]
    bt3<- bt3[,1]
    bt<- bt[,1]
  }
  
  if ( table(data$sex)[1]>0 & table(data$sex)[2] ==0 ) {  
    box<- box[,c(1,4,5)]
    bt3<- bt3[,2]
    bt<- bt[,2]
  }
  
  out<-list(box, bt,bt3)	
  return(out)
}


MeanBox1<-function(vars, labs, data=setcsa) {
  msum<-apply(subset(data,sex2==1)[,vars],2,FUN=sum, na.rm=TRUE)
  fsum<-apply(subset(data,sex2==2)[,vars],2,FUN=sum, na.rm=TRUE)
  if(sum(msum)==0) {mmean=msum} else {  mmean<-apply(subset(data,sex2==1)[,vars],2,FUN=mean, na.rm=TRUE)}
  if(sum(fsum)==0) {fmean=fsum} else {  fmean<-apply(subset(data,sex2==2)[,vars],2,FUN=mean, na.rm=TRUE)}
  
  if(sum(msum)==0) {mmedian=msum} else {  mmedian<-apply(subset(data,sex2==1)[,vars],2,FUN=median, na.rm=TRUE)}
  if(sum(fsum)==0) {fmedian=fsum} else {  fmedian<-apply(subset(data,sex2==2)[,vars],2,FUN=median, na.rm=TRUE)}
  
  b1<-cbind(mmean,fmean)
  
  mmean<-formatC(mmean,format="f",digits=1)
  mmedian<-formatC(mmedian,format="f",digits=1)
  fmean<-formatC(fmean,format="f",digits=1)
  fmedian<-formatC(fmedian,format="f",digits=1)
  
  box<-cbind(labs,mmean,mmedian,fmean,fmedian)
  rownames(box)<-NULL
  colnames(box)<-c("Variable","Male Mean", "Male Median", "Female Mean", "Female Median")
  
  if ( table(data$sex)[1]==0 & table(data$sex)[2] >0 ) { 
    box<- box[,c(1,2,3)]
    b1<- b1[,1]
  }
  
  if ( table(data$sex)[1]>0 & table(data$sex)[2] ==0 ) {  
    box<- box[,c(1,4,5)]
    b1<- b1[,2]
  }
  
  out<-list(box,b1)
  return(out)
}



PercPlot<-function(data=out2,cex=1,cex.names=1 ) {
  b=barplot(t(data[[2]]),beside=TRUE, legend =c("Male", "Female"), names.arg=labs,
            las=1,col=c("#007FFF", "Navyblue"), ylim=c(0,max(data[[2]])*1.5 ), cex.names=cex.names , yaxt="n")
  my.axis <-paste(axTicks(2),"%")
  axis(2,at=axTicks(2), labels=my.axis, las=1)
  text(x=b,y=t(data[[2]]),labels=t(data[[3]]) ,pos=3,col="black",cex=cex) 
}




CountPlot<-function(data=out2, cex=1, cex.names=1  ) {
  par(mfrow=c(2,1))
  b<-barplot(data[[2]][2,], main="Male",  names.arg=data[[1]][,1], cex.names=cex.names,  
             col="Navyblue", ylim=c(0,max(data[[2]][2,])*1.5 ), las=1)
  text(x=b,y=data[[2]][2,],labels=data[[1]][,3] ,pos=3,col="black",cex=cex) 
  b2<-barplot(data[[2]][1,], main="Female",  names.arg=data[[1]][,1], cex.names=cex.names,  
              col="#007FFF", ylim=c(0,max(data[[2]][1,])*1.5 ), las=1)
  text(x=b2,y=data[[2]][1,],labels=data[[1]][,5] ,pos=3,col="black",cex=cex.names) 
}


MeanPlot<-function(data=out2,cex=1,cex.names=1) {
  b1<-out2[[2]]
  b<-barplot(t(b1),beside=TRUE, horiz=FALSE, legend =c("Male", "Female"), names.arg=labs,  
             las=1,col=c("#007FFF", "Navyblue"), ylim=c(0,max(b1)*1.5 ), 
             cex.names=cex.names,   xlab="Mean" )
  text(x=b,y=as.matrix(t(b1)),labels=t(formatC(b1,format="f",digits=1)) ,col="black",
       cex=cex, pos=3) 
}



# Stop Scientific Notation
options("scipen" = 20)


ggpie <- function(df, plot_title, by = "cat",  totals = "res") {
  
  #  display categories from largest to smallest
  #    original df not mutated - elements added to plot outside ggpie
  #    will likely have a different order.
  df <- arrange(df, desc(df[[totals]]))
  
  # get labels and where they go
  labels <- paste0(df[[by]], " ", round(df[[totals]] / sum(df[[totals]]) * 1000)/10,  "%")
  at <- cumsum(df[[totals]]) - df[[totals]] / 2
  
  ggplot(df, aes_string(x = 1, y = totals, fill = by)) +
    
    # make stacked bar chart with black border
    geom_bar(stat = 'identity', color = 'black') +
    
    # x-value moves percents closer/farther from center. 0.7-1.3 reasonable.
    annotate(geom = "text", y = at, x = 1.65, label = labels, size = 5) +
    
    # convert to pie chart by using polar coordinates.
    #   comment out for stacked bar chart.
    coord_polar(theta = 'y', start=0) +
    
    # add a title. makes repeated code cleaner.
    labs(title = plot_title) +
    
    # kill the legend and floating labels for axes. make text big.
    theme_minimal()+
    theme(legend.position = "none", 
          text = element_text(size = 22),
          axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          panel.border = element_blank(),
          panel.grid=element_blank(),
          axis.title = element_blank()
    ) +
    
    # label the categories
    scale_y_continuous(breaks = at, labels = NULL) +
    
    # Manual slice coloring...often a bad idea
    scale_fill_manual(values=c("#007FFF", "Navyblue")) +
    
    # remove black border from legend for when it's added back in
    guides(fill = guide_legend(override.aes = list(color = NA)))
  
}

source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")

#tabb<-function(data,varr, newvn, textsize=3) {
#  df1<-tabular(  data[[varr]] ~  sex*(1+Percent("col")),  data=set) 
#  df1<-as.matrix(df1)
#  df1<-data.frame(df1[4:dim(df1)[1],])
#  names(df1)<- c( newvn, "Female Counts", "Female Percent", "Male Counts", "Male Percent")
#  df1[,2] <-as.numeric(as.character(df1[,2]))
#  df1[,3] <-as.numeric(as.character(df1[,3]))
#  df1[,4] <-as.numeric(as.character(df1[,4])) 
#  df1[,5] <-as.numeric(as.character(df1[,5])) 
#  df2<-df1
#  df2[,2] <-formatC(df1[,2], format="d", big.mark=',')
#  df2[,4] <-formatC(df1[,4], format="d", big.mark=',')
#  df2[,3] <-sprintf("%1.1f%%", df1[,3])
#  df2[,5] <-sprintf("%1.1f%%", df1[,5])
#  df2<-df2[,c(1,4,5,2,3)]
#  df3<-df1[,c(1,3,5)]
#  names(df3)<- c( newvn,  "Female", "Male")
#  df3<-melt(df3, id=newvn)
#  g1<-ggplot(df3, aes_string(x = newvn, y = "value", fill="variable")) +    geom_bar(stat = "identity", position="dodge") +
#    scale_fill_manual(values=c("lightslateblue", "Navyblue"), name="Gender") +  ylab("Percent")  + theme_minimal() +  
#    geom_text(aes(y=value, ymax=value, label=paste0(round(value,1),"%"  )), position= position_dodge(width=0.9), vjust=-.5, color="black", size = textsize) +
#    scale_y_continuous("Percent",limits=c(0,max(df3$value)*1.1))
#  dfs<-list(df1, df2, df3, g1)
#  return(dfs)
#}



tabb<-function(data,varr, newvn, textsize=3) {
  #  set$xx<-set$acat
  #  newvn="Age"
  #  data<-set
  
  df1 <-data %>% group_by_("sex",varr) %>% summarize(n=n())
  colnames(df1)[2]<-'xx'
  df1<- filter(df1, !is.na(xx))
  
  df2 <-data %>% group_by(sex) %>% summarize(ns=n())
  df1<-merge(df1, df2, by='sex')
  df1$perc <- df1$n/ df1$ns*100
  df1$perclab<-sprintf("%1.1f%%", round(df1$perc,1))
  
  df3<-dcast(df1, xx~sex, value.var="perc")
  colnames(df3)[2:ncol(df3)] <- paste0(colnames(df3)[2:ncol(df3)], "_perc")
  
  df4<-dcast(df1, xx~sex, value.var="n")
  colnames(df4)[2:ncol(df4)] <- paste0(colnames(df3)[2:ncol(df4)], "_n")
  
  df5<-merge(df4,df3, by=names(df3)[1], sort=F)
  
  if ( ncol(df5)==5 ) {  df5<-df5[c(1, 3, 5, 2, 4)] }
  df5[,2] <- ifelse(is.na(df5[,2]), 0, df5[,2])
  df5[,3] <- ifelse(is.na(df5[,3]), 0, df5[,3])
  if ( ncol(df5)==5 ) {
    df5[,4] <- ifelse(is.na(df5[,4]), 0, df5[,4])
    df5[,5] <- ifelse(is.na(df5[,5]), 0, df5[,5]) 
  }
  
  
  
  df6<-df5
  df6[,2]<-formatC(df6[,2], format="d", big.mark=',')
  df6[,3]<-sprintf("%1.1f%%", df6[,3])
  if ( ncol(df6)==5 ) {
    df6[,4]<-formatC(df6[,4], format="d", big.mark=',')
    df6[,5]<-sprintf("%1.1f%%", df6[,5])
  }
  
  if ( table(data$sex)[1]==0 & table(data$sex)[2] >0 ) {names(df6)<- c( newvn, "Male Counts", "Male Percent")}
  if ( table(data$sex)[1]>0 & table(data$sex)[2] ==0 ) {names(df6)<- c( newvn, "Female Counts", "Female Percent")}
  if ( table(data$sex)[1]>0 & table(data$sex)[2] >0 )  {names(df6)<- c( newvn, "Male Counts", "Male Percent", "Female Counts", "Female Percent")}
  
 # ymax=perc,
  g1<-ggplot(df1, aes(x = xx, y = perc, fill=sex)) +    geom_bar(stat = "identity", position="dodge") +
    scale_fill_manual(values=c("#007FFF", "Navyblue"), name="Gender") +  ylab("Percent") +  xlab(newvn) + theme_minimal() +  
    geom_text(aes(y=perc,  label=perclab), position= position_dodge(width=0.9), vjust=-.5, color="black", size = textsize) +
    scale_y_continuous("Percent",limits=c(0,max(df1$perc)*1.1))
  
  colnames(df1)[2] <- newvn
  
  dfs<-list(df1, df6, g1)
  return(dfs)
}