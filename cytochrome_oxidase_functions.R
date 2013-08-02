########### Functions cytochrome oxidase project #########################

#convert from gray values to optical density based on kodak scans

gray2od<-function(gray){
kodak<-read.csv("kodak_from_each_slide.csv")
correctSTD<-kodak$std.no
correctSTD[correctSTD==1]<-.05
correctSTD[correctSTD==2]<-(.05+.15)
correctSTD[correctSTD==3]<-(.05+.15+.15)
correctSTD[correctSTD==4]<-(.05+.15+.15+.15)
kodak$correctSTD<-correctSTD
kodak2<-kodak[(kodak$std.no.==1|kodak$std.no.==2|kodak$std.no.==3),]
lm2<-lm(correctSTD~gray.values,data=kodak2) # OD predicted by gray values
OD<-predict(lm2,newdata=data.frame(gray.values=gray),na.action=na.exclude)
return(OD)
}
#inherits gray2od
od2bp<-function(OD){
bp<-read.csv("brainPaste.csv")
bp$od<-gray2od(bp$gray.value)
means<-aggregate(bp$od,by=list(bp$thickness),FUN=mean)
names(means)<-c("thickness","averageOD")
lm1<-lm(thickness~averageOD,data=means)
summary(lm1)
brainPasteUnits<-predict(lm1,newdata=data.frame(averageOD=OD),na.action=na.exclude)
return(brainPasteUnits)
}

