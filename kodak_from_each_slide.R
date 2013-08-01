#read in csv
kodak<-read.csv("kodak_from_each_slide.csv")
kodak<-kodak[!is.na(kodak$slide.no),]

#we want to calculate the variance of these different measurments
cvSave<-vector()
for(std in 0:4){
newDataSet<-kodak[(kodak$std.no.==std),]
stdev<-sd(newDataSet$gray.values)
meanK<-mean(newDataSet$gray.values)
cv<-(stdev/meanK)
cvSave<-c(cvSave,cv*100)
}

correctSTD<-kodak$std.no
correctSTD[correctSTD==1]<-.05
correctSTD[correctSTD==2]<-(.05+.15)
correctSTD[correctSTD==3]<-(.05+.15+.15)
correctSTD[correctSTD==4]<-(.05+.15+.15+.15)
kodak2<-kodak
kodak2[kodak2$std.no.==0,3]<-0

boxplot(gray.values~correctSTD,data=kodak)
plot(gray.values~correctSTD,data=kodak2)


myFirstLM<-lm(gray.values~std.no.,data=kodak)

kodak[(kodak$std.no.==0|kodak$std.no.==1|kodak$std.no.==2|kodak$std.no.==3),]
kodak[(kodak$std.no.!=4),]
xx<-which(kodak$std.no.==4)
kodak[-xx,]
kodak2<-kodak
kodak2$correctSTD<-correctSTD
kodak2<-kodak2[kodak2$correctSTD!=.5,]
kodak2<-kodak2[kodak2$correctSTD!=0,]

lm2<-lm(gray.values~correctSTD,data=kodak2)
kodak[(kodak$std.no.!=4),]

pdf(file="scan_variation.pdf",height=7,width=10)
plot(gray.values~std.no.,data=kodak[(kodak$std.no.!=4),],xlab="kodak standard",ylab="gray level",pch=16,col="#2CA25F15",main="variation across 45 independent scans")
abline(lm2,col="blue",lwd=2)
text(2.3,15000,paste("R-square",round(summary(lm2)$adj.r.squared,3),sep="="))
text(2.3,13500,paste("average CV",round(mean(cvSave[1:4]),3),sep="="))
dev.off()
ls()




