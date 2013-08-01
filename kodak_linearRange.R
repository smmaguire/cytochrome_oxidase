#klr<-read.csv("kodak_linearRange.csv")
#std<-c(.05,.2,.35)
#plot(std,klr$mean)
#klrlm<-(lm(klr$mean~std))

#Going to use the measurments from all the slides has better R-squared
kodak<-read.csv("kodak_from_each_slide.csv")
correctSTD<-kodak$std.no
correctSTD[correctSTD==1]<-.05
correctSTD[correctSTD==2]<-(.05+.15)
correctSTD[correctSTD==3]<-(.05+.15+.15)
correctSTD[correctSTD==4]<-(.05+.15+.15+.15)
kodak$correctSTD<-correctSTD
kodak2<-kodak[(kodak$std.no.==1|kodak$std.no.==2|kodak$std.no.==3),]



lm2<-lm(gray.values~correctSTD,data=kodak2)

plot(gray.values~correctSTD,data=kodak2)

CI<-predict(lm2,newdata=data.frame(correctSTD=seq(from=.05,to=.35,by=.1)),interval="confidence")
CI<-as.data.frame(CI)
CI$newdata<-seq(from=.05,to=.35,by=.1)
lines(CI$newdata,CI$lwr,lty=3)
lines(CI$newdata,CI$upr,lty=3)
abline(lm2)