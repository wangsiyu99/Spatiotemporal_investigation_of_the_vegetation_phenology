#2012_SOS
onedata<-read.csv("E:/毕业论文/结题/处理后_数据/距离梯度/2012_最终版/distance.csv")
#对变量平均值进行可视化

par(family="STkaiti")
plotmeans(SOS~Distance,onedata,col="red",main="",xlab = "SOS")


#用aov()函数进行单因素方差分析
onedata_aov<-aov(SOS~Distance,onedata)
summary(onedata_aov)


#使用TukeyHSD()函数对方差分析结果进行多重比较
tky<-TukeyHSD(onedata_aov)
tky=as.data.frame(tky$Distance)
#tky<-as.data.frame(tky)
tky$pair=rownames(tky)
#TukeyHSD()检验
#par(las=1)
#plot(tky)

#TukeyHSD方差结果多重比较

ggplot(tky,aes())+
  theme_bw(base_family = "STKaiti",base_size = 16)+geom_hline(yintercept=0,lty="11",colour="grey30",size=1)+geom_errorbar(aes(pair,ymin=lwr,ymax=upr),width=0.2,size=1)+
  geom_point(aes(pair,diff),size=2)+labs(colour=" ",x="SOS")+theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5,size=5))
