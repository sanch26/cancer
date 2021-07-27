cancer<-read.csv(choose.files(),header = TRUE)
cancer
head(cancer)
str(cancer)
dim(cancer)
cancer<-cancer[-33]
summary(cancer)
#no.of women affected in bengnin and malingant stage
library (dplyr)
library(corrplot)
cancer%>%count(diagnosis)
#percentage of women affected in b and m stage
cancer %>%count(diagnosis)%>%group_by(diagnosis)%>%
 summarize(perc_dx= round(n/569)*100,22)
#data visualization
#frequency of cancer  diagnosis
diagnosis.table <-(cancer$diagnosis)
color<-terrain.colors(2)


#create pie chart
diagnosis.prop.table<-prop.table(diagnosis.table)*100
diagnosis.prop.df<-as.data.frame(diagnosis.prop.table)
pielabels<-sprintf("%s -3.1%s",diagnosis.prop.df[,1],diagnosis.prop.table,"%")
pie(diagnosis.prop.table,labels=pie,cloclwise= TRUE,col=colors,
    border="gainsboro", radius = 0.8,cex=0.8,main = "frequency of cancer")
legend(1,.4,legend = diagnosis.prop.df[,1],cex=0.7,fill=colors)


#corelation plot
#calculate collinerairty
c<-cor(cancer[,3:31])
corrplot(c,order="hclust",tl.cex = 0.7)
# comparing the radius,concavity of b and m stage
ggplot(cancer,aes(x=diagnosis,y=radius_mean,fill="pink"))+geom_boxplot(fill="yellow")+ggtitle("radius_mean of bengnin")
ggplot(cancer, aes(x=diagnosis,y=radius_mean,fill="green"))





#barplot
ggplot(cancer,aes(x=diagnosis,fill=texture_mean,fill="yellow"))+geom_bar()+ggtitle("women affected")

ggplot(cancer,aes(x=area_se>15,fill=diagnosis))+geom_bar(position="fill")+ggtitle("area")



#histogram
ggplot(cancer,aes(x=concavity_mean,fill=diagnosis))+geom_histogram(binwidth = 5)+ggtitle("concavity_mean")


