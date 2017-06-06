library(Quandl)
#download data and transform into time series
vix_m=Quandl('CBOE/VXMT')
vix_m=as.xts(vix_m$Close,order.by = vix_m$Date)
vix_s=Quandl('CBOE/VXST')
vix_s=as.xts(vix_s$Close,order.by = vix_s$Date)
spy=getSymbols('SPY')
spy=SPY$SPY.Close


#merge data, find diff and neg factor
data=merge.xts(spy,vix_s,join = 'inner')
data=merge.xts(data,vix_m,join = 'inner')
data$diffvix=data$vix_m-data$vix_s
data$neg=ifelse(data$diffvix<0,1,0)
colnames(data)=c('indice','short','medium','diff','neg')
View(data)

#single plot
ggplot(data=data,aes(x=index(data)))+
  geom_line(data = data,aes(y=indice/2,colour=neg))+
  geom_line(data = data,aes(y=short),color='red')+
  geom_line(data = data,aes(y=medium),color='blue')

#multiplot
library(Rmisc)
vix_plot=ggplot(data=data,aes(x=index(data)))+
  geom_line(data = data,aes(y=short),color='red')+
  geom_line(data = data,aes(y=medium),color='blue')
indice_plot=ggplot(data=data,aes(x=index(data)))+
  geom_line(data = data,aes(y=indice/2,colour=neg))+
  theme(legend.position = 'none')
multiplot(indice_plot,vix_plot,cols = 1)

#multiplot: indice, vix short/medium, vix short/medium smoothed
xx=ggplot(data=data,aes(x=index(data)))+
  geom_smooth(data = data,aes(y=short),color='red')+
  geom_smooth(data = data,aes(y=medium),color='blue')
multiplot(indice_plot,vix_plot,xx,cols = 1)

#data table with indice repeated for open, high, low in Visual Trader
#this is for personal use in Visula Trader (a software I am using)
#for this reason the following section is not relevant
write.zoo(data,file ='spy_vixshort_vixlong.csv' ) #write as zoo to obtain the date as index (check directory)
data3=merge.xts(data$indice, data$diff)
data3$indice2=data$indice
data3$indice3=data$indice
data3$indice4=data$indice
View(data3)
write.zoo(data3,file='prova_spy_repeated.csv')
