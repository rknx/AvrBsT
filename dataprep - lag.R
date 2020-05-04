
# env and field_merge



lapply(env, function(x){
  data
})



### Finding the best lag period 
```{r}
source("lagplot.R")
lagplot(fd=data.frame(date=7*dm$week+doi[dm$year],rating=dm$  rating),
        wd=data.frame(date=c(outer(as.numeric(rownames(w)),doi[names(doi)],`+`)),var=unlist(w[,names(w)%in%"rain"])),
        labs=c("Rating","Rain 'in'",dir="h")
)
```
### Lag period by correlation
```{r}
cor=data.frame(rain=0,temp=0,RH=0,wind=0)
factor=c(1,-1,1,-1)
for (h in 1:length(cor)){
 for(i in 1:dim(env)[3]) {
  data=merge(dm[!is.na(dm$sev),],env[,,i])
  cor[i,h]=cor(data$sev,data[[names(cor)[h]]])
 }
  print(paste0(names(cor)[h],": ",
               as.numeric(rownames(cor)[which.max(factor[h]*cor[,h])])-1
               ," (",round(max(factor[h]*cor[,h]),3),")")
  )
}
cat("Highest correlation is with", which.max(rowSums(abs(cor)))-1,"days lag.\n")
```
### Verify lag by fitting
```{r message=F, warning=F}
fitpar=data.frame(lag=0,aic=0,bic=0,logLik=0)
for (i in 1:dim(env)[3]) {
  fit=glmer(sev~rain+temp+wind+RH+(1|ryd/pid),data=merge(dm,env[,,i]),family=Gamma(link=log))
  fitpar[i,]=data.frame(i-1,AIC(fit),BIC(fit),logLik(fit))
}
cat(paste0("Best lag period is ", (fitpar$lag[which.min(fitpar$bic)])," days (BIC = ",round(min(fitpar$bic),2),").\n"))
ggplot(fitpar,aes(lag,bic,label=round(bic)))+
  geom_smooth(method="loess",span=0.3,col="grey40",alpha=0.4)+
  geom_hline(aes(yintercept=min(fitpar$bic)),linetype="dashed")+
  geom_point()+geom_text()
```
### Select best environment
```{r echo = T, results = 'hide'}
env_m=data.frame(env[,,4]) #which.min(fitpar$bic)
#env_r=with(env_m,data.frame(year,week,temp=round(temp),RH=round(RH),wind=round(wind,1),rain=round(rain,1)))
```