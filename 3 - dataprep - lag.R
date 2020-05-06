# Set up environment

## Libraries
if (!require(lme4)) install.packages("lme4")
library(lme4)
if (!require(ggplot)) install.packages("ggplot")
library(ggplot)

## Set the working directory
setwd(rprojroot::find_rstudio_root_file())




# Import data

## Load saved R objects
load(paste0(getwd(), "/Data/field.rda"))
load(paste0(getwd(), "/Data/weather.rda"))




# Some functions

# Point biserial correlation function (github: cran/ltm -> /R/biserial.cor.R)
biserial_cor <- function (x, y) {
	na.row <- complete.cases(x, y) #!imp make na index prior to applying
	x <- x[na.row]
	y <- y[na.row]
	Δμ <- mean(x[y == 1]) - mean(x[y == 0])
	ℙ <- mean(y == 1)
	σ <- sd(x) * sqrt((length(x) - 1) / length(x))
	Δμ * sqrt(ℙ * (1 - ℙ)) / σ
}

## Regression function 
my_fit_test (col1, col2) {
	print cbind(col1, col2)
}

## Clustering function for weather predictors
my_clust <- function(data, k) {
	cor_mat <- cor(data, use="complete.obs")
	groups <- cutree(hclust(dist(cor_mat)), k = k)
	lapply(unique(groups), function(x) names(groups)[groups == x])
}


# Correlation between presence and weather predictors
cor_values <- do.call(rbind, lapply(env, function(x){
	data <- merge(field, x, by = "date")
	out <- do.call(data.frame, lapply(names(x)[-1], function(y){
		biserial_cor( data[[y]], data$presence)
	}))
	names(out) <- names(x)[-1]
	round(out,2)
	# my_fit_test(data)
}))


## Finding best lag period
best_lag <- function(cor_data, k = F) {
	if (k) cor_data <- cor_data[,sapply(my_clust(weather[-1], 5), '[[', 1)]
	which.max(rowSums(abs(cor_data))) - 1
}

lapply(3:10, function(x) best_lag(cor_values, 6))

# Groups of 












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