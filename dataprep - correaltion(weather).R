# Load library
if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)
if (!require(ggfortify)) install.packages("ggfortify")
library(ggfortify)

# correaltion bwtween factors

combination <- combn(names(weather)[-1], 2, simplify = F)

cor_table <- do.call(rbind, lapply(combination, function(x) {
    data.frame(
        factor1 = x[1], 
        factor2 = x[2], 
        correlation = cor(weather[x[1]], weather[x[2]], use="complete.obs")[1]
    )
}))
cor_table[order(-abs(cor_table$correlation)),]

# Manually group based on correlation

group1 = names(weather)[lapply(strsplit(names(weather),'\\.'), "[[", 1) %in% c("Radiation")]
group2 = names(weather)[lapply(strsplit(names(weather),'\\.'), "[[", 1) %in% c("Rain")]
group3 = names(weather)[lapply(strsplit(names(weather),'\\.'), "[[", 1) %in% c("Humidity")]
group4 = names(weather)[lapply(strsplit(names(weather),'\\.'), "[[", 1) %in% c("Wind")]
group5 = names(weather)[lapply(strsplit(names(weather),'\\.'), "[[", 1) %in% c("Temp", "Dew")]
