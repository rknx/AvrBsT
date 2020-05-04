a <- c("gene", "dpi", "dis", "rain", "wind", "temp", "rh", "dew")

vec <- c(F, F, F, T, T, T, T, T, T, T)

terms <- a[vec]

combn(a, 2)

comblen <- seq_len(length(terms))

comb <- unlist(lapply(comblen, function(x) combn(comblen, x, simplify = F)),
    recursive = F
)

formulae <- sapply(comb, function(x) {
    paste("y ~", paste(terms[x], collapse = "+"))
})

for (i in seq_len(length(formulae))) {
    fit <- glm(formulae[i],
        data = data,
        family = binomial(link = "logit")
    )
}

library(ggplot2)

weather_2016$row <- seq_len(nrow(weather_2016))

ggplot() +
    geom_point(aes(x = row, y = Temp.2m), data = weather_2016[weather_2016$Month == "May" & weather_2016$Day == "5", ], color = "red") +
    geom_point(aes(x = row, y = Temp.2m), data = weather_2016[weather_2016$Month == "May" & weather_2016$Day == "6", ], color = "blue") +
    geom_point(aes(x = row, y = Temp.2m), data = weather_2016[weather_2016$Month == "May" & weather_2016$Day == "7", ], color = "green") +
    geom_point(aes(x = row, y = Temp.2m), data = weather_2016[weather_2016$Month == "May" & weather_2016$Day == "8", ], color = "black")