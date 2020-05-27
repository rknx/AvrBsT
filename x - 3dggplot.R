# Set up environment -----------------------------------------------------------

## Basic project functions
source("0 - prerequisites.R")

## Libraries
"ggplot2" %>=>% lib_install %!=>% library(.., char = T)
"reshape2" %>=>% lib_install %!=>% library(.., char = T)
"plotly" %>=>% lib_install %!=>% library(.., char = T)
"rayshader" %>=>% lib_install %!=>% library(.., char = T)

## Set the working directory
setwd(rprojroot::find_rstudio_root_file())


# dataset ----------------------------------------------------------------------

mat <- matrix(c(
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 2, 5, 7, 8, 8, 7, 5, 2, 0,
    0, 5, 8, 9, 10, 10, 9, 8, 5, 0,
    2, 7, 9, 10, 10, 10, 10, 9, 7, 2,
    2, 7, 9, 10, 10, 10, 10, 9, 7, 2,
    0, 5, 8, 9, 10, 10, 9, 8, 5, 0,
    0, 2, 5, 7, 8, 8, 7, 5, 2, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0
), ncol = 10)

df <- melt(mat, var = c("x", "y"), value.name = "height")
df

plot1 <- ggplot(df, aes(x = x, y = y)) +
    geom_raster(aes(fill = height))

plot_gg(plot1,
    width = 5, height = 5, scale = 250,
    zoom = 0.7, theta = 10, phi = 30, windowsize = c(800, 800)
)





#-----------------------------------
mat_plant <- matrix(
    c(
        00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00,
        01, 01, 01, 01, 01, 01, 01, 01, 01, 01, 01, 01,
        01, 66, 72, 82, 88, 89, 89, 88, 82, 72, 66, 58,
        01, 84, 90, 94, 96, 96, 96, 96, 94, 90, 84, 66,
        01, 90, 96, 98, 99, 99, 99, 99, 98, 96, 90, 84,
        01, 92, 96, 98, 99, 99, 99, 99, 98, 96, 92, 88,
        01, 92, 96, 98, 99, 99, 99, 99, 98, 96, 92, 88,
        01, 90, 96, 98, 99, 99, 99, 99, 98, 96, 90, 84,
        01, 84, 90, 94, 96, 96, 96, 96, 94, 90, 84, 66,
        01, 66, 72, 82, 88, 89, 89, 88, 82, 72, 66, 58,
        01, 01, 01, 01, 01, 01, 01, 01, 01, 01, 01, 01,
        00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00
    ),
    nrow = 12, ncol = 12, byrow = T
)

mat_field <- matrix(
    c(
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 2, 2, 1, 1, 1, 1,
        1, 1, 1, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 2, 1, 1, 1,
        1, 1, 2, 2, 3, 3, 3, 4, 4, 4, 4, 4, 4, 3, 3, 3, 2, 2, 1, 1,
        1, 1, 2, 2, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 3, 3, 2, 2, 1, 1,
        1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 5, 5, 4, 4, 3, 3, 2, 2, 1, 1,
        1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 5, 5, 4, 4, 3, 3, 2, 2, 1, 1,
        1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 5, 5, 4, 4, 3, 3, 2, 2, 1, 1,
        1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 5, 5, 4, 4, 3, 3, 2, 2, 1, 1,
        1, 1, 2, 2, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 3, 3, 2, 2, 1, 1,
        1, 1, 2, 2, 3, 3, 3, 4, 4, 4, 4, 4, 4, 3, 3, 3, 2, 2, 1, 1,
        1, 1, 1, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 2, 1, 1, 1,
        1, 1, 1, 1, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 2, 2, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
    ),
    nrow = 20, ncol = 20, byrow = F
)


plant <- c(nrow(mat_plant), ncol(mat_plant))
field <- c(nrow(mat_field), ncol(mat_field))


p <- mat_plant[
    rep(seq_len(plant[1]), times = field[1]),
    rep(seq_len(plant[2]), times = field[2]),
    drop = F
]

sapply(mat_field, rep, plant[1]) %=>%
    matrix(.., ncol = plant[1] * field[2], byrow = T) %=>%
    sapply(.., rep, plant[2]) %=>%
    matrix(.., ncol = plant[2] * field[1], byrow = T) %->%
    q

plot_ly(
    z = ~p,
    type = "surface",
    surfacecolor = q,
    colors = c("darkgreen", "yellow"),
) %>%
    layout(scene = list(
        aspectmode = "manual",
        aspectratio = list(x = 1, y = 1, z = 0.1)
    ))









df_dis <- melt(q, var = c("y", "x"), value.name = "sev")
df_dis$sev <- df_dis$sev * 20

df_tom <- melt(p, var = c("y", "x"), value.name = "height")

df <- merge(df_tom, df_dis, all = T)
df$sev[df$height == 0] <- NA

plot1 <- ggplot(df, aes(x = x, y = y)) +
    geom_raster(aes(fill = sev)) +
    coord_fixed() +
    scale_fill_gradient(
        low = "#537636",
        high = "#dbb95f",
        na.value = "#884211",
        guide = FALSE
    ) +
    theme(
        panel.background = element_rect(fill = "#884211"),
        line = element_blank(),
        text = element_text(color = "transparent", size = 0)
    )

plot2 <- ggplot(df, aes(x = x, y = y)) +
    geom_raster(aes(fill = height), interpolate = T) +
    coord_fixed() +
    scale_fill_gradient(guide = FALSE) +
    theme(
        line = element_blank(),
        text = element_text(color = "transparent", size = 0)
    )

plot_gg(list(plot1, plot2),
    width = 5, height = 5, scale = 75, multicore = T,
    zoom = 0.7, theta = 10, phi = 30, windowsize = c(1200, 960),
    sunangle = 135
)