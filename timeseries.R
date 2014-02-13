

summary(gls(value~season,data=jost.diversity_campanha_modulo_long[1:49,]))

plot(value~season,data=jost.diversity_campanha_modulo_long[1:49,])

as.Date(jost.diversity_campanha_modulo_long$season,origin=1)

?as.Date
