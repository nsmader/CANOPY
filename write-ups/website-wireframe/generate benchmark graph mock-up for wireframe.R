# Generate benchmakr graphs mock-up for wireframe

library(ggplot2)
setwd("~/GitHub/CANOPY/write-ups/website-wireframe")

nSteps <- 100000
steps <- 1:nSteps
#cent <- steps - 0.3*mean(steps)
#f <- 10.0*cent + cent^2+(cent/2)^3
x <- steps / nSteps * 2.5
f <- tanh(x)
e <- rnorm(nSteps)*(nSteps - steps)^3
e <- e / max(e) * max(f) * (1/3)
f.e <- f + e
df <- data.frame(cbind(steps, x, f, e, f.e))
df$ts <- f.e * 12 + 12

summary(df)


df$steps <- steps * 0.82
pad <- df[1:(.18*nSteps),]
pad[, !(colnames(pad) %in% "steps")] <- NA
pad$steps <- 

df$unif  <- 12
df$space <- 19
df$pov   <- 22



ggplot(data = df[seq(1, nrow(df), by = 10),]) + 
  geom_line(aes(steps, unif, linetype = factor("TS - Uniform Distribution"))) + 
  geom_line(aes(steps, space, linetype = factor("TS - Alloc Based on Density"))) + 
  geom_line(aes(steps, pov, linetype = factor("TS - Alloc Based on Poverty"))) +
  geom_line(aes(steps, ts, linetype = factor("CANOPY"))) +
  ylab("Targeting Score") +
  theme(legend.position = "bottom") +
  guides(linetype = guide_legend(nrow = 2), size = 4) +
  ggtitle("Targeting Score for CANOPY")
  scale_linetype("")
ggsave("./benchmark.png")
