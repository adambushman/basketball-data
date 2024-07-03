comp = read.csv("~/eFG-wins-source.csv")

set.seed(14)
test.index = sample(c(1:dim(comp)[1]), .4*dim(comp)[1], replace = FALSE)


model = lm(comp$winRate~ comp$advantage, subset=-test.index)
summary(model)

plot(comp$advantage, comp$winRate
     , main="NBA Season eFG% Advantage vs Win Rate"
     , xlab="Season eFG% Advantage"
     , ylab="Season Win Rate")
abline(model, col = "red", lwd=2)
abline(v=0, col="green")
title(sub="Since 1996-97 Season", col.sub = "red")

#Gotta figure out how to fix this
#predict.lm(model, comp[test.index])

winPct = function(team, adv) {
  wp = 0.49636+(adv*4.75213)
  wpHigh = wp + 0.07787
  wpLow = wp - 0.07787
  
  print(paste(team, "Projected Win Rate:"))
  print(paste("High: ", round(wpHigh, 3), " (", round(wpHigh * 82, 0), ")", sep=""))
  print(paste("Low: ", round(wpLow, 3), " (", round(wpLow * 82, 0), ")", sep=""))
}

winPct("LAL", -0.01)

max(comp$advantage)


x = 0.058
o = 0.0912
n = 16
z = qnorm((1-0.9)/2,0,1)

up = x - ((o*z*1)/sqrt(n))
low = x + ((o*z*1)/sqrt(n))
