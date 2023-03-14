library("tidyr")
library("readxl")

shoot21 <- read.csv("~/R/testdirect/traditional-stats-21.csv")
shoot22 <- read.csv("~/R/testdirect/traditional-stats-22.csv")

addTS = function(df) {
        df$TSA = (df$FGA+(df$FTA*0.44))*2
        df$TSAgm = round(((df$FGA+(df$FTA*0.44)))/df$GP,1)
        df$TS = df$PTS/df$TSA
        
        laTSA = ((sum(df$FGA)+(sum(df$FTA*0.44))))/nrow(df)
        laTSAgm = (laTSA*nrow(df))/sum(df$GP)
        laTS = (sum(df$PTS)/(2*laTSA))/nrow(df)
        
        df$rTSA = round(df$TSA - laTSA,3)
        df$rTSAgm = round(df$TSAgm - laTSAgm,3)
        df$rTS = round(df$TS - laTS,3) 
        
        shooting = df %>% dplyr::select(SEASON, TEAM, PLAYER, rTS, rTSAgm)
        
        return(shooting)
}

shooting = rbind(addTS(shoot22), addTS(shoot21))



#################
#TEAM COMPARISON#
#################

shooting %>% dplyr::filter(shooting$SEASON == "2022" & shooting$TEAM == "UTA") %>% dplyr::arrange(desc(rTSAgm))


################
#YOY COMPARISON#
################

comp.ts = shooting[,-c(4)] %>% pivot_wider(names_from = SEASON, values_from = rTS)
comp.ts = data.frame(comp.ts)
comp.ts = comp.ts %>% filter(comp.ts$X2022 != "NaN" & comp.ts$X2021 != "NaN")
comp.ts$YOY = comp.ts$X2022-comp.ts$X2021

comp.ts = comp.ts %>% arrange(desc(comp.ts$YOY))


comp.tsa = shooting[,-c(3)] %>% pivot_wider(names_from = SEASON, values_from = rTSAgm)
comp.tsa = data.frame(comp.tsa)
comp.tsa = comp.tsa %>% filter(comp.tsa$X2022 != "NaN" & comp.tsa$X2021 != "NaN")
comp.tsa$YOY = comp.tsa$X2022-comp.tsa$X2021

comp.tsa = comp.tsa %>% arrange(desc(comp.tsa$YOY))

comp.full = merge(comp.tsa, comp.ts, by = "PLAYER")

###############
#SEARCH PLAYER#
name = "Jordan Clarkson"
###############


shooting %>% dplyr::filter(PLAYER == name)

plot(shooting$rTS, shooting$TSAgm,
     main = "True Shooting | Attempts vs Efficiency",
     xlab = "Adjusted TS Efficiency",
     ylab = "TS Attempts per Game",
     col=ifelse(shooting$PLAYER == name,'red', 'black'),
     pch=ifelse(shooting$PLAYER == name, 16,1))

###############
#COMPARE PLAYERS#
names = c("Scottie Barnes", "Evan Mobley", "Chris Duarte")
###############

shooting %>% dplyr::filter(PLAYER %in% names)

