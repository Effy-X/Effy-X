widedata <- read.csv("GDP.csv", header = TRUE)
library(tidyr)
longdata <- gather (widedata,Year,Country,Afghanistan:Zimbabwe, factor_key=TRUE)
write.table (longdata, file ="GDP2.csv", sep =",", row.names =TRUE, col.names =TRUE, quote =TRUE)

library("readxl")
library(plm)

SuicideGDP.df <- read_excel("MergingData.xlsx", sheet = "Sheet1")
SuicideGDP.df

BothSexes.df <- SuicideGDP.df[which(SuicideGDP.df$SEX == "Both sexes"), ]

panel.BothSexes.df <- BothSexes.df [,-3]
panel.bothsexes.pool = plm(SuicideRate ~ GDPperCapita,data = panel.BothSexes.df,model = "pooling")
summary(panel.bothsexes.pool)

panel.bothsexes.fixed = plm(SuicideRate ~ GDPperCapita, data = panel.BothSexes.df,model = "within")
summary(panel.bothsexes.fixed)

fixef(panel.bothsexes.fixed)
pFtest(panel.bothsexes.fixed,panel.bothsexes.pool)

panel.bothsexes.random = plm(SuicideRate ~ GDPperCapita,data = panel.BothSexes.df,model = "random", random.method = "walhus")
summary(panel.bothsexes.random)

plmtest(panel.bothsexes.pool)

phtest(panel.bothsexes.random,panel.bothsexes.fixed)


nz.bothsex.df <- BothSexes.df[which(BothSexes.df$COUNTRY == "New Zealand"), ]

nz.bothsexes <- lm(SuicideRate ~ GDPperCapita, data = nz.bothsex.df)
summary(nz.bothsexes)


Female.df <- SuicideGDP.df[which(SuicideGDP.df$SEX == "Female"), ]
Male.df <- SuicideGDP.df[which(SuicideGDP.df$SEX == "Male"), ]

nz.female.df <- Female.df[which(Female.df$COUNTRY == "New Zealand"), ]
nz.male.df <- Male.df[which(Male.df$COUNTRY == "New Zealand"), ]

nz.female <- lm(SuicideRate ~ GDPperCapita, data = nz.female.df)
summary(nz.female)

nz.male <- lm(SuicideRate ~ GDPperCapita, data = nz.male.df)
summary(nz.male)