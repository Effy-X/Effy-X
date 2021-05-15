library(s20x)

# Venezuela
x.V = gdpToFix$Time[1:15]
y.V = gdpToFix$Venezuela[1:15]
plot(x=x.V,y=y.V)
Venezuela.fit = lm(y.V~x.V)
summary(Venezuela.fit)
plot(residuals(Venezuela.fit))
normcheck(Venezuela.fit)

x.V.new = data.frame(x.V=c(16,17,18,19,20))
predict(Venezuela.fit, x.V.new, interval = "confidence")

#Turkmenistan
x.T = gdpToFix$Time[1:19]
y.T = gdpToFix$Turkmenistan[1:19]
plot(x=x.T,y=y.T)
Turkmenistan.fit = lm(y.T~x.T)
summary(Turkmenistan.fit)
plot(residuals(Turkmenistan.fit))
normcheck(Turkmenistan.fit)

x.T.new = data.frame(x.T=20)
predict(Turkmenistan.fit, x.T.new, interval = "confidence")

#Micronesia
x.M = gdpToFix$Time[1:19]
y.M = gdpToFix$"Micronesia, Fed. Sts."[1:19]
plot(x=x.M,y=y.M)
Micronesia.fit = lm(y.M~x.M)
summary(Micronesia.fit)
plot(residuals(Micronesia.fit))
normcheck(Micronesia.fit)

x.M.new = data.frame(x.M=20)
predict(Micronesia.fit, x.M.new, interval = "confidence")

#Iran
x.Iran = gdpToFix$Time[1:19]
y.Iran = gdpToFix$Iran[1:19]
plot(x=x.Iran,y=y.Iran)
Iran.fit = lm(y.Iran~x.Iran+I(x.Iran^2))
summary(Iran.fit)
plot(residuals(Iran.fit))
normcheck(Iran.fit)

x.Iran.new = data.frame(x.Iran=20)
predict(Iran.fit, x.Iran.new, interval = "confidence")

#Cuba
x.Cuba = gdpToFix$Time[1:19]
y.Cuba = gdpToFix$Cuba[1:19]
plot(x=x.Cuba,y=y.Cuba)
Cuba.fit = lm(y.Cuba~x.Cuba)
summary(Cuba.fit)
plot(residuals(Cuba.fit))
normcheck(Cuba.fit)

x.Cuba.new = data.frame(x.Cuba=20)
predict(Cuba.fit, x.Cuba.new, interval = "confidence")

#Sao Tome and Principe
x.Sao = gdpToFix$Time[1:19]
y.Sao = gdpToFix$"Sao Tome and Principe"[1:19]
plot(x=x.Sao,y=y.Sao)
Sao.fit = lm(y.Sao~x.Sao)
summary(Sao.fit)
plot(residuals(Sao.fit))
normcheck(Sao.fit)

x.Sao.new = data.frame(x.Sao=20)
predict(Sao.fit, x.Sao.new, interval = "confidence")

#Iraq
x.Iraq = gdpToFix$Time[1:16]
y.Iraq = gdpToFix$Iraq[1:16]
plot(x=x.Iraq,y=y.Iraq)
Iraq.fit = lm(y.Iraq~x.Iraq)
summary(Iraq.fit)
plot(residuals(Iraq.fit))
normcheck(Iraq.fit)

x.Iraq.new = data.frame(x.Iraq=c(17,18,19,20))
predict(Iraq.fit, x.Iraq.new, interval = "confidence")

#Afghanistan
x.Afghanistan = gdpToFix$Time[1:18]
y.Afghanistan = gdpToFix$Afghanistan[1:18]
plot(x=x.Afghanistan,y=y.Afghanistan)
Afghanistan.fit = lm(y.Afghanistan~x.Afghanistan+I(x.Afghanistan^2))
summary(Afghanistan.fit)
plot(residuals(Afghanistan.fit))
normcheck(Afghanistan.fit)

x.Afghanistan.new = data.frame(x.Afghanistan=c(19,20))
predict(Afghanistan.fit, x.Afghanistan.new, interval = "confidence")



