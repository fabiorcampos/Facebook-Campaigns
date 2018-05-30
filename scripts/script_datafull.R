### Load data
df = read.csv("./data/cmkbr_full.csv", header = TRUE, sep = ",")

### prepare data
names <- df$Nome.da.campanha
ID <- df$ID
df <- df[,-c(1:2)]
df <- df[-c(17,42),] ## Outliers

### exploratory analysis
cor(df)
plot(df$Alcance ~ df$Resultados)
plot(df$Resultados ~ df$Valor.gasto..BRL.)
summary(df)
var(df)
sd(df)

### regression model for Resultados(CLicks)
model <- lm(df$Resultados ~ df$Valor.gasto..BRL.)
model 

teste <- data.frame(x=names[-c(17,42)])
predict <- predict(model, teste)

plot(df$Valor.gasto..BRL., 
     df$Resultados, 
     xlab = "Valor gasto na Campanha (R$)",
     ylab = "Clicks em Links",
     type = "p",
     col = "blue",
     pch=1,
     cex=1.5,
     main = "Regression Model for Facebook")
points(df$Valor.gasto..BRL., predict, type="p", col="red", pch=4)
abline(model, lwd=1, col='red')
legend("topright", c("y", "y_estimado"), pch=c(1,4), col=c("blue", "red"))

### regression model for Alcance(Público alcançado)
model2 <- lm(df$Alcance ~ df$Valor.gasto..BRL.)
model2 

teste2 <- data.frame(x=names[-c(17,42)])
predict2 <- predict(model2, teste2)

plot(df$Valor.gasto..BRL., 
     df$Alcance, 
     xlab = "Valor gasto na Campanha (R$)",
     ylab = "Alcance de Público",
     type = "p",
     col = "blue",
     pch=1,
     cex=1.5,
     main = "Regression Model for Facebook")
points(df$Valor.gasto..BRL., predict2, type="p", col="red", pch=4)
abline(model2, lwd=1, col='red')
legend("topright", c("y", "y_estimado"), pch=c(1,4), col=c("blue", "red"))



