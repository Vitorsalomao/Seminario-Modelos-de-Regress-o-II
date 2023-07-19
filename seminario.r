---
title: "Seminário Regressão II"
author: "Lucas e Vitor"
format: beamer
editor: visual
jupyter: ir
---

```{R}
library(MASS)
library(readr)
library(hnp)
library(ggplot2)
library(broom)
library(statmod)

```

```{R}
setwd("C:/Users/vitor/Documents/UFC/Regressão/Seminário")
dados <- read.csv("petrobras.csv")
View(df)
```

```{R}
data <- df[, c("date", "adjclose_petro", "adjclose_barril", "adjclose_dolar", "adjclose_b3")]
colnames(data) <- c("date", "Petrobras", "Barril de Petróleo", "Dólar", "B3")
data$date <- as.Date(data$date)
melted_data <- reshape2::melt(data, id.vars = "date", variable.name = "ativo", value.name = "value")

# Create the ggplot object
plot <- ggplot(melted_data)

# Define the x, y variables, and the aesthetics (color) for each line
plot <- plot + aes(x = date, y = value, color = ativo)

# Add the lines to the plot
plot <- plot + geom_line(size = 1.5, alpha = 0.8)

# Customize the plot
plot <- plot + labs(title = "Preços dos Ativos", x = "Data", y = "Preço")
plot <- plot + scale_x_date(date_breaks = "3 months")
plot <- plot + theme_minimal()
plot <- plot + labs(color = "Ativo")
plot <- plot + theme_gray()

# Display the plot
print(plot)
```

```{R}
data1 <- df[, c("date", "adjclose_petro", "MM7_petro", "MM14_petro")]
colnames(data1) <- c("date", "Petrobras", "Média Móvel 7 dias", "Média Móvel 14 dias")
data1$date <- as.Date(data1$date)
melted_data <- reshape2::melt(data1, id.vars = "date", variable.name = "ativo", value.name = "value")

# Create the ggplot object
plot <- ggplot(melted_data)

# Define the x, y variables, and the aesthetics (color) for each line
plot <- plot + aes(x = date, y = value, color = ativo)

# Add the lines to the plot
plot <- plot + geom_line(size = 0.7, alpha = 0.5)

# Customize the plot
plot <- plot + labs(title = "Petrobras e Médias Móveis", x = "Data", y = "Preço")
plot <- plot + scale_x_date(date_breaks = "3 months")
plot <- plot + theme_minimal()
plot <- plot + labs(color = "Ativo")
plot <- plot + theme_gray()

# Display the plot
print(plot)
```

```{R}
resp = dados$adjclose_petro
x1 = dados$MM7_petro
x2 = dados$adjclose_barril
#x2 = dados$MM7_barril
#x4 = dados$MM14_barril
x3 = dados$adjclose_dolar
#x6 = dados$MM14_dolar
x4 = dados$adjclose_b3
#x8 = dados$MM14_b3
base = data.frame(Y = resp, X1 = x1, X2 = x2, X3 = x3, X4 = x4)
```

Modelo Gamma com ligação canônica -\> Recíproca (Inversa)

```{R}
modelo1 = glm(resp ~ x1 + x2 + x3 + x4, family = Gamma(link = 'inverse'))
summary(modelo1)
summary(modelo1, dispersion = gamma.dispersion(modelo1))

anova(modelo1, test = 'F')
```

```{R}
plot(modelo1, pch = 19)


```

```{R}
res_q01 = qresiduals(modelo1)
shapiro.test(res_q01)
```

```{R}
analise1 <- data.frame(res_q01 = modelo1$residuals, fitted = modelo1$fitted.values)

# Criar o gráfico utilizando o ggplot2
ggplot(analise1, aes(x = fitted, y = res_q01)) +
  geom_point() +                   # Pontos dos resíduos
  geom_hline(yintercept = 0, col = "red") +   # Linha horizontal em vermelho
  labs(x = "Valores ajustados", y = "Resíduos")
```

```{R}
y_1 = fitted.values(modelo1)
X1 = model.matrix(modelo1)
W = modelo1$weights
W = diag(W)

```

Matriz de projeção H

```{R}
H = sqrt(W)%*%X1%*%solve(t(X1)%*%W%*%X1)%*%t(X1)%*%sqrt(W)
h = diag(H)

```

```{R}
plot(h, xlab = 'Indices', ylab = 'Elementos da matriz de projeção', pch = 19)+identify(h)

```

```{R}
residuos_odinarios = residuals(modelo1, type = 'response')
residuos_componenteDeviance = residuals(modelo1, type = 'deviance')

residuos_padrao = residuos_componenteDeviance/sqrt(0.001448948 * (1 - h))
```

Componente da deviance padronizado.

```{R}
#Residuo de Pearson
residuos_Pearson = residuals(modelo1, type = 'pearson')

# Residuo de Pearson padronizado
residuos_Pearson_Padrao = residuos_Pearson/sqrt(0.001448948 * (1 - h))
```

Avaliar o afastamento da verossimilhança

```{R}
# Diferença da verossimilhança
LD = (round(h,4)/(1-round(h,4)))*(round(residuos_Pearson_Padrao^2, 4))
```

COMPONENTE DA DEVIANCE

```{R}
plot(residuos_padrao, xlab = 'Indices', ylab = 'Componente da Deviance', pch = 19)+title('Resíduos - Componente da Deviance Padronizada') + abline(h = 0, col = 'red')
```

```{R}
plot(LD, xlab = 'Indices', ylab = 'Distancia de Cook', pch = 19)+title('Distancia de Cook vs Indices') + identify(LD)

```

```{R}
qqnorm(res_q01, col = 'blue', pch = 19) + qqline(res_q01)
```

```{R}
set.seed(1234)
hnp(modelo1, main = 'Modelo Gamma Inversa', print.on = TRUE, how.many.out = TRUE, paint.out = TRUE, pch=19)
```

```{R}
df <- data.frame(resp = resp, fitted = fitted(modelo1))

# Verificar se as colunas existem no data frame
print(colnames(df))

# Verificar se os nomes das colunas correspondem às estéticas usadas na função aes() dentro do geom_line()

# Criar o gráfico utilizando o ggplot2
ggplot(df) +
  geom_line(aes(x = seq_along(resp), y = resp), color = 'black') +
  geom_line(aes(x = seq_along(fitted), y = fitted), color = 'red') +
  labs(x = "Contagem", y = "Preço") +
  ggtitle("Ajustado para Modelo Gama Inversa")
```

```{R}
#influence.measures(modelo_semX4)$infmat
plot(influence.measures(modelo1)$infmat[,5], pch = 19) + identify(influence.measures(modelo1)$infmat[,5])

```

Modelo Gamma com ligação canônica -\> Recíproca (Logaritmica)

```{R}
modelo2 = glm(resp ~ x1 + x2 + x3 + x4, family = Gamma(link = 'log'))
summary(modelo2)
summary(modelo2, dispersion = gamma.dispersion(modelo2))
```

```{R}
anova(modelo2, test = 'F')
```

\# O coeficiente de X2 não é significativo a 5%

```{R}
modelo_sem_X2 = glm(resp ~ x1 + x3 +x4, family = Gamma(link = 'log'))
summary(modelo_sem_X2)
summary(modelo_sem_X2, dispersion = gamma.dispersion(modelo_sem_X2))
anova(modelo_sem_X2, test = 'F')
```

```{R}
res_q02 = qresiduals(modelo_sem_X2)
shapiro.test(res_q02)
plot(res_q02 ~ fitted(modelo_sem_X2)) + abline(h = 0, col = 'red')

```

```{R}
y_2 = fitted.values(modelo_sem_X2)
X2 = model.matrix(modelo_sem_X2)
W2 = modelo_sem_X2$weights
W2 = diag(W2)

```

\# Matriz de projeção H

```{R}
H2 = sqrt(W2)%*%X2%*%solve(t(X2)%*%W2%*%X2)%*%t(X2)%*%sqrt(W2)
h2 = diag(H2)
```

```{R}
plot(h2, xlab = 'Indices', ylab = 'Elementos da matriz de projeção', pch = 19)
```

Análise dos resíduos

```{R}
residuos_odinarios2 = residuals(modelo_sem_X2, type = 'response')
residuos_componenteDeviance2 = residuals(modelo_sem_X2, type = 'deviance')
```

```{R}
# Componente da deviance padronizado
residuos_padrao2 = residuos_componenteDeviance2/sqrt(0.001229335 * (1 - h2))
```

Resíduo de Pearson

```{R}
residuos_Pearson2 = residuals(modelo_sem_X2, type = 'pearson')
# Residuo de Pearson padronizado
residuos_Pearson_Padrao2 = residuos_Pearson2/sqrt(0.001229335 * (1 - h2))
```

Avaliar o afastamento da verossimilhança

```{R}
# Diferença da verossimilhança
LD2 = (round(h2,4)/(1-round(h2,4)))*(round((residuos_Pearson_Padrao2)^2, 4))
```

COMPONENTE DA DEVIANCE

```{R}
plot(residuos_padrao2, xlab = 'Indices', ylab = 'Componente da Deviance', pch = 19)+title('Resíduos - Componente da Deviance Padronizada')+abline(h = 0, col = 'red')+identify(residuos_padrao2)
```

```{R}
plot(LD2, xlab = 'Indices', ylab = 'Distancia de Cook', pch = 19)+title('Distancia de Cook vs Indices') +identify(LD2)

```

```{R}
qqnorm(res_q02, col = 'blue', pch = 19)+qqline(res_q02)
```

```{R}
set.seed(1234)
hnp(modelo_sem_X2, main = 'Modelo Gamma Ligação Logarítmica', print.on = TRUE, how.many.out = TRUE,paint.out = TRUE, pch=19)
```

```{R}
df1 <- data.frame(resp = resp, fitted = fitted(modelo_sem_X2))

# Verificar se as colunas existem no data frame
print(colnames(df1))

# Verificar se os nomes das colunas correspondem às estéticas usadas na função aes() dentro do geom_line()

# Criar o gráfico utilizando o ggplot2
ggplot(df1) +
  geom_line(aes(x = seq_along(resp), y = resp), color = 'black') +
  geom_line(aes(x = seq_along(fitted), y = fitted), color = 'green') +
  labs(x = "Contagem", y = "Preço") +
  ggtitle("Ajustado para Modelo Gama Canônica")
```

```{R}
plot(influence.measures(modelo_sem_X2)$infmat[,5], pch = 19)+identify(influence.measures(modelo_sem_X2)$infmat[,5])
```

Modelo Gamma com ligação -\> Identidade

```{R}
modelo3 = glm(resp ~ x1 + x2 + x3 + x4, family = Gamma(link = 'identity'))
summary(modelo3) 
summary(modelo3, dispersion = gamma.dispersion(modelo3))
```

```{R}
anova(modelo3, test = 'F')
```

#O coeficiente de X4 não é significativo a 5%

```{R}
modelo3_semX4 = glm(resp ~ x1 + x2 + x3, family = Gamma(link = 'identity'))
summary(modelo3_semX4)
summary(modelo3_semX4, dispersion = gamma.dispersion(modelo3_semX4_X2))
anova(modelo3_semX4_X2, test = 'F')
```

```{R}
res_q03 = qresiduals(modelo3_semX4)
shapiro.test(res_q03)
plot(res_q03 ~ fitted(modelo3_semX4)) +abline(h = 0, col = 'red')

```

```{R}
y_3 = fitted.values(modelo3_semX4)
X3 = model.matrix(modelo3_semX4)
W3 = modelo3_semX4$weights
W3 = diag(W3)

```

\# Matriz de projeção H

```{R}
H3 = sqrt(W3)%*%X3%*%solve(t(X3)%*%W3%*%X3)%*%t(X3)%*%sqrt(W3)
h3 = diag(H3)

```

```{R}
plot(h3, xlab = 'Indices', ylab = 'Elementos da matriz de projeção', pch = 19)
```

\# Análise dos resíduos

```{R}
residuos_odinarios3 = residuals(modelo3_semX4, type = 'response')
residuos_componenteDeviance3 = residuals(modelo3_semX4, type = 'deviance')
```

```{R}
# Componente da deviance padronizado
residuos_padrao3 = residuos_componenteDeviance3/sqrt(0.00122493 * (1 - h3))
```

Residuo de Pearson

```{R}
residuos_Pearson3 = residuals(modelo3_semX4, type = 'pearson')
# Residuo de Pearson padronizado
residuos_Pearson_Padrao3 = residuos_Pearson3/sqrt(0.00122493 * (1 - h3))

```

Avaliar o afastamento da verossimilnaça

```{R}
# Diferença da verossimilhança
LD3 = (round(h3,4)/(1-round(h3,4)))*(round((residuos_Pearson_Padrao3)^2, 4))

```

\# COMPONENTE DA DEVIANCE

```{R}
plot(residuos_padrao3, xlab = 'Indices', ylab = 'Componente da Deviance', pch = 19)
title('Resíduos - Componente da Deviance Padronizada')+abline(h = 0, col = 'red')+identify(residuos_padrao3)

```

```{R}
plot(LD3, xlab = 'Indices', ylab = 'Distancia de Cook', pch = 19) + title('Distancia de Cook vs Indices') + identify(LD3)
```

```{R}
qqnorm(res_q03, col = 'blue', pch = 19) + qqline(res_q03)
```

```{R}
set.seed(1234)
hnp(modelo3_semX4, main = 'Modelo Gamma Ligação Identidade', print.on = TRUE, how.many.out = TRUE,paint.out = TRUE, pch=19)
```

```{R}
plot(resp, type = 'l') + lines(fitted(modelo3_semX4), col = 'red')
```

```{R}
df3 <- data.frame(resp = resp, fitted = fitted(modelo3_semX4))

# Verificar se as colunas existem no data frame
print(colnames(df3))

# Verificar se os nomes das colunas correspondem às estéticas usadas na função aes() dentro do geom_line()

# Criar o gráfico utilizando o ggplot2
ggplot(df3) +
  geom_line(aes(x = seq_along(resp), y = resp), color = 'black') +
  geom_line(aes(x = seq_along(fitted), y = fitted), color = 'purple') +
  labs(x = "Contagem", y = "Preço") +
  ggtitle("Ajustado para Modelo Gama Identidade")
```

```{R}
plot(influence.measures(modelo3_semX4)$infmat[,5], pch = 19)
identify(influence.measures(modelo3_semX4)$infmat[,5])

```

```{R}
# Comparações
AIC(modelo1, modelo_sem_X2, modelo3_semX4)
```

```{R}
plot(resp, type = 'l')
lines(fitted(modelo1), col = 'red')
lines(fitted(modelo_sem_X2), col = 'green')
lines(fitted(modelo3_semX4_X2_X3), col = 'purple')
```

```{R}
df4 <- data.frame(resp = resp,
                 fitted_modelo1 = fitted(modelo1),
                 fitted_modelo_sem_X2 = fitted(modelo_sem_X2),
                 fitted_modelo3_semX4 = fitted(modelo3_semX4))

# Criar o gráfico utilizando o ggplot2
ggplot(df4) +
  geom_line(aes(y = resp), color = 'black') +
  geom_line(aes(y = fitted_modelo1), color = 'red') +
  geom_line(aes(y = fitted_modelo_sem_X2), color = 'green') +
  geom_line(aes(y = fitted_modelo3_semX4), color = 'purple') +
  labs(x = 'Índice', y = 'Valores') +
  ggtitle('Gráfico com Múltiplas Linhas')
```

```{R}
df <- data.frame(
  resp = resp,
  fitted_modelo1 = fitted(modelo1),
  fitted_modelo_sem_X2 = fitted(modelo_sem_X2),
  fitted_modelo3_semX4 = fitted(modelo3_semX4)
)

# Criar o gráfico utilizando o ggplot2
ggplot(df) +
  geom_line(aes(x = seq_along(resp), y = resp), color = 'black') +
  geom_line(aes(x = seq_along(fitted_modelo1), y = fitted_modelo1), color = 'red') +
  geom_line(aes(x = seq_along(fitted_modelo_sem_X2), y = fitted_modelo_sem_X2), color = 'green') +
  geom_line(aes(x = seq_along(fitted_modelo3_semX4), y = fitted_modelo3_semX4), color = 'purple') +
  ggtitle('Gráfico com Múltiplas Linhas') + scale_color_manual(values = c('black', 'red', 'green', 'purple') + labels = c('Petrobras', 'Inversa', 'Logística', 'Identidade')) + labs(x = 'Índice', y = 'Valores')
```
