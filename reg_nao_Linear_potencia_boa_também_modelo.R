# Carregue os pacotes no seu script:
library(ggplot2)

# Crie os dados fictícios (substitua pelos seus próprios dados):
dados <- data.frame(
  x = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
  y = c(3, 7, 13, 24, 43, 72, 110, 157, 215, 290)
)

# visualize a dispersão dos pontos:
ggplot(dados, aes(x = x, y= y)) + geom_point()



# Encontre valores iniciais razoáveis
inicial_a <- coef(lm(log(y) ~ x, data=dados))[1]
inicial_b <- coef(lm(log(y) ~ x, data=dados))[2]

# Realize a regressão não linear com o modelo de potência usando a função `nls`:
modelo_potencia <- nls(y ~ a * x^b, data = dados, start = list(a = exp(inicial_a), b = inicial_b))

# Extraia os coeficientes e o valor de R²:
coeficientes <- coef(modelo_potencia)
a <- coeficientes["a"]
b <- coeficientes["b"]

r2 <- 1 - sum(resid(modelo_potencia)^2) / sum((dados$y - mean(dados$y))^2)

# Crie um gráfico com os pontos originais e a curva ajustada usando o `ggplot2` e inclua a equação e o R² no gráfico:
ggplot(dados, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "nls", formula = y ~ a * x^b, 
              method.args = list(start = list(a = exp(inicial_a), b = inicial_b)), 
              se = FALSE, color = "red", linetype = "solid") +
  labs(title = "Regressão Não Linear - Modelo de Potência",
       x = "Variável Independente (x)",
       y = "Variável Dependente (y)") +
  annotate("text", x = min(dados$x), y = max(dados$y), hjust = 0, vjust = 1,
           label = sprintf("Equação: y = %.2f * x^%.2f\nR² = %.2f", a, b, r2),
           size = 4, color = "black")
