# Carregue os pacotes no seu script:
library(ggplot2)

# Crie os dados fictícios (teor de proteínas de 0% a 40%, com dados em triplicata para cada ponto):
set.seed(123)  # Para reprodutibilidade
teor_proteinas <- seq(0, 40, by=8)

# Ajustando a dispersão nos dados:
# A curva atinge um máximo em torno de 20% de proteínas e depois decresce
ganho_peso <- -0.2 * (teor_proteinas - 20)^2 + 400 + rnorm(length(teor_proteinas), 0, 10)

dados <- data.frame(
  Proteina = rep(teor_proteinas, each=3),
  Peso = rep(ganho_peso, each=3) + rnorm(length(ganho_peso) * 3, 0, 5)  # Menor variabilidade nos triplicatas
)

# Visualize a dispersão dos pontos:
ggplot(dados, aes(x = Proteina, y = Peso)) + geom_point()

# Prepare o modelo polinomial do segundo grau:
dados$Proteina2 <- dados$Proteina^2
modelo_pol <- lm(Peso ~ Proteina + Proteina2, data = dados)

# Extraia os coeficientes e o valor de R²:
coeficientes <- coef(modelo_pol)
a <- coeficientes["(Intercept)"]
b <- coeficientes["Proteina"]
c <- coeficientes["Proteina2"]
r2 <- summary(modelo_pol)$r.squared

# ------------------------------
library(ggplot2)
library(dplyr)
library(ggthemes)
library(ggpubr)

# Supondo que os dados originais já foram criados com triplicatas, como especificado anteriormente
# dados <- data.frame(...)

# Calcular a média, o erro padrão e o desvio padrão para cada nível de Proteina
dados_sumario <- dados %>%
  group_by(Proteina) %>%
  summarise(
    Media = mean(Peso),
    ErroPadrao = sd(Peso) / sqrt(n()),
    DesvioPadrao = sd(Peso),
    .groups = 'drop'  # Removendo o agrupamento após o summarise
  )

# Preparando o modelo polinomial para usar na linha de ajuste
dados$Proteina2 <- dados$Proteina^2
modelo_pol <- lm(Peso ~ Proteina + Proteina2, data = dados)

# Gráfico com média e barra de erro padrão
ggplot(dados, aes(x = Proteina, y = Peso)) +
  geom_point(alpha=0.5, color = "black") +  # Pontos semi-transparentes para visualizar a sobreposição
  geom_point(data = dados_sumario, aes(y = Media), color = "blue", size = 2, shape = 17) +  # Pontos da média
  geom_errorbar(data = dados_sumario, 
                aes(ymin = Media - ErroPadrao, 
                    ymax = Media + ErroPadrao, 
                    x = Proteina,
                    y = Media), 
                linewidth = 0.8,
                width = 0.8, 
                color = "red") +
  geom_text(data = dados_sumario, aes(label = sprintf("%.2f ± %.2f", Media, DesvioPadrao), y = Media + ErroPadrao + 5), vjust = 0) +
  stat_smooth(data = dados, aes(x = Proteina, y = Peso), method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "red", linetype = "solid") +
  labs(x = "Teor de Proteínas (%)",
       y = "Ganho de Peso (g)") +
  annotate("text", x = min(dados$Proteina) + 10, y = min(dados$Peso) + 20, hjust = 0.1, vjust = 1,
           label = sprintf("Equação: y = %.2f + %.2f*x + %.2f*x^2\nR² = %.2f", coef(modelo_pol)["(Intercept)"], coef(modelo_pol)["Proteina"], coef(modelo_pol)["Proteina2"], summary(modelo_pol)$r.squared),
           size = 4, color = "black") +
  theme_classic() +
  theme(axis.text = element_text(size = 11))
