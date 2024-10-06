# Carregue os pacotes no seu script:
library(ggplot2)
library(dplyr)
library(ggthemes)
library(ggpubr)
library(readxl)
library(janitor)
library(cowplot)
library(readr)

# Importando os dados 

dados <- read_excel(path = "dados/Dados artigo Alho - Evaldo.xlsx", sheet = 2, 
                    col_types =  ) |> clean_names()

# Visualize a dispersão dos pontos:
ggplot(dados, aes(x = niveis , y = gp)) + geom_point()

# Weigth Gain -------------------------------------------
# Ajustando o modelo de regressão linear simples
dados$niveis2 <- dados$niveis^2
modelo_pol <- lm(gp ~ niveis + niveis2, data = dados)

# Extraia os coeficientes e o valor de R² e p-valor:
coeficientes <- coef(modelo_pol)
a <- coeficientes["(Intercept)"]
b <- coeficientes["niveis"]
r2 <- summary(modelo_pol)$r.squared
p_valor <- summary(modelo_pol)$fstatistic
p_valor <- pf(p_valor[1], p_valor[2], p_valor[3], lower.tail = FALSE)

# Calcular a média, o erro padrão e o desvio padrão para cada nível de niveis
dados_sumario <- dados %>%
  group_by(niveis) %>%
  summarise(
    Media = mean(gp),
    ErroPadrao = sd(gp) / sqrt(n()),
    DesvioPadrao = sd(gp),
    .groups = 'drop'  # Removendo o agrupamento após o summarise
  )

# Gráfico com média e barra de erro padrão
g1 <- ggplot(dados, aes(x = niveis, y = gp)) +
  geom_point(alpha=0.5, color = "black") +  
  # Pontos da média:
  geom_point(data = dados_sumario, aes(y = Media), 
             color = "blue", size = 2, shape = 17) +  
  geom_errorbar(data = dados_sumario, 
                aes(ymin = Media - ErroPadrao, 
                    ymax = Media + ErroPadrao, 
                    x = niveis,
                    y = Media), 
                linewidth = 0.8,
                width = 0.05, 
                color = "red") +
  # Média +- dp :
  geom_text(data = dados_sumario, 
            aes(label = sprintf("%.2f ± %.2f", Media, DesvioPadrao), 
                                      y = Media + ErroPadrao + 5), vjust = 0) +
  stat_smooth(data = dados, aes(x = niveis, y = gp), method = "lm", 
              formula = y ~ poly(x, 2), se = TRUE, color = "black", 
              linetype = "solid") +
  labs(x = expression("Garlic essential oil (g.kg"^"-1" * ")"),
       y = "Weight gain (g)") +
  annotate("text", x = min(dados$niveis) + 0.4 , y = min(dados$gp) + 20, 
           hjust = 0.1, vjust = 1,
           label = sprintf("y = %.2f + %.2f*x + %.2f*x^2\nR² = %.2f, p = %.4f",
                           coef(modelo_pol)["(Intercept)"], coef(modelo_pol)["niveis"], 
                           coef(modelo_pol)["niveis2"], summary(modelo_pol)$r.squared, p_valor),
           size = 4, color = "black") +
  annotate("text", x = max(dados$niveis) +0 , y = max(dados$gp), label = "A", 
           hjust = 1.1, vjust = -0.1, size = 5) +
  scale_x_continuous(limits = c(-0.1,1.55))+
  scale_y_continuous(breaks = seq(100, 170, by = 10))+
  theme_classic() + 
  theme(axis.text = element_text(size = 12))
g1

# Length Gain -------------------------------------------
# Ajustando o modelo de regressão linear simples
dados$niveis2 <- dados$niveis^2
modelo_pol <- lm(gc ~ niveis + niveis2, data = dados)

# Extraia os coeficientes e o valor de R² e p-valor:
coeficientes <- coef(modelo_pol)
a <- coeficientes["(Intercept)"]
b <- coeficientes["niveis"]
r2 <- summary(modelo_pol)$r.squared
p_valor <- summary(modelo_pol)$fstatistic
p_valor <- pf(p_valor[1], p_valor[2], p_valor[3], lower.tail = FALSE)

# Calcular a média, o erro padrão e o desvio padrão para cada nível de niveis
dados_sumario <- dados %>%
  group_by(niveis) %>%
  summarise(
    Media = mean(gc),
    ErroPadrao = sd(gc) / sqrt(n()),
    DesvioPadrao = sd(gc),
    .groups = 'drop'  # Removendo o agrupamento após o summarise
  )

# Gráfico com média e barra de erro padrão
g2 <- ggplot(dados, aes(x = niveis, y = gc)) +
  geom_point(alpha=0.5, color = "black") +  
  # Pontos da média:
  geom_point(data = dados_sumario, aes(y = Media), 
             color = "blue", size = 2, shape = 17) +  
  geom_errorbar(data = dados_sumario, 
                aes(ymin = Media - ErroPadrao, 
                    ymax = Media + ErroPadrao, 
                    x = niveis,
                    y = Media), 
                linewidth = 0.8,
                width = 0.05, 
                color = "red") +
  # Média +- dp :
  geom_text(data = dados_sumario, 
            aes(label = sprintf("%.2f ± %.2f", Media, DesvioPadrao), 
                y = Media + ErroPadrao + 0.1), vjust = 0) +
  stat_smooth(data = dados, aes(x = niveis, y = gc), method = "lm", 
              formula = y ~ poly(x, 2), se = TRUE, color = "black", 
              linetype = "solid") +
  labs(x = expression("Garlic essential oil (g.kg"^"-1" * ")"),
       y = "Length gain (g)") +
  annotate("text", x = min(dados$niveis) + 0.4 , y = min(dados$gc) + 0.7, 
           hjust = 0.1, vjust = 1,
           label = sprintf("y = %.2f + %.2f*x + %.2f*x^2\nR² = %.2f, p = %.4f",
                           coef(modelo_pol)["(Intercept)"], coef(modelo_pol)["niveis"], 
                           coef(modelo_pol)["niveis2"], summary(modelo_pol)$r.squared, p_valor),
           size = 4, color = "black") +
  annotate("text", x = max(dados$niveis) +0 , y = max(dados$gc), label = "B", 
           hjust = 1.1, vjust = -0.1, size = 5) +
  scale_x_continuous(limits = c(-0.07,1.55))+
  scale_y_continuous(breaks = seq(8, 11, by = 0.5)) +
  theme_classic() + 
  theme(axis.text = element_text(size = 12))
g2

# Combinando plots -------------
combined_plot <- plot_grid(g1, g2, ncol = 2)

# Exibir o gráfico combinado
print(combined_plot)

ggsave("Fig1.png", plot = combined_plot, 
       width = 30, height = 10, dpi = 300,  units = "cm")


# Crude protein -------------------------------------------
# Ajustando o modelo de regressão linear simples
dados$niveis2 <- dados$niveis^2
modelo_pol <- lm(pb ~ niveis + niveis2, data = dados)

# Extraia os coeficientes e o valor de R² e p-valor:
coeficientes <- coef(modelo_pol)
a <- coeficientes["(Intercept)"]
b <- coeficientes["niveis"]
r2 <- summary(modelo_pol)$r.squared
p_valor <- summary(modelo_pol)$fstatistic
p_valor <- pf(p_valor[1], p_valor[2], p_valor[3], lower.tail = FALSE)

# Calcular a média, o erro padrão e o desvio padrão para cada nível de niveis
dados_sumario <- dados %>%
  group_by(niveis) %>%
  summarise(
    Media = mean(pb),
    ErroPadrao = sd(pb) / sqrt(n()),
    DesvioPadrao = sd(pb),
    .groups = 'drop'  # Removendo o agrupamento após o summarise
  )

# Gráfico com média e barra de erro padrão
g3 <- ggplot(dados, aes(x = niveis, y = pb)) +
  geom_point(alpha=0.5, color = "black") +  
  # Pontos da média:
  geom_point(data = dados_sumario, aes(y = Media), 
             color = "blue", size = 2, shape = 17) +  
  geom_errorbar(data = dados_sumario, 
                aes(ymin = Media - ErroPadrao, 
                    ymax = Media + ErroPadrao, 
                    x = niveis,
                    y = Media), 
                linewidth = 0.8,
                width = 0.05, 
                color = "red") +
  # Média +- dp :
  geom_text(data = dados_sumario, 
            aes(label = sprintf("%.2f ± %.2f", Media, DesvioPadrao), 
                y = Media + ErroPadrao + 0.1), vjust = 0) +
  stat_smooth(data = dados, aes(x = niveis, y = pb), method = "lm", 
              formula = y ~ poly(x, 2), se = TRUE, color = "black", 
              linetype = "solid") +
  labs(x = expression("Garlic essential oil (g.kg"^"-1" * ")"),
       y = "Crude protein (g)") +
  annotate("text", x = min(dados$niveis) + 0.4 , y = min(dados$pb) + 0.7, 
           hjust = 0.1, vjust = 1,
           label = sprintf("y = %.2f + %.2f*x + %.2f*x^2\nR² = %.2f, p = %.4f",
                           coef(modelo_pol)["(Intercept)"], coef(modelo_pol)["niveis"], 
                           coef(modelo_pol)["niveis2"], summary(modelo_pol)$r.squared, p_valor),
           size = 4, color = "black") +
  scale_x_continuous(limits = c(-0.07,1.55))+
  scale_y_continuous(breaks = seq(14, 19, by = 0.5)) +
  theme_classic() + 
  theme(axis.text = element_text(size = 11))
g3

ggsave("Fig2.png", width = 15, height = 11, 
       dpi = 300, units = "cm")
