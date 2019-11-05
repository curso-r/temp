library(broom)
library(readxl)
library(janitor)
library(MASS)
library(lime)
library(caret)
library(GGally)
library(car)
library(ISLR)
library(tidyverse)
#--------------------------------------------------
# Os dados

# cars
glimpse(cars)
ggplot(cars) + 
  geom_point(aes(x = speed, y = dist))

# ajuste de uma regressão linear simples
melhor_reta <- lm(dist ~ speed, data = cars)


# tabela com as predições junto
cars_com_predicoes <- melhor_reta %>% 
  augment()

glimpse(cars_com_predicoes)


# Boston
glimpse(Boston)

ggpairs(Boston)


#--------------------------------------------------
# Exercício 1: calcule o EQM da melhor reta e compare com a saída do `summary(melhor_reta)`
# EQM = mean((y - yh)^2)

y = cars_com_predicoes$dist
yh = cars_com_predicoes$.fitted

EQM <-

summary(melhor_reta)


#--------------------------------------------------
# Exercício 2: calcule beta0 e beta1

x <- 
y <- 
  
xbarra <-
ybarra <-
  
beta1 <-
beta0 <- ""




#--------------------------------------------------
# Exercício 3: tire as informações do objeto `melhor_reta`
# para decidir se speed está associado com dist.
# Use a função summary().

#--------------------------------------------------
# Exercício 4: Interprete o parâmetro `speed` (beta 1).

#--------------------------------------------------
# Exercício 5: calcule o R2 para a `melhor reta` e depois
# compare com o valor da saída do `summary()`.

#--------------------------------------------------
# Exercício 6: calcule o R2 ajustado para a `melhor reta` e depois
# compare com o valor da saída do `summary()`.

#--------------------------------------------------
# Exercício 7: Estude os gráficos que saem do `plot(melhor_reta)`.
# Procure por outliers.
plot(melhor_reta)


#--------------------------------------------------
# Exercício 8: Interprete os parametros do modelo abaixo
# Balance: Saldo no Cartão de Crédito
# Gender: Masculino ou Feminino
modelo_balance <- lm(Balance ~ Gender, data = Credit)
summary(modelo_balance)


#--------------------------------------------------
# Exercício 9: Crie um pipeline utilizando group_by() e summarise()
# para calcular as médias de Balance por Gender. Compare os resultados
# com o exercício anterior.



#--------------------------------------------------
# Exercício 10: Explore como usar a função model.matrix().
# Use a fórmula do modelo do exercício 8 na model.matrix() e 
# veja sua saída.



#--------------------------------------------------
# Exercício 10: Repita os exercícios 8 e 9, mas agora
# usando Ethnicity no lugar de Gender.


#--------------------------------------------------
# Exercício 11: Crie um boxplot de Balance para cada Ethnicity
# e avalie se a análise visual é compatível com o que os valores-p
# indicam.


#--------------------------------------------------
# Exercício 12: Use os dados simulados `y_x` abaixo para ajustar dois modelos:
# 1) y ~ x
# 2) y ~ log10(x)
# Avalie qual modelo é melhor quanto ao EQM e quanto ao R^2.
set.seed(1)
y_x <- tibble(
  x = runif(60),
  y = 10 + 0.5*log(x) + rnorm(30, sd = 0.1)
) 

#--------------------------------------------------
# Exercício 13: Explore a saída de model.matrix()
# utlizando as fórmulas
# y ~ x
# y ~ poly(x, 2)
# y ~ poly(x, 3)
set.seed(1)
y_x_poly <- tibble(
  x = runif(30, 0, 20),
  y = 500 + 0.4 * (x-10)^3 + rnorm(30, sd = 50)
)




#--------------------------------------------------
# Exercício 14: interações
iris %>%
  ggplot(aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point(aes(colour = Species)) +
  geom_smooth(method = "lm", se = FALSE, colour = "black") +
  geom_smooth(method = "lm", aes(colour = Species), se = FALSE) 

glimpse(iris)

# O Modelo abaixo ajusta Sepal.Width explicada por 
# Sepal.Length (variável contínua)
# Species (Variável categórica)
# Interações entre Sepal.Length e Species.
modelo_iris <- lm(Sepal.Width ~ Sepal.Length * Species, data = iris)

# Utilizando o model.matrix(), veja como que fica a matriz do modelo.
# Quantas colunas existem?

#--------------------------------------------------
# Exercício 15: 
# Consultando o summary(modelo_iris) e usando geom_abline(), desenhe 
# no gráfico acima uma reta que coincida com a reta veremelha da setosa. 
# Repita o procedimento para versicolor e virginica.


#--------------------------------------------------
# Exercício 16: 
# Interprete os parâmetros.


#--------------------------------------------------
# Exercício 17: 
# Dentro do mutate(), use a função fct_relevel() para colocar versicolor 
# como o nível de referência da variável Species.
# Ajuste o modelo novamente e veja se algum termo ficou sem valor-p alto.


#--------------------------------------------------
# Exercício 18: 
# Considerando apenas as colunas  Limit, Age e Rating da tabela Credit:
# 1) ajuste uma regressã linear para Balance.
# 2) calcule os VIFs dos 3 preditores usando a função vif().


#--------------------------------------------------
# Exercício 19: 
# Considerando apenas as colunas  Limit, Age e Rating da tabela Credit:
# 1) Preencha o lime() com os objetos corretos
# 2) Crie uma explicacao com o explain() para as duas primeiras linhas da tabela Boston
# 3) visualize com plot_features()
modelo_boston <- train(medv ~ ., data = Boston, method = "lm")

explicador <- lime() # preencha aqui

explicacao <- explain(Boston[1:2, ], explicador, n_features = 5) # Aquijá está OK

plot_features() # preencha aqui





#--------------------------------------------------
# Análise Monster Trader V8
monster_trader <- read_excel("dados/Monster Trader_v8.xlsx", range = "AK2196:AU2548") %>%
  clean_names() %>%
  rename(
    data = x1
  ) %>%
  select(-x10)

glimpse(monster_trader)

mt_lm <- lm(
  delta_real ~ 1
  + x2
  + delta_ena_se
  + delta_ena_s
  + delta_pld_s_1
  + delta_pld_s_2
  + delta_acum_se
  + delta_acum_s
  + delta_ampere
  ,data = monster_trader
)

tidy(mt_lm)

summary(mt_lm)


# outliers ------------------------------------------
monster_trader_outliers <- monster_trader %>%
  mutate(
    outlier = 1:nrow(.) %in% c(57)
  ) %>%
  gather(variavel, valor, -data, -outlier)

monster_trader_outliers  %>%
  ggplot(aes(x = valor)) +
  geom_density(alpha = 0.1) +
  geom_jitter(aes(y = 1), alpha = 0.05) +
  geom_jitter(aes(y = 1), colour = "red", size = 3, data = monster_trader_outliers %>% filter(outlier)) +
  facet_wrap(~variavel, scale = "free")

