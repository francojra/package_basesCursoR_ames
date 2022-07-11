# Base de dados - Curso R ------------------------------------------------------------------------------------------------------------------
# Autoria do script: Jeanne Franco ---------------------------------------------------------------------------------------------------------
# Data: 11/07/22 ---------------------------------------------------------------------------------------------------------------------------

# Carregar pacotes ----------------------------------------------------------------------------------------------------------------------------

library(basesCursoR)
library(dplyr)
library(ggplot2)
library(magrittr)
library(tidyr)
library(forcats)
library(gridExtra)

# Identificar bases disponíveis ------------------------------------------------------------------------------------------------------------

basesCursoR::bases_disponiveis()

# Carregar base de dados -------------------------------------------------------------------------------------------------------------------

ames <- basesCursoR::pegar_base("ames")
View(ames)

# Selecionar dados -------------------------------------------------------------------------------------------------------------------------

ames1 <- ames %>%
  select(lote_area, rua_tipo, geral_qualidade, geral_condicao, 
         exterior_qualidade, alvenaria_area) 
View(ames1)  
glimpse(ames1)

ames1$rua_tipo <- as.factor(ames1$rua_tipo)
ames1$exterior_qualidade <- as.factor(ames1$exterior_qualidade)

# Análises ---------------------------------------------------------------------------------------------------------------------------------

ames2 <- ames1 %>%
  group_by(rua_tipo) %>%
  summarise(med = mean(lote_area),
            sd = sd(lote_area),n = n(),
            se = sd/sqrt(n)) %>%
  drop_na()
View(ames2)

p1 <- ggplot(ames2, aes(x = fct_reorder(rua_tipo, med), y = med)) +
  geom_col(fill = "#984ea3", color = "black") +
  geom_errorbar(aes(x = rua_tipo, y = med, ymin = med - se,
                    ymax = med + se), width = 0.2, size = 0.9) +
  labs(x = "Tipo de rua", y = "Área do lote")
p1

ames3 <- ames1 %>%
  group_by(exterior_qualidade) %>%
  summarise(med = mean(geral_condicao),
            sd = sd(geral_condicao),n = n(),
            se = sd/sqrt(n)) %>%
  drop_na()
View(ames3)

p2 <- ggplot(ames3, aes(x = fct_reorder(exterior_qualidade, med), y = med)) +
  geom_col(fill = "#ff7f00", color = "black") +
  geom_errorbar(aes(x = exterior_qualidade, y = med, ymin = med - se,
                    ymax = med + se), width = 0.2, size = 0.9) +
  labs(x = "Qualidade do exterior", y = "Condição geral")
p2


p3 <- ggplot(ames1, aes(x = alvenaria_area, y = geral_qualidade)) +
  geom_point(size = 5, alpha = 0.7) +
  geom_smooth(method = "lm", color = "black") +
  labs(x = "Área da alvenaria", y = "Qualidade geral")
p3

ames4 <- ames1 %>%
  group_by(rua_tipo) %>%
  summarise(med = mean(geral_qualidade),
            sd = sd(geral_qualidade), n = n(),
            se = sd/sqrt(n)) %>%
  drop_na()
View(ames4)

p4 <- ggplot(ames4, aes(x = fct_reorder(rua_tipo, med), y = med)) +
  geom_col(fill = "#e41a1c", color = "black") +
  geom_errorbar(aes(x = rua_tipo, y = med, ymin = med - se,
                    ymax = med + se), width = 0.2, size = 0.9) +
  labs(x = "Tipo de rua", y = "Qualidade geral")
p4

grid.arrange(p1, p2, p3, p4)