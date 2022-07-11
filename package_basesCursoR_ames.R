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

p1 <- ggplot(cred2, aes(x = fct_reorder(estado_civil, med), y = med)) +
  geom_col(fill = "#984ea3", color = "black") +
  geom_errorbar(aes(x = estado_civil, y = med, ymin = med - se,
                    ymax = med + se), width = 0.2, size = 0.9) +
  labs(x = "Estado Civil", y = "Despesas")
p1

cred3 <- cred1 %>%
  group_by(rua_tipo) %>%
  summarise(med = mean(despesas),
            sd = sd(despesas),n = n(),
            se = sd/sqrt(n)) %>%
  drop_na()
View(cred3)

p2 <- ggplot(cred3, aes(x = fct_reorder(trabalho, med), y = med)) +
  geom_col(fill = "#ff7f00", color = "black") +
  geom_errorbar(aes(x = trabalho, y = med, ymin = med - se,
                    ymax = med + se), width = 0.2, size = 0.9) +
  labs(x = "Trabalho", y = "Despesas")
p2

cred4 <- cred1 %>%
  group_by(estado_civil) %>%
  summarise(med = mean(renda, na.rm = T),
            sd = sd(renda, na.rm = T),n = n(),
            se = sd/sqrt(n)) %>%
  drop_na()
View(cred4)

p3 <- ggplot(cred4, aes(x = fct_reorder(estado_civil, med), y = med)) +
  geom_col(fill = "#377eb8", color = "black") +
  geom_errorbar(aes(x = estado_civil, y = med, ymin = med - se,
                    ymax = med + se), width = 0.2, size = 0.9) +
  labs(x = "Estado Civil", y = "Renda")
p3

cred5 <- cred1 %>%
  group_by(trabalho) %>%
  summarise(med = mean(renda, na.rm = T),
            sd = sd(renda, na.rm = T), n = n(),
            se = sd/sqrt(n)) %>%
  drop_na()
View(cred5)

p4 <- ggplot(cred5, aes(x = fct_reorder(trabalho, med), y = med)) +
  geom_col(fill = "#e41a1c", color = "black") +
  geom_errorbar(aes(x = trabalho, y = med, ymin = med - se,
                    ymax = med + se), width = 0.2, size = 0.9) +
  labs(x = "Trabalho", y = "Renda")
p4

grid.arrange(p1, p2, p3, p4)
