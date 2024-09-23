library(tidyverse)
library(readxl)
library(stringr)

base_teto_raw <- read_excel("./dados/base-teto-2023.xlsx", sheet = "2023")

base_teto_raw %>% select(CATEGORIA_RTN) %>% filter(stringr::str_detect(CATEGORIA_RTN, "recatório")) %>% unique()


base_justicas_estaduais <- read_excel("./dados/Planilha_1841031_Relacao_PLOA_2025.xlsx", skip = 1)

base_justicas_estaduais$`Número da ação originária no padrão estabelecido pelo CNJ` %>% unique() %>% length()

lista_processos <- 
  str_replace_all(
    base_justicas_estaduais$`Número da ação originária no padrão estabelecido pelo CNJ`[1:10],
    "[-.]", 
    ""
  )

