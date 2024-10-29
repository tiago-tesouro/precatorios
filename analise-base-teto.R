library(tidyverse)
library(readxl)
library(stringr)

base_teto_raw <- read_excel("./dados/base-teto-2022.xlsx", sheet = "2022")

base_precatorios <- base_teto_raw %>%
  filter(ID_PROGRAMA_PT == "0901")

tbl_base_precatorio <- base_precatorios %>%
  mutate(
    class = ifelse(
      NO_GRUPO_DESPESA_NADE == "PESSOAL E ENCARGOS SOCIAIS", "Pessoal",
      ifelse(
        UNIDADE_ORCAMENTARIA_CODIGO == "40904", "Benefícios Previdenciários", 
        ifelse(
          UNIDADE_ORCAMENTARIA_CODIGO == "55901", "LOAS/RMV", "Demais"
        )
      )
    )
  )

tbl_base_precatorio %>% group_by(class) %>% summarise(dot = sum(DOTACAO_ATUALIZADA)) %>% arrange(desc(dot))

base_precatorios %>% group_by(UNIDADE_ORCAMENTARIA_DESCRICAO) %>% summarize(dot = sum(DOTACAO_INICIAL)) %>% arrange(desc(dot))
