library(tidyverse)
library(readxl)
library(stringr)

base_teto_raw <- readxl::read_excel("./dados/base-teto-2023.xlsx", sheet = "2023")

base_precatorios <- base_teto_raw %>%
  filter(ID_PROGRAMA_PT == "0901")

tbl_base_precatorio <- base_precatorios %>%
  mutate(
    class = ifelse(
      NO_GRUPO_DESPESA_NADE == "PESSOAL E ENCARGOS SOCIAIS", "Pessoal",
      ifelse(
        UNIDADE_ORCAMENTARIA_CODIGO %in% c("33904", "40904"), "Benefícios Previdenciários", 
        ifelse(
          UNIDADE_ORCAMENTARIA_CODIGO == "55901", "LOAS/RMV", "Demais"
        )
      )
    )
  )

tbl_base_precatorio %>% 
  group_by(class) %>% 
  summarise(loa = sum(DOTACAO_INICIAL), pag = sum(DESPESAS_PAGAS)) %>% 
  janitor::adorn_totals()

base_precatorios %>% group_by(UNIDADE_ORCAMENTARIA_DESCRICAO) %>% summarize(dot = sum(DOTACAO_INICIAL)) %>% arrange(desc(dot))
