library(tidyverse)
library(readxl)
library(stringr)

# base_teto_raw <- read_excel("./dados/base-teto-2023.xlsx", sheet = "2023")
# 
# base_teto_raw %>% select(CATEGORIA_RTN) %>% filter(stringr::str_detect(CATEGORIA_RTN, "recatório")) %>% unique()


base_justicas_estaduais <- read_excel("./dados/Planilha_1841031_Relacao_PLOA_2025.xlsx", skip = 1)

base_justicas_estaduais$`Número da ação originária no padrão estabelecido pelo CNJ` %>% unique() %>% length()
  
# da lista que está aqui: https://datajud-wiki.cnj.jus.br/api-publica/endpoints 
datapasta::df_paste()
tribunais <- data.frame(
  stringsAsFactors = FALSE,
          Tribunal = c("Tribunal de Justiça do Acre",
                       "Tribunal de Justiça de Alagoas",
                       "Tribunal de Justiça do Amazonas","Tribunal de Justiça do Amapá",
                       "Tribunal de Justiça da Bahia","Tribunal de Justiça do Ceará",
                       "TJ do Distrito Federal e Territórios",
                       "Tribunal de Justiça do Espírito Santo","Tribunal de Justiça de Goiás",
                       "Tribunal de Justiça do Maranhão",
                       "Tribunal de Justiça de Minas Gerais","TJ de Mato Grosso do Sul",
                       "Tribunal de Justiça de Mato Grosso","Tribunal de Justiça do Pará",
                       "Tribunal de Justiça da Paraíba",
                       "Tribunal de Justiça de Pernambuco","Tribunal de Justiça do Piauí",
                       "Tribunal de Justiça do Paraná","Tribunal de Justiça do Rio de Janeiro",
                       "TJ do Rio Grande do Norte",
                       "Tribunal de Justiça de Rondônia","Tribunal de Justiça de Roraima",
                       "Tribunal de Justiça do Rio Grande do Sul",
                       "Tribunal de Justiça de Santa Catarina","Tribunal de Justiça de Sergipe",
                       "Tribunal de Justiça de São Paulo",
                       "Tribunal de Justiça de Tocantins"),
               Url = c("https://api-publica.datajud.cnj.jus.br/api_publica_tjac/_search",
                       "https://api-publica.datajud.cnj.jus.br/api_publica_tjal/_search",
                       "https://api-publica.datajud.cnj.jus.br/api_publica_tjam/_search",
                       "https://api-publica.datajud.cnj.jus.br/api_publica_tjap/_search",
                       "https://api-publica.datajud.cnj.jus.br/api_publica_tjba/_search",
                       "https://api-publica.datajud.cnj.jus.br/api_publica_tjce/_search",
                       "https://api-publica.datajud.cnj.jus.br/api_publica_tjdft/_search",
                       "https://api-publica.datajud.cnj.jus.br/api_publica_tjes/_search",
                       "https://api-publica.datajud.cnj.jus.br/api_publica_tjgo/_search",
                       "https://api-publica.datajud.cnj.jus.br/api_publica_tjma/_search",
                       "https://api-publica.datajud.cnj.jus.br/api_publica_tjmg/_search",
                       "https://api-publica.datajud.cnj.jus.br/api_publica_tjms/_search",
                       "https://api-publica.datajud.cnj.jus.br/api_publica_tjmt/_search",
                       "https://api-publica.datajud.cnj.jus.br/api_publica_tjpa/_search",
                       "https://api-publica.datajud.cnj.jus.br/api_publica_tjpb/_search",
                       "https://api-publica.datajud.cnj.jus.br/api_publica_tjpe/_search",
                       "https://api-publica.datajud.cnj.jus.br/api_publica_tjpi/_search",
                       "https://api-publica.datajud.cnj.jus.br/api_publica_tjpr/_search",
                       "https://api-publica.datajud.cnj.jus.br/api_publica_tjrj/_search",
                       "https://api-publica.datajud.cnj.jus.br/api_publica_tjrn/_search",
                       "https://api-publica.datajud.cnj.jus.br/api_publica_tjro/_search",
                       "https://api-publica.datajud.cnj.jus.br/api_publica_tjrr/_search",
                       "https://api-publica.datajud.cnj.jus.br/api_publica_tjrs/_search",
                       "https://api-publica.datajud.cnj.jus.br/api_publica_tjsc/_search",
                       "https://api-publica.datajud.cnj.jus.br/api_publica_tjse/_search",
                       "https://api-publica.datajud.cnj.jus.br/api_publica_tjsp/_search",
                       "https://api-publica.datajud.cnj.jus.br/api_publica_tjto/_search")
)

tribunais <- tribunais %>%
  mutate(
    Tribunal = str_replace(
      Tribunal,
      "Tribunal de Justiça",
      "Tribunal de Justiça do Estado"
    ),
    
    Tribunal = str_replace(
      Tribunal,
      "TJ",
      "Tribunal de Justiça do Estado"
    ),
    
    
  )

lista_processos <- base_justicas_estaduais %>%
  select(Tribunal, `Número da ação originária no padrão estabelecido pelo CNJ`) %>%
  left_join(tribunais)

lista_processos %>% filter(is.na(Url))

lista_processos_ajuste <- lista_processos %>%
  mutate(
    processo = str_replace_all(`Número da ação originária no padrão estabelecido pelo CNJ`,
    "[-.]", 
    ""
    )
  ) %>%
  distinct()
