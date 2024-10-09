library(tidyverse)
library(readxl)

dados_api <-read_rds("dados-api-todos-tribunais.rds")
base_justicas_estaduais <- read_excel("./dados/Planilha_1841031_Relacao_PLOA_2025.xlsx", skip = 1)

base_justicas_estaduais %>% colnames()

dados_tempos <- base_justicas_estaduais %>%
  mutate(
    t1 = `Data trânsito em julgado da sentença ou do acórdão lavrado na fase de conhecimento do processo judicial;\r\n\r\n(dd/mm/aaaa)` - `Data do ajuizamento da ação originária.\r\n\r\n(dd/mm/aaaa)`,
    t2 = `Data trânsito em julgado (dos embargos à execução, da decisão que resolveu a impugnação ao cálculo no cumprimento de sentença ou do decurso do prazo para sua apresentação ou, se for o caso, da decisão que reconheceu parcela incontroversa)\r\n\r\n(dd/mm/aaaa)` - `Data trânsito em julgado da sentença ou do acórdão lavrado na fase de conhecimento do processo judicial;\r\n\r\n(dd/mm/aaaa)`,
    t3 = `Data de autuação do precatório.\r\n\r\n(dd/mm/aaaa)` - `Data trânsito em julgado (dos embargos à execução, da decisão que resolveu a impugnação ao cálculo no cumprimento de sentença ou do decurso do prazo para sua apresentação ou, se for o caso, da decisão que reconheceu parcela incontroversa)\r\n\r\n(dd/mm/aaaa)`
  ) %>%
  select(
    assunto = `Código do Tipo de Causa Julgada (de acordo com a Tabela Única de Assuntos do CNJ – TUA)\r\nConsulte a tabela em www.cnj.jus.br/assuntos`,
    t1, t2, t3,
    Tribunal,
    valor = `Valor individualizado por beneficiário, atualizado até 2 de abril.\r\n\r\n(EX.: Proposta Orçamentária para 2025, valor atualizado em 2 de abril de 2024).\r\n\r\nUtilize a vírgula para separador de centavos`
  ) %>%
  mutate(
    across(c("t1", "t2", "t3"), .fns = ~ as.numeric(.x/(60*60*24)))
  )

principais_assuntos <- dados_tempos %>% 
  group_by(assunto) %>%
  summarise(v = sum(valor)) %>%
  arrange(desc(v))

paretto <- principais_assuntos %>%
  mutate(
    acum = cumsum(v),
    total_restante = sum(v) - cumsum(v),
    total = sum(v),
    pct_valor = scales::percent(v / total),
    pct_restante = scales::percent(total_restante / total)
    )

sumario <- principais_assuntos %>%
  mutate(
    cat = ifelse(row_number() <= 5, assunto, "Demais")
  ) %>%
  group_by(cat) %>%
  summarise(v = sum(v))

ggplot(sumario, aes(x = v, y = reorder(cat, v))) + geom_col()

ggplot(principais_assuntos, aes(x = v, y = reorder(assunto, v))) + geom_col()

medias <- dados_tempos %>%
  group_by(assunto) %>%
  summarize(
    across(c("t1", "t2", "t3"), .fns = ~ mean(.x))
  )

medias$assunto %>% dput()
