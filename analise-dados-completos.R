library(tidyverse)
library(readxl)
library(extrafont)
library(ggbeeswarm)
library(ggridges)
loadfonts()

dados_api <-read_rds("dados-api-todos-tribunais.rds")
base_justicas_estaduais <- read_excel("./dados/Planilha_1841031_Relacao_PLOA_2025.xlsx", skip = 1)

base_justicas_estaduais %>% colnames()

dados_tempos <- base_justicas_estaduais %>%
  rename(
    d1 = `Data do ajuizamento da ação originária.\r\n\r\n(dd/mm/aaaa)`,
    d2 = `Data trânsito em julgado da sentença ou do acórdão lavrado na fase de conhecimento do processo judicial;\r\n\r\n(dd/mm/aaaa)`,
    d3 = `Data trânsito em julgado (dos embargos à execução, da decisão que resolveu a impugnação ao cálculo no cumprimento de sentença ou do decurso do prazo para sua apresentação ou, se for o caso, da decisão que reconheceu parcela incontroversa)\r\n\r\n(dd/mm/aaaa)`,
    d4 = `Data de autuação do precatório.\r\n\r\n(dd/mm/aaaa)`
  ) %>%
  mutate(
    t1 = d2-d1,
    t2 = d3-d2,
    t3 = d4-d3
  ) %>%
  select(
    assunto = `Código do Tipo de Causa Julgada (de acordo com a Tabela Única de Assuntos do CNJ – TUA)\r\nConsulte a tabela em www.cnj.jus.br/assuntos`,
    d1,d2,d3,d4,t1, t2, t3,
    Tribunal,
    valor = `Valor individualizado por beneficiário, atualizado até 2 de abril.\r\n\r\n(EX.: Proposta Orçamentária para 2025, valor atualizado em 2 de abril de 2024).\r\n\r\nUtilize a vírgula para separador de centavos`
  ) %>%
  mutate(
    across(c("t1", "t2", "t3"), .fns = ~ as.numeric(.x/(60*60*24)))
  )

principais_assuntos <- dados_tempos %>% 
  group_by(assunto) %>%
  summarise(v = sum(valor)) %>%
  arrange(desc(v)) #%>%
  # ungroup() %>%
  # mutate(v = format(v, big.mark = ".", decimal.mark = ",", scientific = FALSE))
  
principal_assunto <- principais_assuntos$assunto[1]

paretto <- principais_assuntos %>%
  mutate(
    acum = cumsum(v),
    total_restante = sum(v) - cumsum(v),
    total = sum(v),
    pct_acum = scales::percent(acum / total),
    pct_valor = scales::percent(v / total),
    pct_restante = scales::percent(total_restante / total)
    )

sumario <- principais_assuntos %>%
  mutate(
    cat = ifelse(row_number() <= 6, assunto, "Demais")
  ) %>%
  group_by(cat) %>%
  summarise(v = sum(v))

top_assuntos <- paretto %>%
  filter(row_number() <= 6) %>%
  mutate(assunto = forcats::fct_reorder(assunto, acum, .desc = TRUE)) %>%
  select(assunto, acum, total_restante) %>%
  gather("key", "v", -assunto) %>%
  mutate(key = factor(key, levels = c("total_restante", "acum")))

lista_assuntos <- top_assuntos <- paretto %>%
  filter(row_number() <= 6) %>% .$assunto
  
ggplot(top_assuntos, aes(x = v, fill = key, y = assunto)) + 
  geom_col(width = .5) +
  scale_fill_manual(values = c("acum" = "hotpink", "total_restante" = "gray"), labels = c("acum" = "Valor", "total_restante" = "Demais")) +
  scale_x_continuous(labels = scales::label_number(scale = 1/1e9, suffix = " tri")) +
  labs(x = NULL, y = NULL, title = "Valores acumulados dos principais assuntos") +
  theme_minimal() +
  theme(
    element_text(family = "Georgia"),
    legend.position = "none",
    panel.grid.minor.x = element_blank()
  )

ggplot(principais_assuntos, aes(x = v, y = reorder(assunto, v))) + geom_col()

medias <- dados_tempos %>%
  group_by(assunto) %>%
  summarize(
    across(c("t1", "t2", "t3"), .fns = ~ mean(.x))
  )

medias$assunto %>% dput()


# Anos --------------------------------------------------------------------

dados_principais_assuntos <- dados_tempos %>%
  mutate(
    assunto = ifelse(assunto %in% lista_assuntos, assunto, "Demais")
  ) %>%
  mutate(
    across(
      .cols = starts_with("d"),
      .fns = ~ year(.x),
      .names = "a{substr(.col, 2, 2)}")
    ) %>%
  mutate(
    dtm = interval(d1, d4) / months(1),
    dty = interval(d1, d4) / years(1)
  )

ggplot(dados_principais_assuntos) +
  geom_col(aes(x = a4, y = valor)) +
  facet_wrap(~assunto)

matriz_anos <- dados_principais_assuntos %>%
  group_by(assunto) %>%
  count(a1, a4)

ggplot(matriz_anos, aes(x = a4, y = a1, fill = n)) +
  geom_raster() + 
  colorspace::scale_fill_continuous_sequential() +
  scale_x_continuous(breaks = scales::breaks_pretty(), limits = c(min(matriz_anos$a1), max(matriz_anos$a4))) +
  labs(y = "Ano da ação original", x = "Ano em que se tornou precatório") +
  theme_minimal() +
  facet_wrap(~assunto)

ggplot(
  dados_principais_assuntos %>% count(Tribunal),
  aes(
    x = n, y = reorder(Tribunal,n)
  )
) +
  geom_col(fill = "goldenrod") +
  geom_text(aes(label = format(n, big.mark = ".")), hjust = "left", size = 4, family = "Fira Code", nudge_x = 100) +
  labs(x = NULL, y = NULL) +
  scale_x_continuous(labels = scales::label_number(big.mark = "."), expand = expansion(add = c(0,2000))) +
  labs(title = "Processos por Tribunal") +
  theme_minimal() +
  theme(
    text = element_text(family = "Fira Code", size = 14),
    panel.grid.minor = element_blank()
  )

ggsave("./plots/precatorios-por-tribunal.png", height = 14, width = 10, bg = "white")

ggplot(
  dados_principais_assuntos %>% group_by(Tribunal) %>% summarise(n = sum(valor)),
  aes(
    x = n, y = reorder(Tribunal,n)
  )
) +
  geom_col(fill = "steelblue") +
  geom_text(aes(
    label =  scales::number(n, scale = 1e-6, big.mark = ".", decimal.mark = ",", accuracy = 0.1)),
    hjust = "left", size = 4, family = "Fira Code", nudge_x = 10e6) +
  labs(x = NULL, y = NULL) +
  scale_x_continuous(labels = scales::label_number(scale = 1e-6, big.mark = ".", decimal.mark = ","), expand = expansion(add = c(0,5e8))) +
  labs(title = "Valores por Tribunal (R$ mi)") +
  theme_minimal() +
  theme(
    text = element_text(family = "Fira Code", size = 14),
    panel.grid.minor = element_blank()
  )

ggsave("./plots/precatorios-por-tribunal-valores.png", height = 14, width = 10, bg = "white")


ggplot(dados_principais_assuntos %>% 
         group_by(Tribunal) %>% 
         mutate(
           mediana = median(valor),
           media = mean(valor)) %>% 
         ungroup(), 
       aes(x = valor, y = reorder(Tribunal, media))) + 
  geom_boxplot() +
  geom_point(aes(x = media), color = "firebrick") +
  labs(x = NULL, y = NULL) +
  scale_x_continuous(labels = scales::label_number(scale = 1e-3, big.mark = ".", decimal.mark = ",", suffix = " mil"),
                     limits = c(0,1e6), breaks = seq(0, 1e6, by = 1e5)) +
  labs(title = "Distribuição dos valores dos processos por Tribunal ", subtitle = "até R$ 1 milhão (existem 68 processos com valor superior)") +
  theme_minimal() +
  theme(
    text = element_text(family = "Fira Code", size = 14),
    panel.grid.minor.x = element_line(linetype = "dotted")
  )

ggsave("./plots/precatorios-distribuicao-por-tribunal-valores.png", height = 10, width = 16, bg = "white")



ggplot(
  dados_principais_assuntos %>% 
    group_by(assunto) %>% 
    summarise(n = sum(valor)) %>% 
    ungroup() %>% 
    mutate(assunto = factor(assunto, levels = rev(c(lista_assuntos, "Demais")))),
  aes(
    x = n, y = assunto
  )
) +
  geom_col(fill = "steelblue") +
  geom_text(aes(
    label =  scales::number(n, scale = 1e-6, big.mark = ".", decimal.mark = ",", accuracy = 1)),
    hjust = "left", size = 4, family = "Fira Code", nudge_x = 10e6) +
  labs(x = NULL, y = NULL) +
  scale_x_continuous(labels = scales::label_number(scale = 1e-6, big.mark = ".", decimal.mark = ","), expand = expansion(add = c(0,5e8))) +
  labs(title = "Valores por assunto (R$ mi)") +
  theme_minimal() +
  theme(
    text = element_text(family = "Fira Code", size = 14),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  )

ggsave("./plots/precatorios-por-assuntos-valores.png", height = 6, width = 10, bg = "white")



ggplot(
  dados_principais_assuntos %>% 
    count(assunto) %>%
    mutate(assunto = factor(assunto, levels = rev(c(lista_assuntos, "Demais")))),
  aes(
    x = n, y = assunto
  )
) +
  geom_col(fill = "goldenrod") +
  geom_text(aes(
    label =  scales::number(n, big.mark = ".", decimal.mark = ",", accuracy = 1)),
    hjust = "left", size = 4, family = "Fira Code", nudge_x = 100) +
  labs(x = NULL, y = NULL) +
  scale_x_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ","), expand = expansion(add = c(0,1000))) +
  labs(title = "Quantidade de processos por assunto (R$ mi)") +
  theme_minimal() +
  theme(
    text = element_text(family = "Fira Code", size = 14),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  )

ggsave("./plots/precatorios-por-assuntos-qde.png", height = 6, width = 10, bg = "white")


ggplot(dados_principais_assuntos %>% 
         group_by(assunto) %>% 
         mutate(
           mediana = median(valor),
           media = mean(valor)) %>% 
         ungroup(), 
       aes(x = valor, y = reorder(assunto, media))) + 
  geom_boxplot() +
  geom_point(aes(x = media), color = "firebrick") +
  labs(x = NULL, y = NULL) +
  scale_x_continuous(labels = scales::label_number(scale = 1e-3, big.mark = ".", decimal.mark = ",", suffix = " mil"),
                     limits = c(0,1e6), breaks = seq(0, 1e6, by = 1e5)) +
  labs(title = "Distribuição dos valores dos processos por assunto", subtitle = "até R$ 1 milhão (existem 68 processos com valor superior)") +
  theme_minimal() +
  theme(
    text = element_text(family = "Fira Code", size = 14),
    panel.grid.minor.x = element_line(linetype = "dotted")
  )

ggsave("./plots/precatorios-distribuicao-por-assunto-valores.png", height = 10, width = 16, bg = "white")





ggplot(dados_principais_assuntos) + 
  geom_histogram(aes(x = valor), bins = 50) + 
  labs(x = NULL, y = NULL) +
  facet_wrap(~assunto) +
  theme_bw()




# ggplot(dados_principais_assuntos) +
#   geom_histogram(aes(dty)) +
#   facet_wrap(~assunto)
# 
# ggplot(dados_principais_assuntos, aes(x = dty, y = Tribunal)) + geom_boxplot()
# ggplot(dados_principais_assuntos, aes(x = valor, y = Tribunal)) + geom_boxplot()
# ggplot(dados_principais_assuntos, aes(x = valor, y = Tribunal)) + geom_jitter(color = "steelblue", alpha = 0.2)
# 
# ggplot(dados_principais_assuntos, aes(x = dty, y = Tribunal, size = valor, color = assunto)) +
#   geom_jitter()

ggplot(dados_principais_assuntos, aes(x = dty, y = assunto, color = as.factor(a4))) + 
  geom_quasirandom() + 
  colorspace::scale_color_discrete_sequential(palette = "Oranges") +
  labs(title = "Tempo até o processo se tornar precatório (em anos)", y = NULL, x = NULL) +
  theme_minimal()

dados_beeswarm <- dados_principais_assuntos %>%
  select(d1, dty, assunto, valor) %>%
  group_by(assunto) %>%
  mutate(valor_q75 = quantile(valor, 0.75)) %>%
  ungroup()

ggplot(dados_beeswarm, aes(x = as.Date(d1), y = assunto, color = valor > valor_q75
                                                             #, color = as.factor(a4)
                                                             )) + 
  geom_quasirandom() + 
  #colorspace::scale_color_discrete_sequential(palette = "Oranges") +
  scale_color_manual(values = c("TRUE" = "firebrick", "FALSE" = "steelblue")) +
  labs(title = "Precatórios por data do processo originário", subtitle = "Destaque para os 25% maiores valores", y = NULL, x = NULL) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_minimal() +
  theme(
    legend.position = "none",
    text = element_text(family = "Fira Code"))
  #+ facet_wrap(~a4)

ggsave("./plots/precatorios-por-data-processo-beeswarm.png", bg = "white", height = 8, width = 15)

ggplot(dados_principais_assuntos, aes(x = as.Date(d1), y = assunto, fill = assunto)) +
  geom_density_ridges(scale = 2, rel_min_height = 0.01) +
  labs(title = "Data do processo originário", y = NULL, x = NULL) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  theme(legend.position = "none")

ggplot(dados_principais_assuntos) + 
  geom_histogram(aes(x = a1), binwidth = 1) + 
  labs(x = NULL, y = NULL) +
  facet_wrap(~assunto) +
  theme_bw()


sumario_ano_assunto <- dados_principais_assuntos %>%
  count(assunto, a1) %>%
  group_by(assunto) %>%
  mutate(
    media_n = mean(n),
    mediana_n = median(n),
    q75 = quantile(n, 0.75)) %>%
  ungroup()

ggplot(sumario_ano_assunto, aes(y = n, x = a1, fill = n > q75)) +
  geom_col() +
  scale_fill_manual(values = c("TRUE" = "firebrick", "FALSE" = "steelblue")) +
  labs(x = NULL, y = NULL, title = "Número de processos-autores por ano da ação original", subtitle = "Destaque para os 25% anos com mais processos") +
  facet_wrap(~assunto) +
  theme_bw() +
  theme(
    text = element_text(family = "Fira Code"),
    legend.position = "none"
  )

ggsave("./plots/precatorios-qde-por-ano-histo.png", bg = "white", height = 8, width = 15)



sumario_ano_assunto_valor <- dados_principais_assuntos %>%
  group_by(assunto, a1) %>%
  summarize(valor = sum(valor)) %>%
  ungroup() %>%
  group_by(assunto) %>%
  mutate(
    media_v = mean(valor),
    mediana_v = median(valor),
    q75 = quantile(valor, 0.75)) %>%
  ungroup()

ggplot(sumario_ano_assunto_valor, aes(y = valor, x = a1, fill = valor > q75)) +
  geom_col() +
  scale_fill_manual(values = c("TRUE" = "firebrick", "FALSE" = "steelblue")) +
  scale_y_continuous(labels = scales::label_number(scale = 1/1e6, suffix = " mi")) +
  facet_wrap(~assunto) +
  labs(x = NULL, y = NULL, title = "Valor do estoque atual de precatõrios por ano da ação original", subtitle = "Destaque para os 25% maiores valores") +
  theme_bw() +
  theme(
    text = element_text(family = "Fira Code"),
    legend.position = "none"
  )

ggsave("./plots/precatorios-valor-por-ano-histo.png", bg = "white", height = 8, width = 15)

ggplot(sumario_ano_assunto_valor, aes(y = valor, x = a1, fill = valor > q75)) +
  geom_col() +
  scale_fill_manual(values = c("TRUE" = "firebrick", "FALSE" = "steelblue")) +
  scale_y_continuous(labels = scales::label_number(scale = 1/1e6, suffix = "mi")) +
  facet_wrap(~assunto
             #, scales = "free"
             ) +
  labs(x = NULL, y = NULL, title = "Valor do estoque atual de precatõrios por ano da ação original", subtitle = "Destaque para os 25% maiores valores") +
  theme_bw()

dados_principais_assuntos %>% count(a1) %>% .$n %>% quantile(0.75)

ggplot(dados_principais_assuntos) +
  geom_histogram(aes(x = a1), binwidth = 1, color = "white")

dados_principais_assuntos %>% filter(a4 <2020)

dados_principais_assuntos %>%
  group_by(assunto) %>%
  summarise(media = mean(valor))

medias_valores <- dados_tempos %>%
  group_by(assunto) %>%
  summarise(media = mean(valor), n()) %>%
  arrange(desc(media))

dados_principais_assuntos %>%
  ggplot() + geom_boxplot(aes(x = valor, y = assunto))

summary(dados_principais_assuntos$valor)
