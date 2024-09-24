lista_processos_ajuste %>% count(Tribunal) %>% arrange(n) %>% print(n = 30)
lista_nomes_tribunais <- lista_processos_ajuste %>% count(Tribunal) %>% arrange(n) %>% select(Tribunal) %>% unlist()

# funcoes definidas em api-datajud.R
# Organizar isso depois.

#AL

lista_al <- lista_processos_ajuste %>% filter(Tribunal == "Tribunal de Justiça do Estado de Alagoas")
lista_processos_api <- paste0('"', lista_al$processo, '"', collapse = ", ")
tab <- busca_processo(lista_processos_api, lista_al$Url[1])
write_rds(tab, "./processed/dados-al.rds")


# Funcao para processar cada estado ---------------------------------------

processa_estado <- function(nome_tribunal, sigla) {
  
  lista_processos_estado <- lista_processos_ajuste %>% filter(Tribunal == nome_tribunal)
  lista_processos_estado_string <- paste0('"', lista_processos_estado$processo, '"', collapse = ", ")
  tab <- busca_processo(lista_processos_estado_string, lista_processos_estado$Url[1])
  write_rds(tab, paste0("./processed/dados-", sigla, ".rds"))
  
}

# chama funcao ------------------------------------------------------------

processa_estado(lista_nomes_tribunais[1], "ac")
processa_estado(lista_nomes_tribunais[2], "ap")
processa_estado(lista_nomes_tribunais[3], "pi")
processa_estado(lista_nomes_tribunais[4], "al")
processa_estado(lista_nomes_tribunais[5], "rr")

