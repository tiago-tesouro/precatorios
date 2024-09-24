headers <- c(
  'Authorization' = 'ApiKey cDZHYzlZa0JadVREZDJCendQbXY6SkJlTzNjLV9TRENyQk1RdnFKZGRQdw==',
  'Content-Type' = 'application/json'
)

body <- paste0('{
  "size": 100,
  "query": {
    "match": {
      "numeroProcesso":  "', lista_processos_ajuste$processo[3] ,'"
      }
   }
}')

res <- VERB("POST", url = lista_processos_ajuste$Url[3], body = body, add_headers(headers))
write(content(res, 'text'), file = "result.json")
parsed <- jsonlite::fromJSON(content(res, 'text'), simplifyVector = FALSE)


lista_al <- lista_processos_ajuste %>% filter(Tribunal == "Tribunal de Justiça do Estado de Alagoas")

lista_processos_api <- paste0('"', lista_al$processo, '"', collapse = ", ")

tab <- busca_processo(lista_processos_api, lista_al$Url[1])

#l <- processa_json(parsed$hits$hits[[1]]$`_source`)
