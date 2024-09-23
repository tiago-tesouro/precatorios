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
parsed <- jsonlite::fromJSON(content(res, 'text'))
