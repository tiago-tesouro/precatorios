library(httr)
library(tidyverse)

#Substituir <API Key> pela Chave Pública
headers <- c(
  'Authorization' = 'ApiKey cDZHYzlZa0JadVREZDJCendQbXY6SkJlTzNjLV9TRENyQk1RdnFKZGRQdw==',
  'Content-Type' = 'application/json'
)

body <- '{
  "size": 100,
  "query": {
        "bool": {
            "must": [
                {"match": {"assuntos.codigo":  10672}}
            ]
        }
   }
}';

res <- VERB("POST", url = "https://api-publica.datajud.cnj.jus.br/api_publica_tjac/_search", body = body, add_headers(headers))

cat(content(res, 'text'))

t <- jsonlite::fromJSON(content(res, 'text'))

a <- t$hits$hits

# Para um processo:

body <- '{
  "size": 100,
  "query": {
    "match": {
      "numeroProcesso":  "00176617820068010001"
      }
   }
}';

# Para vários processos: 

# Convert the list to a JSON array format
process_values <- paste0('"', lista_processos, '"', collapse = ", ")
# lista_processos veio de analise.R

# Write the query body using the `terms` query
body <- paste0('{
  "size": 100,
  "query": {
    "terms": {
      "numeroProcesso": [', process_values, ']
    }
  }
}')



#número do processo original 00176617820068010001

res <- VERB("POST", url = "https://api-publica.datajud.cnj.jus.br/api_publica_tjal/_search", body = body, add_headers(headers))

cat(content(res, 'text'))

t <- jsonlite::fromJSON(content(res, 'text'))

a <- t$hits$hits$`_source`


#https://api-publica.datajud.cnj.jus.br/api_publica_tjdft/_search

