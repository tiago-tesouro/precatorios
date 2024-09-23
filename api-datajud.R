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
process_values <- paste0('"', lista_processos_ajuste$processo[2], '"', collapse = ", ")
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

write(content(res, 'text'), file = "result.json")

json_response <- content(res, 'text')

parsed_json <- jsonlite::fromJSON(json_response)

processa_json <- function(parsed_json) {
  
  nHits <- length(parsed_json$hits$hits)
  
  linha <- data.frame(1)
  
  if (nHits != 0) {
    
    processo <- parsed_json$hits$hits$`_source`
    
    linha$numeroProcesso <- processo$numeroProcesso
    
    linha$cod_classe <- processo$classe$codigo
    linha$nome_classe <- processo$classe$nome
    linha$grau <- processo$grau
    linha$orgao_julgador_codigo <- processo$orgaoJulgador$codigo
    linha$orgao_julgador_nome <- processo$orgaoJulgador$nome
    
    linha$tribunal <- processo$tribunal
    linha$data_ultima_atualizacao <- processo$dataHoraUltimaAtualizacao
    linha$data_ajuizamento <- processo$dataAjuizamento
    
    nAssuntos <- length(processo$assuntos)
    for (i in 1:nAssuntos) {
      
      linha[1,paste0("cod_assunto",i)] <- processo$assuntos[[i]]$codigo
      linha[1,paste0("nome_assunto",i)] <- processo$assuntos[[i]]$nome
      
    }
    
    assuntos <- sapply(processo$assuntos, function(x) x$nome)  # Extract all "nome" fields
    linha$assuntos <- paste(assuntos, collapse = ", ")
    
    nMovimentos <- nrow(processo$movimentos[[1]])
    linha$qde_movimentos <- nMovimentos
    if (nMovimentos > 0) {
      linha$dataPrimeiroMovimento <- processo$movimentos[[1]]$dataHora[[1]]
      linha$dataultimoMovimento <- processo$movimentos[[1]]$dataHora[[nMovimentos]]
    }
  
    
    return(linha)
    
  }
  
  
}



#https://api-publica.datajud.cnj.jus.br/api_publica_tjdft/_search

a <- processa_json(parsed_json)
