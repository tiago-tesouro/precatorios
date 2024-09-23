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

# # Para vários processos: 
# 
# # Convert the list to a JSON array format
# process_values <- paste0('"', lista_processos_ajuste$processo[1], '"', collapse = ", ")
# # lista_processos veio de analise.R
# 
# # Write the query body using the `terms` query
# body <- paste0('{
#   "size": 100,
#   "query": {
#     "terms": {
#       "numeroProcesso": [', process_values, ']
#     }
#   }
# }')
# 
# 
# 
# #número do processo original 00176617820068010001
# 
# res <- VERB("POST", url = "https://api-publica.datajud.cnj.jus.br/api_publica_tjal/_search", body = body, add_headers(headers))
# 
# write(content(res, 'text'), file = "result.json")
# 
# json_response <- content(res, 'text')
# 
# parsed_json <- jsonlite::fromJSON(json_response)



# PROCESSAMENTO -----------------------------------------------------------

processa_json <- function(parsed_json, linha) {
  
  nHits <- length(parsed_json$hits$hits)
  
  print(paste0("Nhits: ", nHits))
  
  if (nHits > 0) {
    
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
    
    if (!is.null(nMovimentos)) {
      linha$qde_movimentos <- nMovimentos
      linha$dataPrimeiroMovimento <- processo$movimentos[[1]]$dataHora[[1]]
      linha$dataultimoMovimento <- processo$movimentos[[1]]$dataHora[[nMovimentos]]
    }
  
    
    return(linha)
    
  }
  
  
}

busca_processo <- function(numero_processo, endpoint) {
  #numero_processo <- "21123234"
  #print(c(numero_processo))
  linha <- data.frame(numero_processo_pesquisa = c(numero_processo))
  #print(linha)
  
  headers <- c(
    'Authorization' = 'ApiKey cDZHYzlZa0JadVREZDJCendQbXY6SkJlTzNjLV9TRENyQk1RdnFKZGRQdw==',
    'Content-Type' = 'application/json'
  )
  
  body <- paste0('{
  "size": 100,
  "query": {
    "match": {
      "numeroProcesso": "', numero_processo, '"
    }
  }
}')
  
  #print(body)
  #print(endpoint)
  
  res <- VERB("POST", url = endpoint, body = body, add_headers(headers))
  
  json_response <- content(res, 'text')
  
  #cat(json_response)
  
  parsed_json <- jsonlite::fromJSON(json_response)
  
  print(paste(numero_processo, endpoint, parsed_json$`_shards`$total, parsed_json$`_shards`$successful, length(parsed_json$hits$hits)))
  
  linha <- processa_json(parsed_json, linha)
  
  return(linha)
  
}

# # testando a funcao
# a <- busca_processo(lista_processos_ajuste$processo[1], lista_processos_ajuste$Url[1])


params_lista_processos <- lista_processos_ajuste[1:10,] %>%
  select(numero_processo = processo,
         endpoint = Url)

results_list <- purrr::pmap(params_lista_processos, busca_processo)




#https://api-publica.datajud.cnj.jus.br/api_publica_tjdft/_search

a <- processa_json(parsed_json)


