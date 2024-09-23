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

processa_json <- function(processo, linha, hit) {
  
  #nHits <- nrow(parsed_json$hits$hits)
  
  #print(paste0("Nhits: ", nHits))
  
  #if (nHits > 0) {
  
  linha$numeroProcesso <- processo$numeroProcesso[hit]
  
  linha$cod_classe <- processo$classe$codigo[hit]
  linha$nome_classe <- processo$classe$nome[hit]
  linha$grau <- processo$grau[hit]
  linha$orgao_julgador_codigo <- processo$orgaoJulgador$codigo[hit]
  linha$orgao_julgador_nome <- processo$orgaoJulgador$nome[hit]
  
  linha$tribunal <- processo$tribunal[hit]
  linha$data_ultima_atualizacao <- processo$dataHoraUltimaAtualizacao[hit]
  linha$data_ajuizamento <- processo$dataAjuizamento[hit]
  
  nAssuntos <- length(processo$assuntos[[hit]])
  for (i in 1:nAssuntos) {
    
    linha[1,paste0("cod_assunto",i)] <- processo$assuntos[[hit]]$codigo[i]
    linha[1,paste0("nome_assunto",i)] <- processo$assuntos[[hit]]$nome[i]
    
  }
  
  assuntos <- sapply(processo$assuntos[[hit]], function(x) x$nome)  # Extract all "nome" fields
  linha$assuntos <- paste(assuntos, collapse = ", ")
  
  nMovimentos <- nrow(processo$movimentos[[hit]])
  
  if (!is.null(nMovimentos)) {
    linha$qde_movimentos <- nMovimentos
    linha$dataPrimeiroMovimento <- processo$movimentos[[hit]]$dataHora[[1]]
    linha$dataultimoMovimento <- processo$movimentos[[hit]]$dataHora[[nMovimentos]]
  }

  
  return(linha)
    
  #}
  
  
}

busca_processo <- function(numero_processo, endpoint) {
  #numero_processo <- "21123234"
  #print(c(numero_processo))
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
  
  nHits <- nrow(parsed_json$hits$hits)
  
  if (nHits > 0) {
    
    tabela <- data.frame(numero_processo_pesquisa = rep(numero_processo, nHits))
    
    print(paste(numero_processo, endpoint, parsed_json$`_shards`$total, parsed_json$`_shards`$successful, nHits))
    
    processo <- parsed_json$hits$hits$`_source`
    
    for (hit in 1:nHits) {
      linha <- tabela[hit,]
      
      tabela[hit,] <- processa_json(processo, linha, hit)
    }
    
  } else {
    
    tabela <- data.frame(numero_processo_pesquisa = c(numero_processo))
    
  }
  
  return(tabela)
  
}

# # testando a funcao
# a <- busca_processo(lista_processos_ajuste$processo[1], lista_processos_ajuste$Url[1])


params_lista_processos <- lista_processos_ajuste[1:10,] %>%
  select(numero_processo = processo,
         endpoint = Url)

results_list <- purrr::pmap(params_lista_processos, busca_processo)




#https://api-publica.datajud.cnj.jus.br/api_publica_tjdft/_search

a <- processa_json(parsed_json)


