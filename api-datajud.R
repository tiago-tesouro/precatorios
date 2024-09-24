library(httr)
library(tidyverse)


# Exploration -------------------------------------------------------------

# #Substituir <API Key> pela Chave Pública
# headers <- c(
#   'Authorization' = 'ApiKey cDZHYzlZa0JadVREZDJCendQbXY6SkJlTzNjLV9TRENyQk1RdnFKZGRQdw==',
#   'Content-Type' = 'application/json'
# )
# 
# body <- '{
#   "size": 100,
#   "query": {
#         "bool": {
#             "must": [
#                 {"match": {"assuntos.codigo":  10672}}
#             ]
#         }
#    }
# }';
# 
# res <- VERB("POST", url = "https://api-publica.datajud.cnj.jus.br/api_publica_tjac/_search", body = body, add_headers(headers))
# 
# cat(content(res, 'text'))
# 
# t <- jsonlite::fromJSON(content(res, 'text'))
# a <- t$hits$hits

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

processa_json <- function(processo) {
  
  #nHits <- nrow(parsed_json$hits$hits)
  
  #print(paste0("Nhits: ", nHits))
  
  #if (nHits > 0) {
  linha <- data.frame(numeroProcesso = c(NA))
  
  linha$numeroProcesso <- processo$numeroProcesso
  
  linha$cod_classe <- processo$classe$codigo
  
  linha$nome_classe <- processo$classe$nome
  linha$formato <- processo$formato$nome
  linha$grau <- processo$grau
  linha$orgao_julgador_codigo <- processo$orgaoJulgador$codigo
  linha$orgao_julgador_nome <- processo$orgaoJulgador$nome
  
  linha$tribunal <- processo$tribunal
  linha$data_ultima_atualizacao <- processo$dataHoraUltimaAtualizacao
  linha$data_ajuizamento <- processo$dataAjuizamento
  
  nAssuntos <- length(processo$assuntos)
  
  if (!is.null(nAssuntos) & nAssuntos > 0) {
    
    tryCatch(
      
      {
        for (i in 1:nAssuntos) {
          
          linha[1,paste0("cod_assunto",i)] <- as.character(processo$assuntos[[i]]$codigo)
          linha[1,paste0("nome_assunto",i)] <- processo$assuntos[[i]]$nome
          
        }
        
        assuntos <- sapply(processo$assuntos, function(x) x$nome)  # Extract all "nome" fields
        linha$assuntos <- paste(assuntos, collapse = ", ")
        
        NA
        
      },
      
      error = function(cond) {
        print("Erro nos assuntos")
        NA
      }
      
    )
    
  }
  
  nMovimentos <- length(processo$movimentos)
  
  if (!is.null(nMovimentos)) {
    linha$qde_movimentos <- nMovimentos
    linha$dataPrimeiroMovimento <- processo$movimentos[[1]]$dataHora
    linha$dataultimoMovimento <- processo$movimentos[[nMovimentos]]$dataHora
  }

  
  return(linha)
    
  #}
  
  
}

busca_processo <- function(numeros_processos, endpoint) {
  #numero_processo <- "21123234"
  #print(c(numero_processo))
  #print(linha)
  
  headers <- c(
    'Authorization' = 'ApiKey cDZHYzlZa0JadVREZDJCendQbXY6SkJlTzNjLV9TRENyQk1RdnFKZGRQdw==',
    'Content-Type' = 'application/json'
  )
  
  # body <- paste0('{
  #   "size": 100,
  #   "query": {
  #     "match": {
  #       "numeroProcesso": "', numero_processo, '"
  #     }
  #   }
  # }')
  
  body <- paste0('{
    "size": 10000,
    "query": {
      "terms": {
        "numeroProcesso": [', numeros_processos, ']
      }
    }
  }')
  
  #print(body)
  #print(endpoint)
  
  res <- VERB("POST", url = endpoint, body = body, add_headers(headers))
  
  json_response <- content(res, 'text')
  
  #cat(json_response)
  
  parsed_json <- jsonlite::fromJSON(json_response, simplifyVector = FALSE)
  
  nHits <- length(parsed_json$hits$hits)
  
  tabela <- data.frame()
  
  if (nHits > 0) {
    
    print(paste(parsed_json$`_shards`$total, parsed_json$`_shards`$successful, nHits))
    
    for (hit in 1:nHits) {
      
      print(hit)
      
      processo <- parsed_json$hits$hits[[hit]]$`_source`
      linha <- processa_json(processo)
      tabela <- bind_rows(tabela, linha)
      
    }
    
  } else {
    
    print("Nada.")
    
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


