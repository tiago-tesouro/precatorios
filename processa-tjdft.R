

# Prepara lista de processos que serão pesquisados na API -----------------

base_tjdft <- read_excel("./dados/tjdft-Precatorios_Federais___Orcamento_2025.xlsx")

api_endpoint <- "https://api-publica.datajud.cnj.jus.br/api_publica_tjdft/_search"

lista_processos_df <- base_tjdft %>%
  select(Tribu)
  select(`Nome do Tribunal`, `Número da Ação Originária`) %>%
  mutate(
    processo = str_replace_all(`Número da Ação Originária`,
                               "[-.]", 
                               ""
    )
  ) %>%
  distinct()

lista_processos_df_string <- paste0('"', lista_processos_df$processo, '"', collapse = ", ")


# API Call ----------------------------------------------------------------

headers <- c(
  'Authorization' = 'ApiKey cDZHYzlZa0JadVREZDJCendQbXY6SkJlTzNjLV9TRENyQk1RdnFKZGRQdw==',
  'Content-Type' = 'application/json'
)

body <- paste0('{
    "size": 10000,
    "query": {
      "terms": {
        "numeroProcesso": [', lista_processos_df_string, ']
      }
    }
  }')

res <- VERB("POST", url = api_endpoint, body = body, add_headers(headers))

json_response <- content(res, 'text')

parsed_json <- jsonlite::fromJSON(json_response, simplifyVector = FALSE)

hits <- parsed_json$hits$hits

# testando se em cada hit há algum movimento do tipo código ZZZ (inscrição em precatórios)

# Define the target code
target_code <- 12457

# Filter the list based on whether any movement contains the target code
filtered_hits <- keep(hits, function(obj) {
  any(map_lgl(obj$`_source`$movimentos, ~ .x$codigo == target_code))
})

filtered_hits <- keep(hits, function(obj) {
  any(map_lgl(obj$`_source`$movimentos, ~ str_detect(.x$nome, "precat")))
})

# get_movement_date <- function(hit_content, target_code) {
#   # Find the movement where the code matches the target code
#   movimento_de_inscricao_em_precatorio <- keep(hit_content$movimentos, ~ .x$code == target_code)
#   
#   # If a match is found, extract the date, otherwise return NA
#   if (length(movimento_de_inscricao_em_precatorio) > 0) {
#     return(movimento_de_inscricao_em_precatorio[[1]]$dataHora)
#   } else {
#     return(NA)
#   }
# }

get_movement_date <- function(hit_content, target_code) {
  
  # Find the movement where the code matches the target code

  for (mov in hit_content$movimentos) {
    if (mov$codigo == target_code) {
      return (mov$dataHora)
    }
  }
  
  return (NA)
  
}


processa_hit <- function(processo) {
  
  #nHits <- nrow(parsed_json$hits$hits)
  
  #print(paste0("Nhits: ", nHits))
  
  #if (nHits > 0) {
  linha <- data.frame(numeroProcesso = c(NA))
  
  linha$numeroProcesso <- processo$numeroProcesso
  
  linha$cod_classe <- as.character(processo$classe$codigo)
  #print(paste("Processamento do hit ", class(linha$cod_classe)))
  
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
          linha[1,paste0("nome_assunto",i)] <- ifelse(
            is.null(processo$assuntos[[i]]$nome),
            "NULL",
            processo$assuntos[[i]]$nome
          )
          
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
  
  target_code <- 12457 #precatorios
  linha$movimento_inscricao_precatorio <- get_movement_date(processo, target_code)
  
  if (!is.null(nMovimentos)) {
    linha$qde_movimentos <- nMovimentos
    linha$dataPrimeiroMovimento <- processo$movimentos[[1]]$dataHora
    linha$dataultimoMovimento <- processo$movimentos[[nMovimentos]]$dataHora
  }
  
  
  return(linha)
  
  #}
  
}

processa_parsed_json <- function(parsed_json) {
  
  nHits <- length(parsed_json$hits$hits)
  
  tabela <- data.frame()
  
  if (nHits > 0) {
    
    print(paste(parsed_json$`_shards`$total, parsed_json$`_shards`$successful, nHits))
    
    for (hit in 1:nHits) {
      
      print(hit)
      
      processo <- parsed_json$hits$hits[[hit]]$`_source`
      linha <- processa_hit(processo)#processa_json(processo)
      #print(class(linha$cod_classe))
      tabela <- bind_rows(tabela, linha)
      
    }
    
  } else {
    
    print("Nada.")
    
  }
  
  return(tabela)
  
}

tab_df <- processa_parsed_json((parsed_json))
