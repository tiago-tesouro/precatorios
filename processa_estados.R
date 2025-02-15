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
  lista_processos_estado_string <- paste0(
    '"', 
    lista_processos_estado$processo %>% str_pad(20, side = "left", pad = "0"), 
    '"', 
    collapse = ", ")
  tab <- busca_processo(lista_processos_estado_string, lista_processos_estado$Url[1])
  write_rds(tab, paste0("./processed/dados-", sigla, ".rds"))
  
}

processa_estado_sp <- function(p1, p2, sigla) {
  
  lista_processos_estado <- lista_processos_ajuste %>% filter(Tribunal == "Tribunal de Justiça do Estado de São Paulo") %>% filter(row_number() <= p2 & row_number() > p1)
  lista_processos_estado_string <- paste0('"', lista_processos_estado$processo %>% str_sub(1,20), '"', collapse = ", ")
  tab <- busca_processo(lista_processos_estado_string, lista_processos_estado$Url[1])
  write_rds(tab, paste0("./processed/dados-", sigla, ".rds"))
  
}

# chama funcao ------------------------------------------------------------

processa_estado(lista_nomes_tribunais[1], "ac")
processa_estado(lista_nomes_tribunais[2], "ap")
processa_estado(lista_nomes_tribunais[3], "pi")
processa_estado(lista_nomes_tribunais[4], "al")
processa_estado(lista_nomes_tribunais[5], "rr")
processa_estado(lista_nomes_tribunais[6], "ma")
processa_estado(lista_nomes_tribunais[7], "se")
processa_estado(lista_nomes_tribunais[8], "to")
processa_estado(lista_nomes_tribunais[9], "pa")
processa_estado(lista_nomes_tribunais[10], "ro")
processa_estado(lista_nomes_tribunais[11], "rn")
processa_estado(lista_nomes_tribunais[12], "es")
processa_estado(lista_nomes_tribunais[13], "ce")
processa_estado(lista_nomes_tribunais[14], "am")
processa_estado(lista_nomes_tribunais[15], "pb")
processa_estado(lista_nomes_tribunais[16], "pe") #vazio
processa_estado(lista_nomes_tribunais[17], "go")
processa_estado(lista_nomes_tribunais[18], "ms")
processa_estado(lista_nomes_tribunais[19], "ba")
processa_estado(lista_nomes_tribunais[20], "mt")
processa_estado(lista_nomes_tribunais[21], "mg")
processa_estado(lista_nomes_tribunais[22], "rj")
processa_estado(lista_nomes_tribunais[23], "pr")
processa_estado(lista_nomes_tribunais[24], "rs")
processa_estado(lista_nomes_tribunais[25], "sc")
#processa_estado(lista_nomes_tribunais[26], "sp") vazio
#processa_estado_grande(lista_nomes_tribunais[26], 0, 1000, "sp1")

# PE ----------------------------------------------------------------------


lista_pe <- lista_processos_ajuste %>% filter(Tribunal == "Tribunal de Justiça do Estado de Pernambuco")
lista_processos_api <- paste0('"', lista_pe$processo %>% str_pad(20, side = "left", pad = "0"), '"', collapse = ", ")
tab <- busca_processo(lista_processos_api, lista_pe$Url[1])
write_rds(tab, "./processed/dados-al.rds")

headers <- c(
  'Authorization' = 'ApiKey cDZHYzlZa0JadVREZDJCendQbXY6SkJlTzNjLV9TRENyQk1RdnFKZGRQdw==',
  'Content-Type' = 'application/json'
)

body <- '{
  "size": 100,
  "query": {
    "match": {
      "numeroProcesso":  "00138746520048170001"
      }
   }
}';

res <- VERB("POST", url = lista_pe$Url[1], body = body, add_headers(headers))
json_response <- content(res, 'text')
parsed_json <- jsonlite::fromJSON(json_response, simplifyVector = FALSE)



# CE ----------------------------------------------------------------------


lista_processos_estado <- lista_processos_ajuste %>% filter(Tribunal == lista_nomes_tribunais[25])
lista_processos_estado_string <- paste0('"', lista_processos_estado$processo, '"', collapse = ", ")

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
        "numeroProcesso": [', lista_processos_estado_string, ']
      }
    }
  }')

#print(body)
#print(endpoint)

res <- VERB("POST", url = lista_processos_estado$Url[1], body = body, add_headers(headers))

json_response <- content(res, 'text')

parsed_json <- jsonlite::fromJSON(json_response, simplifyVector = FALSE)

length(parsed_json$hits$hits[[96]]$`_source`$assuntos)


# SC ----------------------------------------------------------------------

lista_processos_estado <- lista_processos_ajuste %>% filter(Tribunal == lista_nomes_tribunais[25]) %>% filter(row_number() <= 1000 & row_number() > 0)
lista_processos_estado_string <- paste0('"', lista_processos_estado$processo, '"', collapse = ", ")

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
        "numeroProcesso": [', lista_processos_estado_string, ']
      }
    }
  }')

#print(body)
#print(endpoint)

res <- VERB("POST", url = lista_processos_estado$Url[1], body = body, add_headers(headers))

json_response <- content(res, 'text')

parsed_json <- jsonlite::fromJSON(json_response, simplifyVector = FALSE)

tab <- busca_processo(lista_processos_estado_string, lista_processos_estado$Url[1])
write_rds(tab, paste0("./processed/dados-sc1.rds"))


# SP ----------------------------------------------------------------------

lista_sp <- lista_processos_ajuste %>% filter(Tribunal == "Tribunal de Justiça do Estado de São Paulo")
lista_processos_api <- paste0('"', lista_sp$processo %>% str_sub(1,20), '"', collapse = ", ")
tab <- busca_processo(lista_processos_api, lista_sp$Url[1])
#write_rds(tab, "./processed/dados-al.rds")

headers <- c(
  'Authorization' = 'ApiKey cDZHYzlZa0JadVREZDJCendQbXY6SkJlTzNjLV9TRENyQk1RdnFKZGRQdw==',
  'Content-Type' = 'application/json'
)

# body <- '{
#   "size": 100,
#   "query": {
#     "match": {
#       "numeroProcesso":  "00053895120228260565"
#       }
#    }
# }';

# # first query with sort, to get the timestamp
# ver https://datajud-wiki.cnj.jus.br/api-publica/exemplos/exemplo3
# esse query deu origem ao tab_sp1
body <- paste0('{
    "size": 1000,
    "query": {
      "terms": {
        "numeroProcesso": [', lista_processos_api, ']
      }
    },
  "sort": [
  {
   "@timestamp": {
   "order": "asc"
    }
   }
   ]
  }')

# esses foram para o tab_sp2
body <- paste0('{
    "size": 5000,
    "query": {
      "terms": {
        "numeroProcesso": [', lista_processos_api, ']
      }
    },
  "sort": [
  {
   "@timestamp": {
   "order": "asc"
    }
   }
   ],
   "search_after": [ 1719623953327 ]
  }')

# esses foram para o tab_sp3
body <- paste0('{
    "size": 5000,
    "query": {
      "terms": {
        "numeroProcesso": [', lista_processos_api, ']
      }
    },
  "sort": [
  {
   "@timestamp": {
   "order": "asc"
    }
   }
   ],
   "search_after": [ 1721397300240 ]
  }')

# esses foram para o tab_sp4
body <- paste0('{
    "size": 5000,
    "query": {
      "terms": {
        "numeroProcesso": [', lista_processos_api, ']
      }
    },
  "sort": [
  {
   "@timestamp": {
   "order": "asc"
    }
   }
   ],
   "search_after": [ 1721568207615 ]
  }')

# esses foram para o tab_sp5
body <- paste0('{
    "size": 5000,
    "query": {
      "terms": {
        "numeroProcesso": [', lista_processos_api, ']
      }
    },
  "sort": [
  {
   "@timestamp": {
   "order": "asc"
    }
   }
   ],
   "search_after": [ 1727417330846 ]
  }')

# esses foram para o tab_sp6
body <- paste0('{
    "size": 5000,
    "query": {
      "terms": {
        "numeroProcesso": [', lista_processos_api, ']
      }
    },
  "sort": [
  {
   "@timestamp": {
   "order": "asc"
    }
   }
   ],
   "search_after": [ 1727584488188 ]
  }')

# esses foram para o tab_sp7
body <- paste0('{
    "size": 5000,
    "query": {
      "terms": {
        "numeroProcesso": [', lista_processos_api, ']
      }
    },
  "sort": [
  {
   "@timestamp": {
   "order": "asc"
    }
   }
   ],
   "search_after": [ 1727927140324 ]
  }')


res <- VERB("POST", url = lista_sp$Url[1], body = body, add_headers(headers))
json_response <- content(res, 'text')

parsed_json <- jsonlite::fromJSON(json_response, simplifyVector = FALSE)

#primeiros mil
tab_sp1 <- processa_parsed_json(parsed_json)

# proximos 5 mil, depois do search_after
tab_sp2 <- processa_parsed_json(parsed_json)

# proximos 5 mil, depois do search_after
tab_sp3 <- processa_parsed_json(parsed_json)

# proximos 5 mil, depois do search_after
tab_sp4 <- processa_parsed_json(parsed_json)

# proximos 5 mil, depois do search_after
tab_sp5 <- processa_parsed_json(parsed_json)

# proximos 5 mil, depois do search_after
tab_sp6 <- processa_parsed_json(parsed_json)

# proximos 5 mil, depois do search_after
tab_sp7 <- processa_parsed_json(parsed_json)

ultimo_timestamp_do_parsed_json <- as.character(parsed_json$hits$hits[[length(parsed_json$hits$hits)]]$sort[[1]])


#Binding them all
# Step 1: Get all object names in the environment
all_objects <- ls()

# Step 2: Filter for names that start with "tab"
tab_objects <- all_objects[grepl("^tab_sp", all_objects)]

# Step 3: Use mget to retrieve the data frames as a list
tab_data_frames <- mget(tab_objects)

# Step 4: Bind all the data frames into one
tab_sp <- bind_rows(tab_data_frames)

write_rds(tab_sp, "./processed/dados-sp.rds")






# DEBUG -------------------------------------------------------------------



# tentando um hit específico, para testar o erro
hit_da_vez <- parsed_json$hits$hits[[910]]$`_source`

linha_da_Vez <- processa_hit(hit_da_vez)

# # só para comparar o formato dos códigos dos processos (BA ok, PE e SP problemáticos)
# lista_processos_ajuste %>%
#   filter(str_detect(Tribunal, "Pernambuco|São Paulo|Bahia")) %>%
#   group_by(Tribunal) %>%
#   slice_sample(n = 1) %>%
#   select(Tribunal, processo)
# 
# processa_estado_sp(0, 1000, "sp1")


