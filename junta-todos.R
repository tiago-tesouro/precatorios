# Specify the folder path where your .rds files are stored
folder_path <- "./processed"

# List all .rds files in the folder
rds_files <- list.files(path = folder_path, pattern = "\\.rds$", full.names = TRUE)

# Read all .rds files and store them as a list of data frames
df_list <- lapply(rds_files, readRDS)

# Bind all the data frames together into one data frame
super_tabela <- bind_rows(df_list)

write_rds(super_tabela, file = "dados-api-todos-tribunais.rds")

super_tabela %>% filter(!is.na(cod_assunto9))

super_tabela_long_assuntos <- super_tabela %>%
  gather(starts_with("nome_assunto"), key = nome_var, value = assunto) %>%
  filter(!is.na(assunto))

super_tabela_long_assuntos %>% count(assunto) %>% arrange(-n)
super_tabela %>% count(nome_classe) %>% arrange(-n)

teste_datas <- super_tabela %>% 
  group_by(numeroProcesso) %>%
  mutate()