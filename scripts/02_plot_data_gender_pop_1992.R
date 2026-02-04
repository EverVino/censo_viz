library("dplyr")
library("readr")

df <- read_delim("data/raw/Censo1992/BaseRedatamExtracted/PERSONA.csv", delim=";")

head(df)

colnames(df)
str(df)
nrow(df)
# P04 Edad # Genero P03
#poblacion (VIVIENDA_REF_ID) < vivienda (VIVIENDA_REF_ID, MUNIC_REF_ID) < municipio (MUNIC_REF_ID, PROVIN_REF_ID) < provincia (PROVIN_REF_ID, DEPTO_REF_ID)

df_vivienda <- read_delim("data/raw/Censo1992/BaseRedatamExtracted/VIVIENDA.csv", delim=";")
df_municipio <- read_delim("data/raw/Censo1992/BaseRedatamExtracted/MUNIC.csv", delim=";")
df_provincia <- read_delim("data/raw/Censo1992/BaseRedatamExtracted/PROVIN.csv", delim=";")
df_departamento <- read_delim("data/raw/Censo1992/BaseRedatamExtracted/DEPTO.csv", delim=";")

colnames(df_departamento)
colnames(df_provincia)
colnames(df_municipio)
colnames(df_vivienda)

poblacion <- df %>% select(VIVIENDA_REF_ID, P04, P03)
head(poblacion)
n_poblacion <- poblacion %>% 
  left_join(df_vivienda %>% select(VIVIENDA_REF_ID, MUNIC_REF_ID), by='VIVIENDA_REF_ID') %>%
  left_join(df_municipio %>% select(MUNIC_REF_ID, PROVIN_REF_ID), by='MUNIC_REF_ID') %>%
  left_join(df_provincia %>% select(PROVIN_REF_ID, DEPTO_REF_ID), by='PROVIN_REF_ID') %>%
  rename(edad=P04, sexo=P03) %>%
  select(-VIVIENDA_REF_ID, -MUNIC_REF_ID, -PROVIN_REF_ID)

resultados <- list()
for (dep_id in 1:9) {
  to_write <- n_poblacion %>%
    filter(DEPTO_REF_ID == dep_id) %>%
    group_by(sexo, edad) %>%
    summarize(cantidad = n(), .groups = "drop") %>%
    mutate(
      rango_edad = ifelse(
        edad >= 100,
        "100+",
        paste0(
          floor(edad/5)*5,
          "-",
          floor(edad/5)*5 + 4
        )
      )
    ) %>%
    group_by(rango_edad, sexo) %>%
    summarize(total = sum(cantidad), .groups = "drop") %>%
    mutate(dep = dep_id)
  
  resultados[[dep_id]] <- to_write
}

final <- bind_rows(resultados)
write.csv(final, "data/processed/poblacion_genero_bolivia_1992.csv", row.names = FALSE)
