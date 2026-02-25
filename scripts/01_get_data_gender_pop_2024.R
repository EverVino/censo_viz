library("dplyr")
library("readr")

df <- read_delim("data/raw/Censo2024/BasededatosCSV/Persona_CPV-2024.csv")

head(df)

colnames(df)
str(df)
nrow(df)
# P25 Edad # Genero P24
#poblacion (VIVIENDA_REF_ID) < vivienda (VIVIENDA_REF_ID, MUNIC_REF_ID) < municipio (MUNIC_REF_ID, PROVIN_REF_ID) < provincia (PROVIN_REF_ID, DEPTO_REF_ID)

poblacion <- df %>% select(idep, p25_sexo, p26_edad)

head(poblacion)
unique(poblacion$idep)

to_write <- poblacion %>%
  filter(idep == "02") %>%
  group_by(p25_sexo, p26_edad) %>%
  summarize(cantidad = n(), .groups = "drop") %>%
  mutate(
    rango_edad = ifelse(
      p26_edad >= 100,
      "100+",
      paste0(
        floor(p26_edad/5)*5,
        "-",
        floor(p26_edad/5)*5 + 4
      )
    )
  ) %>%
  rename(genero = p25_sexo) %>%
  group_by(rango_edad, genero) %>%
  summarize(total = sum(cantidad), .groups = "drop")

head(to_write)
sum(to_write$total)

resultados <- list()

for (dep_id in c("01", "02", "03", "04", "05", "06", "07", "08", "09")) {
  to_write <- poblacion %>%
    filter(idep == dep_id) %>%
    group_by(p25_sexo, p26_edad) %>%
    summarize(cantidad = n(), .groups = "drop") %>%
    mutate(
      rango_edad = ifelse(
        p26_edad >= 100,
        "100+",
        paste0(
          floor(p26_edad/5)*5,
          "-",
          floor(p26_edad/5)*5 + 4
        )
      )
    ) %>%
    group_by(rango_edad, p25_sexo) %>%
    summarize(total = sum(cantidad), .groups = "drop") %>%
    mutate(dep = dep_id)
  
  resultados[[dep_id]] <- to_write
}

final <- bind_rows(resultados)
write.csv(final, "data/processed/poblacion_genero_bolivia_2024.csv", row.names = FALSE)
