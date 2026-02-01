library("dplyr")
library("haven")

data <- read_sav("data/raw/Censo1976/BOL_POB_1976_.sav")

resultados <- list()

for (dep_id in 1:9) {
    to_write <- data %>%
        filter(dep == dep_id) %>%
        group_by(p03, p04) %>%
        summarize(cantidad = n(), .groups = "drop") %>%
        mutate(
            rango_edad = ifelse(
                p04 >= 80,
                "80+",
                paste0(
                    floor(p04/5)*5,
                    "-",
                    floor(p04/5)*5 + 4
                )
            )
        ) %>%
        rename(genero = p03) %>%
        group_by(rango_edad, genero) %>%
        summarize(total = sum(cantidad), .groups = "drop") %>%
        mutate(dep = dep_id)
    resultados[[dep_id]] <- to_write
}

final <- bind_rows(resultados)
write.csv(final, "data/processed/poblacion_genero_bolivia.csv", row.names = FALSE)

