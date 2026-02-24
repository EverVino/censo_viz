library("dplyr")
library("tidyr")
df <- read.csv("data/raw/Censo2001/dep_edad_genero_2001.csv")
colnames(df)[3] <- "hombre"
colnames(df)[4] <- "mujer"

ndf <- gather(data=df, key=sexo, value=cantidad,3:4)
ndf <- ndf %>% mutate(cantidad = gsub(" ", "", cantidad))
ndf$cantidad <- as.integer(ndf$cantidad)
head(ndf)
str(ndf)

resultados <- list()

for (dep_id in 1:9) {
  to_write <- ndf %>% 
    filter(dep==dep_id) %>% 
    mutate(rango_edad=ifelse(
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
    summarize(total = sum(cantidad), .groups = "drop")%>%
    mutate(dep = dep_id)
  
  resultados[[dep_id]] <- to_write
}

final <- bind_rows(resultados)
write.csv(final, "data/processed/poblacion_genero_bolivia_2001.csv", row.names = FALSE)

