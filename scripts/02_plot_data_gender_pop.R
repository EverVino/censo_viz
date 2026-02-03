library("ggplot2")
library("readr")
library("dplyr")

df <- read_csv("data/processed/poblacion_genero_bolivia.csv")

head(df)
poblacion_total <- sum(df$total)
poblacion_total


poblacion_lp <- df %>%
  filter(dep == 2) %>%
  mutate(
  edad_inicio = as.integer(sub("-.*|\\+", "", rango_edad))
  ) %>%
  arrange(edad_inicio) %>%
  mutate(
    rango_edad = factor(rango_edad, levels = unique(rango_edad))
  ) %>%
  mutate(porcentaje=paste(round(total/poblacion_total_lp*100,2),"%",sep=""))

poblacion_total_lp <- sum(poblacion_lp$total)
poblacion_total_lp
head(poblacion_lp)

ggplot(poblacion_lp, 
       aes(x = rango_edad, 
           y = ifelse(genero == 1, -total, total), 
           fill = factor(genero)
           )
       ) +
  geom_col() +
  geom_text(label= poblacion_lp$porcentaje, stat = "identity", 
            hjust=ifelse(test = poblacion_lp$genero == 1,  yes = -0.25, no = 1.25),
            color="white", fontface="bold", size=3)+
  scale_y_continuous(labels = abs) +
  coord_flip() +
  #labs(title = "Piramide poblacional - La Paz (1976)", y = "Poblacion", x ="Rango de Edad", fill="Genero") +
  theme_minimal() +
  scale_fill_manual(
    values = c("steelblue", "tomato"),
    labels = c("Hombres", "Mujeres")
  ) +
  #scale_fill_brewer(palette="Pastel1") +
  labs(
    x = "",
    y = "",
    fill=""
  )+
  theme( 
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(),
    axis.text.x=element_blank(), 
    axis.text.y=element_text( size=15),
    strip.text.x=element_text(size=15),
    legend.position="bottom",
    legend.text=element_text(size=20)
  )


