library("ggplot2")
library("readr")
library("dplyr")


df <- read_csv("data/processed/poblacion_genero_bolivia_1976.csv")

head(df)

poblacion_lp <- df %>% filter(dep==2)
poblacion_total_lp <- sum(poblacion_lp$total)
poblacion_total_lp
poblacion_lp <- poblacion_lp %>%
  mutate(
  edad_inicio = as.integer(sub("-.*|\\+", "", rango_edad))
  ) %>%
  arrange(edad_inicio) %>%
  mutate(
    rango_edad = factor(rango_edad, levels = unique(rango_edad))
  ) %>%
  mutate(porcentaje=paste(round(total/poblacion_total_lp*100,2),"%",sep=""))

head(poblacion_lp)

p <- ggplot(poblacion_lp, 
       aes(x = rango_edad, 
           y = ifelse(genero == 1, -total, total), 
           fill = factor(genero)
           )
       ) +
  geom_col() +
  geom_text(label= poblacion_lp$porcentaje, stat = "identity", 
            hjust=ifelse(test = poblacion_lp$genero == 1,  yes = -0.25, no = 1.25),
            color="white", fontface="bold", size=4.5)+
  scale_y_continuous(labels = abs) +
  coord_flip() +
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
    axis.text.y=element_text(size=15),
    strip.text.x=element_text(size=15),
    legend.position="bottom",
    legend.text=element_text(size=15),
    plot.margin = margin(100, 20, 20, 20)
  )

ggsave(
  "figures/piramide_lp_1976.svg",
  p,
  width = 8,
  height = 8,
  units = "in"
)
