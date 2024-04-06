
source("rmd/scripts/baseEnergia.R")

pop <- base %>%  
  select(country, iso_code, year, population)

emisiones_capita_elec <- baseCo2 %>% 
  mutate(iso_code = ifelse(Country == "WORLD", "WOR", Country),
         year = as.numeric(year)) %>%
  left_join(y=pop %>% select(-country), by = c("iso_code", "year")) %>% 
  mutate(capita = valor/population*1000000000) %>% 
  filter(iso_code == "WOR" & year >= 1990 & categorica == "Electricidad")

# Grafico de participación de fuentes

generacion_capita_elec <- base %>% filter(country == "Colombia" | country == "World") %>% 
  select(country, year, iso_code, ends_with("electricity"), greenhouse_gas_emissions, population)

generacion_capita_elec <- generacion_capita_elec %>% 
  gather(key = tipo, value = valor, 4:length(generacion_capita_elec))

generacion_capita_elec <- generacion_capita_elec %>% 
  filter(country == "World" & tipo == "per_capita_electricity" & year >= 1990)


generacion_capita_elec <- generacion_capita_elec %>%
  ungroup() %>% 
  left_join(y = emisiones_capita_elec %>% 
              ungroup() %>% 
              select(year, capita), by = ("year"))

indice1 <- generacion_capita_elec %>% 
  filter(year == 1990) %>% 
  select(valor)

indice2 <- generacion_capita_elec %>% 
  filter(year == 1990) %>% 
  select(capita)

indice1.1 <- generacion_capita_elec %>% 
  filter(year == 2020) %>% 
  select(valor)

indice2.2 <- generacion_capita_elec %>% 
  filter(year == 2020) %>% 
  select(capita)


crec_generacion <- ((1+(indice1.1$valor[1]/indice1$valor[1] - 1))^(1/(2020-1990)) - 1)*100
crec_emisiones <- ((1+(indice2.2$capita[1]/indice2$capita[1] - 1))^(1/(2020-1990)) - 1)*100

grafico_gen_emi <- generacion_capita_elec %>% 
  filter(year <= 2020) %>% 
  mutate(indice_generacion = valor/indice1$valor[1]*100,
         indice_emision = capita/indice2$capita[1]*100) %>% 
  ggplot(mapping = aes(x = year)) +
  geom_rect(mapping = aes(xmin = 2013, xmax = 2020,
                          ymin = -Inf, ymax = Inf),
            fill = "lightgrey",
            alpha = 0.025)+
  geom_line(mapping = aes(y = indice_generacion), color = "#A85D51", linewidth = 1.1) +
  geom_line(mapping = aes(y = indice_emision), color = "#6EA851", linewidth = 1.1) +
  geom_point(mapping = aes(y = indice_generacion), color = "#A85D51", shape = 15, size = 2) +
  geom_point(mapping = aes(y = indice_emision), color = "#6EA851", shape = 17, size = 2) +
  scale_x_continuous(breaks = seq(1990, 2020, 5),
                     limits = c(1990, 2024)) +
  annotate(geom = "text",
           x = c(2023, 2023),
           y = c(148, 119),
           label = c(paste("Generación \n","+", round(crec_generacion, 2), "% ", "anual", sep = ""),
                     paste("Emisiones \n","+", round(crec_emisiones, 2), "% ", "anual", sep = "")),
           size = c(4,4),
           color = c("#A85D51", "#6EA851")) +
  labs(x = NULL,
       y = "Indice base 100 = 1990",
       title = "Crecimiento de la **generación de eléctricidad per cápita** y de las **emisione de Gases Efecto Invernadero (GEI) per cápita** del sector eléctrico",
       subtitle = "<span style='color:#4B5253;'>En los ultimos años se ha empezado a observar un mayor distanciamiento entre estas series. La sustitución de cabón y petróleo por gas, sumado al fuerte impulso <br>que han tenido las fuentes de energía renovable, ayudan a explicar la **divergencia** de las series a nivel mundial.</span>",
       caption = "Fuente: Elaboración propia en base a Climate Watch y OWiD <br> **#30DayChartChallenge #Day4** <br> @GEstebanGomez") +
  # Set theme settings
  theme_bw() +
  theme(plot.title = element_markdown(hjust = 0.0, size = 11),
        plot.subtitle = element_markdown(hjust = 0.0, size = 9),
        plot.caption = element_markdown(size=8, hjust=0.0, face="italic", color="black"),
        axis.text.x = element_text(size = 7, angle = 0),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        axis.title.x = element_text(size = 7),
        legend.title = element_text(size=7, hjust = 0.5), 
        legend.text = element_text(size=6),
        legend.position = "none")

ggsave(filename = "../rmd/resultados/graficos/30DayChartChallenge/5Day_diverging.png",
       plot = grafico_gen_emi,
       dpi = 500,
       width = 10.6,
       height = 4.58
)
