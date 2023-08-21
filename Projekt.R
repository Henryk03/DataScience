library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(forcats)
library(factoextra)
library(ggrepel)

# setwd("~/Desktop/Scuola/Informatica/Data_Science/DataScience_Project")

# Caricamento dei dataset
bestselling_consoles = read_csv("best-selling game consoles.csv")
videogame_sales = read_csv("vgsales.csv")

colnames(bestselling_consoles) = c("console", "type", "company", "release", "discontinuation", "units_sold_mln", "remarks")

# bsgc serve per "costruire" console_sales
bsgc = bestselling_consoles %>%
  group_by(console, release, discontinuation) %>%
  summarise("release_year" = ifelse(release > discontinuation & discontinuation == 0,
                                    release,
                                    min(release, discontinuation)
                                    ), 
            "discontinuation_year" = ifelse(discontinuation == 0, 
                                  discontinuation, 
                                  max(release, discontinuation)
                                            )
  )

# Dataset (sistemato) sulle vendite delle console
console_sales = left_join(bestselling_consoles, bsgc) %>%
  select(console:company, release_year, discontinuation_year, units_sold_mln)


# vgs1 e vgs2 servono per "costruire" vg_esclusive e vg_multiplatform
vgs1 = select(videogame_sales, -c("Rank")) %>%
  mutate(Year = replace(Year, Year == "N/A", NA),
         Publisher = replace(Publisher, Publisher == "N/A", NA)
         )

vgs2 = vgs1 %>%
  group_by(Name) %>%
  summarise(n = n())

# Dataset dei titoli esclusive
vg_esclusive = semi_join(vgs1, filter(vgs2, n == 1)) %>%
  mutate(Platform = "Exclusive")

# Dataset dei titoli multi-piattaforma
vg_multiplatform = anti_join(vgs1, vg_esclusive, by = "Name") %>%
  group_by(Name) %>%
  mutate(Year = min(Year, na.rm = TRUE),
         Platform = "Multi-platform",
         NA_Sales = sum(NA_Sales, na.rm = TRUE),
         EU_Sales = sum(EU_Sales, na.rm = TRUE),
         JP_Sales = sum(JP_Sales, na.rm = TRUE),
         Other_Sales = sum(Other_Sales, na.rm = TRUE),
         Global_Sales = sum(Global_Sales, na.rm = TRUE)
         ) %>%
  distinct()

# Dataset contenente tutti i titoli esclusive e multi-piattaforma 
vg_sales = union(vg_esclusive, vg_multiplatform) %>%
  arrange(desc(Global_Sales)) %>%
  na.omit()


# Visualize

# 1a domanda
bestselling_types = console_sales %>%
  group_by(type) %>%
  summarise(sales = sum(units_sold_mln))

ggplot(data = bestselling_types, mapping = aes(x = fct_reorder(type, sales), y = sales)) +
  geom_col(width = 0.7) +
  geom_text(mapping = aes(label = sales),
            size = 7,
            color = "red",
            nudge_y = 40) +
  labs(x = NULL, y = "Unità vendute (milioni)") +
  theme_minimal() 


# 2a domanda
tidy1 = vg_sales %>%
  group_by(Genre) %>%
  summarise(NordAmerica = sum(NA_Sales),
            Europa = sum(EU_Sales),
            Giappone = sum(JP_Sales),
            Altri = sum(Other_Sales)
  )

color = c("#B30000", "#FF6600", "#FFB300", "#8CC63F", "#33A1C9", "#003399", "#660099", "#FF007F", "#00CC99", "#009999", "#CC0066", "#FFCC90")

best_genre = pivot_longer(tidy1, c("NordAmerica", "Europa", "Giappone", "Altri"), 
                          names_to = "continent", 
                          values_to = "value")

ggplot(data = best_genre) +
  geom_col(mapping = aes(x = continent, y = value, fill = Genre), position = "dodge") +
  labs(x = NULL, y = "Copie vendute (milioni)", fill = "Genere") +
  scale_fill_manual(values = color) +
  theme_minimal()


# 3a domanda
console_sales_reg1 = console_sales %>%
  head(30)

console_sales_reg2 = console_sales_reg1[-c(3, 5, 17, 19, 20, 24, 27, 28, 29), ]

# Short name per le console
shortn = c("PS2", "DS", "GB", "PS4", "PS", "Wii", "PS3", "X360", "GBA", "PSP", "3DS", "NES", "XOne", 
           "SNES", "N64", "2600", "XB", "GC", "WiiU", "PSV", "SAT")

console_sales_reg3 = cbind(console_sales_reg2, shortn) %>%
  select(console, shortn, units_sold_mln)

vg_sales_reg = vgs1 %>%
  select(-c("NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales")) %>%
  group_by(Platform) %>%
  summarise(vgsales = sum(Global_Sales, na.rm = TRUE))

# Dataset per la regressione e il clustering
sales_reg = left_join(console_sales_reg3, vg_sales_reg, by = c("shortn" = "Platform")) %>%
  select(console, units_sold_mln, vgsales) %>%
  na.omit()


# Regressione
ggplot(data = sales_reg, 
       mapping = aes(x = units_sold_mln, 
                     y = vgsales,
                     )
       ) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  labs(x = "Vendite console (milioni)",
       y = "Vendite videogames (milioni)"
       ) +
  theme_minimal()


# Clustering
# Dataset scalato
sales_reg_scaled = scale(select(sales_reg, -c("console")))

set.seed(123)
model = kmeans(sales_reg_scaled, centers = 4, nstart = 25)
print(model)

# Deviazione Standard
usm_sd = sd(sales_reg$units_sold_mln)
vgs_sd = sd(sales_reg$vgsales)

# media originale = (media scalata * deviazione standard) + media originale della scala
# NB: la media scalata la ricaviamo da print(model)

label_cluster1 = paste("Media vendite console: ", format(-1.0836638 * usm_sd + mean(sales_reg$units_sold_mln), 
                                                         digits = 6
                                                         ), 
                       "\nMedia vendite videogames: ", format(-0.7487726 * vgs_sd + mean(sales_reg$vgsales), 
                                                              digits = 6)
                       )

label_cluster2 = paste("Media vendite console: ", format(2.0115538 * usm_sd + mean(sales_reg$units_sold_mln),
                                                         digits = 6),
                       "\nMedia vendite videogames: ", format(1.7172399 * vgs_sd + mean(sales_reg$vgsales),
                                                              digits = 6)
                       )

label_cluster3 = paste("Media vendite console: ", format(0.6067101 * usm_sd + mean(sales_reg$units_sold_mln),
                                                         digits = 6),
                       "\nMedia vendite videogames: ", format(1.3342042 * vgs_sd + mean(sales_reg$vgsales),
                                                              digits = 6)
                       )

label_cluster4 = paste("Media vendite console: ", format(0.1419623 * usm_sd + mean(sales_reg$units_sold_mln),
                                                         digits = 6),
                       "\nMedia vendite videogames: ", format(-0.4412360 * vgs_sd + mean(sales_reg$vgsales),
                                                              digits = 6)
                       )

annotation = data.frame(x = c(-1.083663, 2.0115538, 0.6067101, 0.1419623),
                        y = c(-0.7487726, 1.7172399, 1.3342042, -0.4412360),
                        label = c(label_cluster1, label_cluster2, label_cluster3, label_cluster4))

fviz_cluster(model,
             data = sales_reg_scaled,
             palette = c("#FFB300", "#800080", "#008000", "#FF0000"), 
             geom = "point",
             ggtheme = theme_minimal()
             ) +
  geom_label_repel(data = annotation, 
                   mapping = aes(x = x, y = y, label = label),
                   nudge_x = 0.3,
                   nudge_y = 0.8,
                   size = 3
                   ) +
  coord_cartesian(xlim = c(-1.5, 3), ylim = c(-1, 3)) +
  labs(title = NULL,
       x = "Vendite console (scalato)",
       y = "Vendite videogames (scalato)"
       )


# 4a domanda
vgs_decennio = vg_sales %>%
  mutate("decennio" = ifelse(Year >= 1980 & Year <= 1989, 
                             "80-89",
                             ifelse(Year >= 1990 & Year <= 1999,
                                    "90-99",
                                    ifelse(Year >= 2000 & Year <= 2009,
                                           "00-09",
                                           ifelse(Year >= 2010 & Year <= 2016,
                                                  "10-16",
                                                  "Error"
                                                  )
                                          )
                                    )
                             )
         )

vgs_decennio1 = subset(vgs_decennio, decennio != "Error")

order = c("80-89", "90-99", "00-09", "10-16")

ggplot(data = vgs_decennio1, mapping = aes(x = decennio, y = Global_Sales, fill = Platform)) +
  geom_boxplot() +
  scale_x_discrete(limits = order) +
  coord_cartesian(ylim = c(-5, 20)) +
  labs(x = "Decennio",
       y = "Copie vendute (milioni)",
       fill = "Piattaforma"
       ) +
  theme_minimal()