library(RSQLite)
library(tidyverse)
library(lubridate)
library(ggrepel)

con <- dbConnect(SQLite(), "data/birds.db")
detections <- dbReadTable(con, "detections")
dbDisconnect(con)

top_5_species <- detections |>
  count(Com_Name, sort = TRUE) |>
  head(5) |>
  pull(Com_Name)

n_days <- detections |>
  mutate(date = as.Date(Date)) |>
  filter(date < (max(date, na.rm = TRUE) - 7)) |>
  distinct(date) |>
  nrow()

#Anzahl aller Erkennungen pro Tag
detections |>
  mutate(date = as.Date(Date),
         date_group = floor_date(date, "1 days")) |>
  filter(date < (max(date, na.rm = TRUE) - 7)) |>
  count(date_group) |>
  ggplot(aes(x = date_group, y = n / 1)) +
  geom_line() +
  geom_point() +
  labs(title = "Anzahl Vogel Erkennungen",
       x = "Datum", y = "Anzahl") +
  theme_minimal()

ggsave("visualization/daily_detections.png", width = 10, height = 6, dpi = 300)


#Verlauf der 5 Häufigsten Vogelarten
detections |>
  filter(Com_Name %in% top_5_species) |>
  mutate(date = as.Date(Date),
         date_group = floor_date(date, "3 days")) |>
  filter(date < (max(date, na.rm = TRUE) - 7)) |>
  mutate(Com_Name = fct_infreq(Com_Name)) |>
  count(date_group, Com_Name) |>
  ggplot(aes(x = date_group, y = n, color = Com_Name, fill = Com_Name)) +
  geom_ribbon(aes(ymin = 0, ymax = n), alpha = 0.2) +
  geom_line(size = 1) +
  coord_cartesian(ylim = c(0, 150)) +
  labs(title = "Top 5 erkannte Vogelarten",
       x = "Datum",
       y = "Anzahl an Erkennungen",
       color = "Vogelarten",
       fill = "Vogelarten") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("visualization/top5_ribbon_3day.png", width = 15, height = 6, dpi = 300)



#Vergleich stündliche Aktivitäten der 5 häufigsten Vögel
detections |>
  filter(Com_Name %in% top_5_species) |>
  mutate(
    date = as.Date(Date),
    hour_decimal = hour(hms(Time)) + minute(hms(Time)) / 60
  ) |>
  filter(date < (max(date, na.rm = TRUE) - 7)) |>
  mutate(Com_Name = fct_infreq(Com_Name)) |>
  ggplot(aes(x = hour_decimal, fill = Com_Name)) +
  geom_histogram(
    binwidth = 0.50,
    boundary = 0,
    alpha = 0.7,
    closed = "left"
  ) +
  facet_wrap(~Com_Name, ncol = 1, scales = "fixed") +
  scale_x_continuous(
    breaks = 5:22,
    labels = 5:22,
    limits = c(5, 22)
  ) +
  labs(
    title = "Aktivitäten",
    x = "Stunde des Tages",
    y = "Anzahl an Erkennungen"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("visualization/top5_hours.png", width = 15, height = 6, dpi = 300)


#Durchschnittliche Vogelaktivität pro Tag
detections |>
  mutate(
    date = as.Date(Date),
    hour_decimal = hour(hms(Time)) + minute(hms(Time)) / 60
  ) |>
  filter(date < (max(date, na.rm = TRUE) - 7)) |>
  ggplot(aes(x = hour_decimal, y = after_stat(count)/ n_days)) +
  geom_histogram(
    binwidth = 0.5,
    boundary = 0,
    alpha = 0.7,
    fill = "steelblue",
    closed = "left"
  ) +
  geom_vline(xintercept = 11.5, linetype = "dashed", color = "black", size = 1) +

  annotate("text", x = 11.7, y = 0, label = "Katzen betreten Garten",
           hjust = 0, vjust = -0.5, color = "black", size = 4) +

  scale_x_continuous(
    breaks = 0:24,
    labels = 0:24,
    limits = c(0, 24)
  ) +
  labs(
    title = "Durchschnittliche Tägliche Aktivität",
    x = "Stunde des Tages",
    y = "Durchschnittliche Erkennungen innerhalb 30 minuten"
  ) +
  theme_minimal()
ggsave("visualization/avg_daily_30min.png", width = 20, height = 6, dpi = 300)


#Anzahl an Aufnahmen per Vogelart
pie_data <- detections |>
  mutate(date = as.Date(Date)) |>
  filter(date < (max(date, na.rm = TRUE) - 7)) |>
  count(Com_Name, sort = TRUE) |>
  mutate(
    species_group = if_else(row_number() <= 9, Com_Name, "Andere")
  ) |>
  group_by(species_group) |>
  summarise(n = sum(n), .groups = "drop") |>
  arrange(desc(n)) |>
  mutate(
    percentage = n / sum(n) * 100,
    label = paste0(species_group, "\n", n, " (", round(percentage, 1), "%)"),
    species_group = factor(species_group, levels = c(setdiff(unique(species_group), "Andere"), "Andere"))) |>
  arrange(species_group) |>
  mutate(
    csum = rev(cumsum(rev(n))),
    pos = n/2 + lead(csum, 1),
    pos = if_else(is.na(pos), n/2, pos)
  )

ggplot(pie_data, aes(x = "", y = n, fill = species_group)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  geom_label_repel(aes(y = pos, label = label),
                   size = 4,
                   nudge_x = 0.7,
                   segment.color = NA,
                   show.legend = FALSE) +
  labs(
    title = "Vogelarten Verteilung",
    fill = "Vogelarten"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "none"
  )
ggsave("visualization/species_piechart.png", width = 12, height = 8, dpi = 300)