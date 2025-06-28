library('reticulate')
library('tidyverse')
library('ggthemes')
library('patchwork')
library('viridis')

pdf(NULL)
use_virtualenv("/mnt/ssd1/pyenv") 
pd <- import("pandas")

hatway_data <- pd$read_pickle("fubar_fawlty.pickle")
hatway_array <- do.call('c', hatway_data)

excitel_data <- pd$Series(pd$read_pickle("network.pickle"))
excitel_array <- do.call('c', list(excitel_data))

hathwayTbl <- tibble(TS = hatway_array)
excitelTbl <- tibble(TS = excitel_array)

startTs <- as.POSIXct('2025-06-12 17:00')

hathwayPlot <- hathwayTbl |> filter(TS >= startTs) |> 
  mutate(DH = strftime(TS, '%Y-%m-%d %H:00')) |> 
  group_by(DH) |> 
  summarise(CNT = n()) |> 
  ungroup() |>
  mutate(DH = as.POSIXct(DH)) |>
  select(DH, CNT)

excitelPlot <- excitelTbl |> filter(TS >= startTs) |> 
  mutate(DH = strftime(TS, '%Y-%m-%d %H:00')) |> 
  group_by(DH) |> 
  summarise(CNT = n()) |> 
  ungroup() |>
  mutate(DH = as.POSIXct(DH)) |>
  select(DH, CNT)

combinedDf <- hathwayPlot |> inner_join(excitelPlot, join_by(DH)) |> 
  rename(HATHWAY = CNT.x, EXCITEL = CNT.y) |>
  pivot_longer(-DH)

ggplot(combinedDf, aes(x = DH, y=value, fill = name)) +
  theme_economist() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_viridis_d() +
  scale_x_datetime(date_labels = "%Y-%m-%d %H00", date_breaks='12 hours') +
  labs(x='', y='count', fill = 'provider',
       title='Internet Breaks', 
       subtitle=sprintf('failures [%s:%s]', min(combinedDf$DH), max(combinedDf$DH)),
       caption = '@StockViz')


ggsave("breaks.png", width=12, height=6, units = "in")

