library('reticulate')
library('tidyverse')
library('ggthemes')
library('patchwork')
library('viridis')

use_virtualenv("/mnt/ssd1/pyenv") 
pd <- import("pandas")

hatway_data <- pd$read_pickle("/mnt/ssd1/stockviz/lob/fubar_fawlty.pickle")
hatway_array <- do.call('c', hatway_data)

excitel_data <- pd$Series(pd$read_pickle("/mnt/ssd1/stockviz/lob/network.pickle"))
excitel_array <- do.call('c', list(excitel_data))

hathwayTbl <- tibble(TS = hatway_array)
excitelTbl <- tibble(TS = excitel_array)

startTs <- as.POSIXct('2025-06-12 17:00')

hathwayPlotDf <- hathwayTbl |> filter(TS >= startTs) |> 
  mutate(DH = strftime(TS, '%Y-%m-%d %H:00')) |> 
  group_by(DH) |> 
  summarise(CNT = n()) |> 
  ungroup() |>
  mutate(DH = as.POSIXct(DH))

excitelPlotDf <- excitelTbl |> filter(TS >= startTs) |> 
  mutate(DH = strftime(TS, '%Y-%m-%d %H:00')) |> 
  group_by(DH) |> 
  summarise(CNT = n()) |> 
  ungroup() |>
  mutate(DH = as.POSIXct(DH))

p1 <- ggplot(hathwayPlotDf, aes(x = DH, y=CNT)) +
  theme_economist() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_x_datetime(date_labels = "%Y-%m-%d %H00", date_breaks='12 hours') +
  labs(x='', y='count', 
       title='Hathway', 
       subtitle=sprintf('failures [%s:%s]', min(hathwayPlotDf$DH), max(hathwayPlotDf$DH)))

p2 <- ggplot(excitelPlotDf, aes(x = DH, y=CNT)) +
  theme_economist() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_x_datetime(date_labels = "%Y-%m-%d %H00", date_breaks='12 hours') +
  labs(x='', y='count', 
       title='Excitel', 
       subtitle=sprintf('failures [%s:%s]', min(excitelPlotDf$DH), max(excitelPlotDf$DH)))

p1/p2 + plot_annotation(
  theme = theme_economist(),
  title = "Internet Breaks",
  caption = '@StockViz')

ggsave("breaks.png", width=12, height=12, units = "in")

