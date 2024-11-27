library(tidyverse)
budget <-  read_csv("102年度新北市總預算歲入來源別預算比較總表（經資併計）_export.csv")
glimpse(budget)
budget <- budget %>%
  rename(
    項目 = field1,
    本年度預算數 = field2,
    上年度預算數 = field3,
    前年度決算數 = field4,
    本年度與上年度比較 = field5
  )


