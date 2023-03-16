#import excel file in RStudio

library(readr)
NFB21 <- read_delim("C:/Users/RiyaBiswas/OneDrive - Exsurgo Rehab Ltd/Desktop/R package/NFB21.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(NFB21)
