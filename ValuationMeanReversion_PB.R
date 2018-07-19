library(dplyr)
library(gtools)
library(ggplot2)
source("https://raw.githubusercontent.com/KaroRonty/ShillerGoyalDataRetriever/master/ShillerGoyalDataRetriever.r")

full_data <- full_data %>% 
  mutate(PB = 1 / as.numeric(bm))

pb_data <- full_data %>% 
  select(dates, PB) %>%
  mutate(dates = paste0(.$dates, "-01")) %>% 
  na.omit() %>% 
  mutate(PB_n1 = lead(.$PB, 12 * 1),
         PB_n2 = lead(.$PB, 12 * 2),
         PB_n3 = lead(.$PB, 12 * 3),
         PB_n4 = lead(.$PB, 12 * 4),
         PB_n5 = lead(.$PB, 12 * 5),
         PB_n6 = lead(.$PB, 12 * 6),
         PB_n7 = lead(.$PB, 12 * 7),
         PB_n8 = lead(.$PB, 12 * 8),
         PB_n9 = lead(.$PB, 12 * 9),
         PB_n10 = lead(.$PB, 12 * 10)
  )

deciles <- seq(0, 1.1, 0.1)

for (i in 1:10){
  temp <- pb_data %>% 
    filter(PB < quantile(full_data$PB, deciles[i + 1], na.rm = T),
           PB > quantile(full_data$PB, deciles[i], na.rm = T)) %>%
    select(-dates) %>% 
    colMeans(na.rm = T)
  
  assign(paste0("pb_n", i), as.data.frame(temp))
  assign(paste0("pb_n", i), as.data.frame(cbind(paste0(i, ". decile"), temp)))
}
# Bind all CAPE data frames
pbs <- mget(ls(pattern = "pb_n")) %>% 
  bind_rows() %>% 
  `colnames<-`(c("Decile", "PB"))

# Order by decile & remove unnecessary data frames
pbs <- pbs[mixedorder(pbs$Decile), ]
pbs$PB <- as.numeric(pbs$PB)
rm(list = ls(pattern = "pb_n"))

# Add orders & factor levels for plotting
pbs$Year <- 0:10
pbs$Decile <- as.factor(pbs$Decile)
pbs <- transform(pbs, Decile=factor(Decile,levels=mixedsort(levels(Decile), 
                                                                decreasing=TRUE)))
# Plot
ggplot(data = pbs, aes(x = Year, y = PB)) +
  geom_line(aes(color = Decile, group = Decile), size = 1.2) +
  scale_x_continuous(breaks = 0:10)
