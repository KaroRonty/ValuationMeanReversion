library(dplyr)
library(gtools)
library(ggplot2)

# Retrieve data from Shiller & Goyal
source("https://raw.githubusercontent.com/KaroRonty/ShillerGoyalDataRetriever/master/ShillerGoyalDataRetriever.r")

# Make a data frame containing the paths of the valuation measure
cape_data <- full_data %>% 
  select(dates, CAPE) %>%
  mutate(dates = paste0(.$dates, "-01")) %>% 
  na.omit() %>% 
  mutate(CAPE_n1 = lead(.$CAPE, 12 * 1),
         CAPE_n2 = lead(.$CAPE, 12 * 2),
         CAPE_n3 = lead(.$CAPE, 12 * 3),
         CAPE_n4 = lead(.$CAPE, 12 * 4),
         CAPE_n5 = lead(.$CAPE, 12 * 5),
         CAPE_n6 = lead(.$CAPE, 12 * 6),
         CAPE_n7 = lead(.$CAPE, 12 * 7),
         CAPE_n8 = lead(.$CAPE, 12 * 8),
         CAPE_n9 = lead(.$CAPE, 12 * 9),
         CAPE_n10 = lead(.$CAPE, 12 * 10)
  )
  
# Calculate valuation deciles & their paths
deciles <- seq(0, 1.1, 0.1)
for (i in 1:10){
  temp <- cape_data %>% 
    filter(CAPE < quantile(full_data$CAPE, deciles[i + 1], na.rm = T),
           CAPE > quantile(full_data$CAPE, deciles[i], na.rm = T)) %>%
    select(-dates) %>% 
    colMeans(na.rm = T)
  
  assign(paste0("cape_n", i), as.data.frame(temp))
  assign(paste0("cape_n", i), as.data.frame(cbind(paste0(i, ". decile"), temp)))
}
# Bind all CAPE data frames
capes <- mget(ls(pattern = "cape_n")) %>% 
  bind_rows() %>% 
  `colnames<-`(c("Decile", "CAPE"))

# Order by decile & remove unnecessary data frames
capes <- capes[mixedorder(capes$Decile), ]
capes$CAPE <- as.numeric(capes$CAPE)
rm(list = ls(pattern = "cape_n"))

# Add orders & factor levels for plotting
capes$Year <- 0:10
capes$Decile <- as.factor(capes$Decile)
capes <- transform(capes, Decile=factor(Decile,levels=mixedsort(levels(Decile), 
                                                       decreasing=TRUE)))
# Plot
ggplot(data = capes, aes(x = Year, y = CAPE)) +
  geom_line(aes(color = Decile, group = Decile), size = 1.2) +
  scale_x_continuous(breaks = 0:10)
