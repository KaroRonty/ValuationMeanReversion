library(dplyr)
library(gtools)
library(ggplot2)
source("https://raw.githubusercontent.com/KaroRonty/ShillerGoyalDataRetriever/master/ShillerGoyalDataRetriever.r")

full_data <- full_data %>% 
  mutate(PE = P / E)

pe_data <- full_data %>% 
  select(dates, PE) %>%
  mutate(dates = paste0(.$dates, "-01")) %>% 
  na.omit() %>% 
  mutate(PE_n1 = lead(.$PE, 12 * 1),
         PE_n2 = lead(.$PE, 12 * 2),
         PE_n3 = lead(.$PE, 12 * 3),
         PE_n4 = lead(.$PE, 12 * 4),
         PE_n5 = lead(.$PE, 12 * 5),
         PE_n6 = lead(.$PE, 12 * 6),
         PE_n7 = lead(.$PE, 12 * 7),
         PE_n8 = lead(.$PE, 12 * 8),
         PE_n9 = lead(.$PE, 12 * 9),
         PE_n10 = lead(.$PE, 12 * 10)
  )

deciles <- seq(0, 1.1, 0.1)

for (i in 1:10){
  temp <- pe_data %>% 
    filter(PE < quantile(full_data$PE, deciles[i + 1], na.rm = T),
           PE > quantile(full_data$PE, deciles[i], na.rm = T)) %>%
    select(-dates) %>% 
    colMeans(na.rm = T)
  
  print(paste(quantile(full_data$PE, deciles[i + 1], na.rm = T), quantile(full_data$PE, deciles[i], na.rm = T)))
  
  assign(paste0("pe_n", i), as.data.frame(temp))
  assign(paste0("pe_n", i), as.data.frame(cbind(paste0(i, ". decile"), temp)))
}
# Bind all CAPE data frames
pes <- mget(ls(pattern = "pe_n")) %>% 
  bind_rows() %>% 
  `colnames<-`(c("Decile", "PE"))

# Order by decile & remove unnecessary data frames
pes <- pes[mixedorder(pes$Decile), ]
pes$PE <- as.numeric(pes$PE)
rm(list = ls(pattern = "pe_n"))

# Add orders & factor levels for plotting
pes$Year <- 0:10
pes$Decile <- as.factor(pes$Decile)
pes <- transform(pes, Decile=factor(Decile,levels=mixedsort(levels(Decile), 
                                                            decreasing=TRUE)))
# Plot
ggplot(data = pes, aes(x = Year, y = PE)) +
  geom_line(aes(color = Decile, group = Decile), size = 1.2) +
  scale_x_continuous(breaks = 0:10)