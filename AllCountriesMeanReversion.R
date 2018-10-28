library(dplyr)
library(gtools)
library(ggplot2)

data <- read.csv("chart.csv", fileEncoding = "UTF-8-BOM")

# Replace zeroes with NA and delete unneeded rows and columns
data[data == 0] <- NA
data <- data[1:I(grep("The", data$Date) - 1), ]
data <- data %>% select(-Date)

# Combine CAPEs of different countries
na_vector <- rep(NA, 10)
cape_data <- NA
for(i in seq_along(colnames(data))){
  cape_data <- c(cape_data, na_vector, data[, i])
}

# Format as data frame
cape_data <- as.data.frame(cape_data)
colnames(cape_data) <- "CAPE"

# Calculate CAPE after n months
for(i in 1:20){
  cape_data <- cbind(cape_data, lead(cape_data$CAPE, 12 * i))
  colnames(cape_data)[i + 1] <- paste0("CAPE_n", i)
}

# Calculate means for the CAPEs in each decile
deciles <- seq(0, 1.1, 0.1)
for (i in 1:10){
  temp <- cape_data %>% 
    filter(CAPE < quantile(cape_data$CAPE, deciles[i + 1], na.rm = T),
           CAPE > quantile(cape_data$CAPE, deciles[i], na.rm = T)) %>%
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
capes$Year <- 0:20
capes$Decile <- as.factor(capes$Decile)
capes <- transform(capes, Decile=factor(Decile,levels=mixedsort(levels(Decile), 
                                                                decreasing=TRUE)))
# Plot
ggplot(data = capes, aes(x = Year, y = CAPE)) +
  geom_line(aes(color = Decile, group = Decile), size = 1.2) +
  scale_x_continuous(breaks = 0:20)
