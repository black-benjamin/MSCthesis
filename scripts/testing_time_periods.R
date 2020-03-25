library(tidyverse)
library(gridExtra)

#redacting PA data to just columns for category, establishment date and size, renaming columns to category, year and size and adding 
#a new column type with "PA" as value for all. 

PAs_redact <- Filtered_PAs %>% select(category, Original_establishment_year, calculated_area_ha) 
PAs_redact <-rename(PAs_redact, "area" = "calculated_area_ha", "year" = "Original_establishment_year")
PAs_redact$type <- c("PA")
PAs_redact$Inside_PA <- NA
view(PAs_redact)

#same process repeated for ELCs

ELCs_redact <- ELCS_PAs_associated %>% select(year_establishment, area_ha, Inside_PA) 
ELCs_redact <- rename(ELCs_redact, "area" = "area_ha", "year" = "year_establishment")
ELCs_redact$type <-  c("ELC")
ELCs_redact$category <- NA
view(ELCs_redact)

#Appending one dataset to the other on matching column names and filling in NA for blanks in non-matching

str(PA_ELC_merged)

PA_ELC_merged <- rbind(PAs_redact, ELCs_redact)
PA_ELC_rounded <- PA_ELC_merged %>% 
  mutate_at(vars(area), list(~ round(.,)))
view(PA_ELC_rounded)

#creating bins for temporal periods to assess how much PA/ELC land would be excluded and included under different bin widths

# 6 year periods as originally planned (2000-2006; 2007-2012; 2013-2018)
Periods6 <- c("2000-2006", "2007-2012", "2013-2018")
#3 year periods ranging from 2010 to 2018
Periods3 <- c("2010-2012","2013-2015", "2016-2018")
Periods2 <- c("2011-2012","2013-2014", "2015-2016", "2017-2018")

#mapping these bins as new columns on the dataframe

data_bins <- as_tibble(PA_ELC_rounded) %>%  mutate(six_year_bins = case_when(year > 2000 & year < 2007 ~ Periods6[1],
                           year > 2007 & year < 2013 ~ Periods6[2],
                           year > 2013 & year < 2019 ~ Periods6[3]),
            three_year_bins = case_when(year > 2010 & year < 2013 ~ Periods3[1],
                                        year > 2013 & year < 2016 ~ Periods3[2],
                                        year > 2016 & year < 2019 ~ Periods3[3]), 
                                        two_year_bins = case_when(year > 2011 & year < 2013 ~ Periods2[1],
                                                                  year > 2013 & year < 2015 ~ Periods2[2],
                                                                  year > 2015 & year < 2017 ~ Periods2[3],
                                                                  year > 2017 & year < 2019 ~ Periods2[4]))
view(data_bins)

#Plot of land excluded using 6 year periods as originally planned (2000-2006; 2007-2012; 2013-2018) 

data_bins %>%
  group_by(type, six_year_bins) %>% 
  summarize_at("area", sum, na.rm=T) %>% 
  ggplot(aes(fill=type, x=six_year_bins, y=area))+
  geom_bar(position= position_dodge(0.8), stat="identity", width = 0.8, na.rm=TRUE)+
  stat_summary(aes(label = stat(y)), fun.y = 'sum', geom = 'text', col = 'black', position= position_dodge(0.8), vjust = 0.1)+
  scale_x_discrete(na.translate = FALSE)+
  ylim(0,2000000)+
  scale_fill_manual("Land type", values = c("ELC" = "steelblue", "PA" = "Darkkhaki"))+
  labs(title = paste(strwrap("Amount of PA and ELC land omitted from analysis under different proposed temporal periods", width = 50), collapse = "\n"),
       x = "Proposed Period of analysis", 
       y = "Amount of land (ha) omitted")+
  theme(plot.title = element_text(size = rel(1.3), hjust = 0.5),
        axis.title = element_text(size = rel(1.2)),
        panel.background = element_rect(fill = "white", colour = "grey", linetype = "solid"),
        legend.position = c(0.90, 0.4),
        legend.background = element_rect(fill = "white", colour = "Dark grey", linetype = "solid"),
        legend.title = element_text(hjust = 0.5))


#Plot of land excluded using 3 year periods ranging from 2010 to 2018


          data_bins %>%
            group_by(type, three_year_bins) %>% 
            summarize_at("area", sum, na.rm=T) %>% 
            ggplot(aes(fill=type, x=three_year_bins, y=area))+
            geom_bar(position = position_dodge(0.8), stat="identity", width = 0.8, na.rm=TRUE)+
            geom_text(aes(label = ..y..), fun.y ='sum', stat = 'summary', position = position_dodge(width = 0.8), vjust= 0.2)+
            scale_x_discrete(na.translate = FALSE)+
            ylim(0,2000000)+
            scale_fill_manual("Land type", values = c("ELC" = "steelblue", "PA" = "Darkkhaki"))+
            labs(title = paste(strwrap("Amount of PA and ELC land omitted from analysis under different proposed temporal periods", width = 60), collapse = "\n"),
                 x = "Proposed Period of analysis", 
                 y = "Amount of land (ha) omitted")+
            theme(plot.title = element_text(size = rel(1.3), hjust = 0.5),
                  axis.title = element_text(size = rel(1.2)),
                  panel.background = element_rect(fill = "white", colour = "grey", linetype = "solid"),
                  legend.position = c(0.90, 0.4),
                  legend.background = element_rect(fill = "white", colour = "Dark grey", linetype = "solid"),
                  legend.title = element_text(hjust = 0.5))

#Plot of land excluded using 2 year periods ranging from 2011 to 2018
          
          data_bins %>%
            group_by(type, two_year_bins) %>% 
            summarize_at("area", sum, na.rm=T) %>% 
            ggplot(aes(fill=type, x=two_year_bins, y=area))+
            geom_bar(position = position_dodge(0.8), stat="identity", width = 0.8, na.rm=TRUE)+
            geom_text(aes(label = ..y..), fun.y ='sum', stat = 'summary', position = position_dodge(width = 0.8), vjust= 0.2)+
            scale_x_discrete(na.translate = FALSE)+
            ylim(0,2000000)+
            scale_fill_manual("Land type", values = c("ELC" = "steelblue", "PA" = "Darkkhaki"))+
            labs(title = paste(strwrap("Amount of PA and ELC land omitted from analysis under different proposed temporal periods", width = 60), collapse = "\n"),
                 x = "Proposed Period of analysis", 
                 y = "Amount of land (ha) omitted")+
            theme(plot.title = element_text(size = rel(1.3), hjust = 0.5),
                  axis.title = element_text(size = rel(1.2)),
                  panel.background = element_rect(fill = "white", colour = "grey", linetype = "solid"),
                  legend.position = c(0.90, 0.5),
                  legend.background = element_rect(fill = "white", colour = "Dark grey", linetype = "solid"),
                  legend.title = element_text(hjust = 0.5))

#Placing the three plots next to each other.
          
five_year_plot <- data_bins %>%
            group_by(type, six_year_bins) %>% 
            summarize_at("area", sum, na.rm=T) %>% 
            ggplot(aes(fill=type, x=six_year_bins, y=area))+
            geom_bar(position= position_dodge(0.8), stat="identity", width = 0.8, na.rm=TRUE)+
            stat_summary(aes(label = stat(y)), fun.y = 'sum', geom = 'text', col = 'black', position= position_dodge(0.8), vjust = 0.1)+
            scale_x_discrete(na.translate = FALSE)+
            ylim(0,2000000)+
            scale_fill_manual("Land type", values = c("ELC" = "steelblue", "PA" = "Darkkhaki"))+
            labs(title = paste(strwrap("Amount of PA and ELC land omitted from analysis under different proposed temporal periods", width = 80), collapse = "\n"),
                 x = "Proposed Period of analysis", 
                 y = "Amount of land (ha) omitted")+
            theme(plot.title = element_text(size = rel(1.3), hjust = 0.5),
                  axis.title = element_text(size = rel(1.2)),
                  panel.background = element_rect(fill = "white", colour = "grey", linetype = "solid"),
                  legend.position = "none",
                  legend.background = element_rect(fill = "white", colour = "Dark grey", linetype = "solid"),
                  legend.title = element_text(hjust = 0.5))
          
        
          
          three_year_plot <- data_bins %>%
            group_by(type, three_year_bins) %>% 
            summarize_at("area", sum, na.rm=T) %>% 
            ggplot(aes(fill=type, x=three_year_bins, y=area))+
            geom_bar(position = position_dodge(0.8), stat="identity", width = 0.8, na.rm=TRUE)+
            geom_text(aes(label = ..y..), fun.y ='sum', stat = 'summary', position = position_dodge(width = 0.8), vjust= 0.2)+
            scale_x_discrete(na.translate = FALSE)+
            ylim(0,2000000)+
            scale_fill_manual("Land type", values = c("ELC" = "steelblue", "PA" = "Darkkhaki"))+
            labs(title = paste(strwrap("Amount of PA and ELC land omitted from analysis under different proposed temporal periods", width = 60), collapse = "\n"),
                 x = "Proposed Period of analysis", 
                 y = "Amount of land (ha) omitted")+
            theme(plot.title = element_text(size = rel(1.3), hjust = 0.5),
                  axis.title = element_text(size = rel(1.2)),
                  panel.background = element_rect(fill = "white", colour = "grey", linetype = "solid"),
                  legend.position = "none",
                  legend.background = element_rect(fill = "white", colour = "Dark grey", linetype = "solid"),
                  legend.title = element_text(hjust = 0.5))
          
          
          two_year_plot <- data_bins %>%
            group_by(type, two_year_bins) %>% 
            summarize_at("area", sum, na.rm=T) %>% 
            ggplot(aes(fill=type, x=two_year_bins, y=area))+
            geom_bar(position = position_dodge(0.8), stat="identity", width = 0.8, na.rm=TRUE)+
            geom_text(aes(label = ..y..), fun.y ='sum', stat = 'summary', position = position_dodge(width = 0.8), vjust= 0.2)+
            scale_x_discrete(na.translate = FALSE)+
            ylim(0,2000000)+
            scale_fill_manual("Land type", values = c("ELC" = "steelblue", "PA" = "Darkkhaki"))+
            labs(title = paste(strwrap("Amount of PA and ELC land omitted from analysis under different proposed temporal periods", width = 60), collapse = "\n"),
                 x = "Proposed Period of analysis", 
                 y = "Amount of land (ha) omitted")+
            theme(plot.title = element_text(size = rel(1.3), hjust = 0.5),
                  axis.title = element_text(size = rel(1.2)),
                  panel.background = element_rect(fill = "white", colour = "grey", linetype = "solid"),
                  legend.position = "Below",
                  legend.background = element_rect(fill = "white", colour = "Dark grey", linetype = "solid"),
                  legend.title = element_text(hjust = 0.5))     


grid.arrange(five_year_plot, three_year_plot, two_year_plot, ncol= 1)

