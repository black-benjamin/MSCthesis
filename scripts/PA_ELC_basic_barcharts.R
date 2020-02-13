library(tidyverse)
library(gridExtra)

#ggplot2 function for creating a barplot with Y axis counting one variables and stack colour distribution by a second variable. 
#Overlapping legend.Manual colour scheme for different PA designations
#This barplot is no. of PAs of different categories by establishment year 

Filtered_PAs %>% 
  ggplot(aes(fill=category, x=factor(Original_establishment_year)))+
  geom_bar(position="stack", stat="count", width= 0.95)+
  scale_fill_manual("PA Designation", values = c("Wildlife Sanctuary" = "mediumseagreen",
                                                 "National Park" = "Forest Green", "Protected Landscape" = "Darkkhaki",
                                                 "Multiple Use Management Area" = "indianred3", "Ramsar Site" = "darkseagreen1",
                                                 "Natural Heritage Site" = "bisque3"))+
  geom_text(stat ="count", position = position_stack(vjust = 0.5), aes(label=..count..), color="Black", size=3.5)+
  labs(title = paste(strwrap("Establishment dates and numbers of PAs in filtered dataset", width = 60), collapse = "\n"),
       x = "Year of establishment", 
       y = "No. of PAs")+
  theme(plot.title = element_text(size = rel(1.3), hjust = 0.5),
        axis.text.x = element_text(angle = 90, hjust = 1),
        axis.title = element_text(size = rel(1.2)),
        panel.background = element_rect(fill = "white", colour = "grey", linetype = "solid"),
        legend.position = c(0.80, 0.7),
        legend.background = element_rect(fill = "white", colour = "Dark grey", linetype = "solid"),
        legend.title = element_text(hjust = 0.5))

#this chunk below is for a similar stack bar plot but instead using the sum of a 2nd variable on the Y axis. In this case the total amount of land established
#in each PA category type per year. 

Filtered_PAs %>%
  group_by(Original_establishment_year, category) %>% 
  summarize_at("calculated_area_ha", sum, na.rm=T) %>% 
  ggplot(aes(fill=category, x= factor(Original_establishment_year), y = calculated_area_ha))+
  geom_bar(stat="identity", width= 0.95, na.rm=TRUE)+
  scale_fill_manual("PA Designation", values = c("Wildlife Sanctuary" = "mediumseagreen",
                                                 "National Park" = "Forest Green", "Protected Landscape" = "Darkkhaki",
                                                 "Multiple Use Management Area" = "indianred3", "Ramsar Site" = "darkseagreen1",
                                                 "Natural Heritage Site" = "bisque3"))+
  labs(title = paste(strwrap("Amount in hectares of Protected Areas designated in Cambodia per year in filtered dataset", width = 60), collapse = "\n"),
       x = "Year of establishment", 
       y = "Amount of PA land Designated (ha)")+
  theme(plot.title = element_text(size = rel(1.3), hjust = 0.5),
        axis.text.x = element_text(angle = 90, hjust = 1),
        axis.title = element_text(size = rel(1.2)),
        panel.background = element_rect(fill = "white", colour = "grey", linetype = "solid"),
        legend.position = c(0.78, 0.7),
        legend.background = element_rect(fill = "white", colour = "Dark grey", linetype = "solid"),
        legend.title = element_text(hjust = 0.5))

#Below is a barplot of the number of ELCS established per year grouped by whether or not they were located inside PA's or not. 

ELCS_PAs_associated %>% 
ggplot(aes(fill= Inside_PA , x=factor(year_establishment)))+
  geom_bar(position="stack", stat="count", width= 0.95)+
  scale_fill_manual("ELC location", values = c("Y" = "steelblue", "N" = "mediumseagreen"), labels = c("Outside PAs", "Inside PAs"))+
  geom_text(stat ="count", position = position_stack(vjust = 0.5), aes(label=..count..), color="Black", size=3.0)+
  labs(title = paste(strwrap("No.of ELCS established in Protected Areas in Cambodia per year", width = 70), collapse = "\n"),
       x = "Year of establishment", 
       y = "No. of ELCs")+
  theme(plot.title = element_text(size = rel(1.3), hjust = 0.5),
        axis.text.x = element_text(angle = 90, hjust = 1),
        axis.title = element_text(size = rel(1.2)),
        panel.background = element_rect(fill = "white", colour = "grey", linetype = "solid"),
        legend.position = c(0.88, 0.7),
        legend.background = element_rect(fill = "white", colour = "Dark grey", linetype = "solid"),
        legend.title = element_text(hjust = 0.5))

#Similar to the 2nd PA bar plot above this chunk is for a stacked bar plot of the amount of land area
#established as ELCS per year grouped by ELCS being inside or outside of PAs. 

ELCS_PAs_associated %>%
  group_by(year_establishment, Inside_PA) %>% 
  summarize_at("area_ha", sum, na.rm=T) %>% 
  ggplot(aes(fill= Inside_PA, x=factor(year_establishment), y = area_ha))+
  geom_bar(stat="identity", width= 0.95, na.rm=TRUE)+
    scale_fill_manual("ELC location", values = c("Y" = "steelblue", "N" = "mediumseagreen"),
                      labels = c("Outside PAs","Inside PAs"))+
    labs(title = paste(strwrap("Total area of ELCS established in Cambodia per year", width = 70), collapse = "\n"),
         x = "Year of establishment", 
         y = "Total area of ELCs (ha)")+
    theme(plot.title = element_text(size = rel(1.3), hjust = 0.5),
          axis.text.x = element_text(angle = 90, hjust = 1),
          axis.title = element_text(size = rel(1.1)),
          panel.background = element_rect(fill = "white", colour = "grey", linetype = "solid"),
          legend.position = c(0.88, 0.7),
          legend.background = element_rect(fill = "white", colour = "Dark grey", linetype = "solid"),
          legend.title = element_text(hjust = 0.5))

#Combining plots on single page for quick output

PA_num <- Filtered_PAs %>% 
  ggplot(aes(fill=category, x=factor(Original_establishment_year)))+
  geom_bar(position="stack", stat="count", width= 0.95)+
  scale_fill_manual("PA Designation", values = c("Wildlife Sanctuary" = "mediumseagreen",
                                                 "National Park" = "Forest Green", "Protected Landscape" = "Darkkhaki",
                                                 "Multiple Use Management Area" = "indianred3", "Ramsar Site" = "darkseagreen1",
                                                 "Natural Heritage Site" = "bisque3"))+
  geom_text(stat ="count", position = position_stack(vjust = 0.5), aes(label=..count..), color="Black", size=2.5)+
  labs(title = paste(strwrap("Establishment dates and numbers of PAs in filtered dataset", width = 50), collapse = "\n"),
       x = "Year of establishment", 
       y = "No. of PAs")+
  theme(plot.title = element_text(size = rel(1.1), hjust = 0.5),
        axis.text.x = element_text(angle = 90, hjust = 1),
        axis.title = element_text(size = rel(1.1)),
        panel.background = element_rect(fill = "white", colour = "grey", linetype = "solid"),
        legend.position = "none",
        legend.background = element_rect(fill = "white", colour = "Dark grey", linetype = "solid"),
        legend.title = element_text(hjust = 0.5),
        text = element_text(size= 8))


PA_area <- Filtered_PAs %>%
  group_by(Original_establishment_year, category) %>% 
  summarize_at("calculated_area_ha", sum, na.rm=T) %>% 
  ggplot(aes(fill=category, x= factor(Original_establishment_year), y = calculated_area_ha))+
  geom_bar(stat="identity", width= 0.95, na.rm=TRUE)+
  scale_fill_manual("PA Designation", values = c("Wildlife Sanctuary" = "mediumseagreen",
                                                 "National Park" = "Forest Green", "Protected Landscape" = "Darkkhaki",
                                                 "Multiple Use Management Area" = "indianred3", "Ramsar Site" = "darkseagreen1",
                                                 "Natural Heritage Site" = "bisque3"))+
  labs(title = paste(strwrap("Amount in hectares of Protected Areas designated in Cambodia per year in filtered dataset", width = 50), collapse = "\n"),
       x = "Year of establishment", 
       y = "Amount of PA land Designated (ha)")+
  guides(shape = guide_legend(override.aes = list(size = 0.4)), color = guide_legend(override.aes = list(size = 0.4)))+
  theme(plot.title = element_text(size = rel(1.1), hjust = 0.5),
        axis.text.x = element_text(angle = 90, hjust = 1),
        axis.title = element_text(size = rel(1.1)),
        panel.background = element_rect(fill = "white", colour = "grey", linetype = "solid"),
        legend.position = "right",
        legend.background = element_rect(fill = "white", colour = "Dark grey", linetype = "solid"),
        legend.title = element_text(hjust = 0.5, size = 7),
        text = element_text(size= 8),
        legend.text = element_text(size = 6))
 

ELC_num <- ELCS_PAs_associated %>% 
  ggplot(aes(fill= Inside_PA , x=factor(year_establishment)))+
  geom_bar(position="stack", stat="count", width= 0.95)+
  scale_fill_manual("ELC location", values = c("Y" = "steelblue", "N" = "mediumseagreen"), labels = c("Outside PAs", "Inside PAs"))+
  geom_text(stat ="count", position = position_stack(vjust = 0.5), aes(label=..count..), color="Black", size=2.5)+
  labs(title = paste(strwrap("No.of ELCS established in Protected Areas in Cambodia per year", width = 50), collapse = "\n"),
       x = "Year of establishment", 
       y = "No. of ELCs")+
  theme(plot.title = element_text(size = rel(1.1), hjust = 0.5),
        axis.text.x = element_text(angle = 90, hjust = 1),
        axis.title = element_text(size = rel(1.1)),
        panel.background = element_rect(fill = "white", colour = "grey", linetype = "solid"),
        legend.position = "none",
        legend.background = element_rect(fill = "white", colour = "Dark grey", linetype = "solid"),
        legend.title = element_text(hjust = 0.5),
        text = element_text(size= 8))


ELC_area <- ELCS_PAs_associated %>%
  group_by(year_establishment, Inside_PA) %>% 
  summarize_at("area_ha", sum, na.rm=T) %>% 
  ggplot(aes(fill= Inside_PA, x=factor(year_establishment), y = area_ha))+
  geom_bar(stat="identity", width= 0.95, na.rm=TRUE)+
  scale_fill_manual("ELC location", values = c("Y" = "steelblue", "N" = "mediumseagreen"),
                    labels = c("Outside PAs","Inside PAs"))+
  labs(title = paste(strwrap("Total area of ELCS established in Cambodia per year", width = 70), collapse = "\n"),
       x = "Year of establishment", 
       y = "Total area of ELCs (ha)")+
  guides(shape = guide_legend(override.aes = list(size = 0.5)), color = guide_legend(override.aes = list(size = 0.5)))+
  theme(plot.title = element_text(size = rel(1.1), hjust = 0.7),
        text = element_text(size= 8),
        axis.text.x = element_text(angle = 90, hjust = 1),
        axis.title = element_text(size = rel(1.1)),
        panel.background = element_rect(fill = "white", colour = "grey", linetype = "solid"),
        legend.position = "right",
        legend.background = element_rect(fill = "white", colour = "Dark grey", linetype = "solid"),
        legend.title = element_text(hjust = 0.5, size = 7),
        legend.text = element_text(size = 6))


        
grid.arrange(PA_num,PA_area, ELC_num,  ELC_area, ncol=2, heights= c(2.0, 2.0), widths= c(1.6, 2.0))












