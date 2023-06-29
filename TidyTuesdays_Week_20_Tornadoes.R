#TidyTuesdays 
#week 20
#Topic:tornadoes
#by:Chauncey Robbs

library(tidyverse)
library(sf)
library(maps)
library(tigris)
library(sf)
library(classInt)
library(ggthemes)
library(tidycensus)
library(scales)
library(ggtext)
library(ggrepel)


#READ IN DATA
tornadoes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-16/tornados.csv')









#CREATE SPATIAL DATA FOR JOIN WITH MISSISSIPPI COUNTIES
tornadoes_sf <- tornadoes |> 
   select(yr,st,loss, slat, slon) |> 
   st_as_sf(coords = c("slon", "slat"),remove = FALSE, crs = 4326) |> 
   st_transform(crs = 2163)
   








#PULL MISSISSPPI COUNTY DATA FOR JOIN WITH TORNADO DATA
mississippi_counties <- tigris::counties(state ="MS") |> st_transform(crs = 2163)







#JOIN TORNADO AND MISSIPPI COUNTY DATA TOGETHER
mississippi_tornadoes <- st_join(mississippi_counties, tornadoes_sf, left = T)







#FIND TOTAL POPULATION VARIABALE
variables <- tidycensus::load_variables(year = 2021,dataset = "acs5/subject")





#CREATE MISSISSIPI POPULATION VARIABLE
mississippi_variables <- c(total_pop="S0101_C01_001")






#PULL MISSISSIPPI COUNTY POPUALTION DATA
mississippi_population <- get_acs(geography = "county", state = "MS",
variables = mississippi_variables,year = 2021,geometry =FALSE )







#JOIN POPULATION DATAT WITH TORNADO DATA
mississippi_pop_tornados_join <- left_join(x = mississippi_population, 
                                           y = mississippi_tornadoes,by ="GEOID")



#CREATE FINAL DATASET WITH TOTAL LOSS AND TOTAL LOSS PER CAPITA
 final_dataset <- mississippi_pop_tornados_join |> st_drop_geometry() |>  select(GEOID,NAME.y,loss,estimate) |> 
   group_by(GEOID,NAME.y,estimate) |> 
   summarise(total_tornados = n(),
             total_loss = sum(loss,na.rm = TRUE),.groups = "drop")|> 
   
   mutate(total_loss = (total_loss),
          total_loss_per_capita = (total_loss/estimate))


 
 
 
#CREATE CASE WHEN FUNCTION TO SET COLORS FOR OUTLIERS
final_dataset <- final_dataset |> mutate(point_colors = 
   case_when(
   NAME.y == "Leake" ~ "#bb3e03",
   NAME.y == "Stone" ~ "#ffc857",
   TRUE ~ "#003049"),
   fill = colorspace::lighten(point_colors,amount = 0.45))





           
  #USE FINAL DATASET TO CREATE PLOT 
  final_dataset |> ggplot(mapping = aes(x= total_tornados,y = total_loss,size = 5))+
   
   
   #ADD HORIZONTAL LINE TO SHOW THE MEDIAN TORNADO LOSS
   geom_hline(yintercept = median(final_dataset$total_loss,na.rm = T), 
              color = "#282828",alpha = .75,linetype =3)+
  
   
   
   #ADD POINT DATA TO PLOT
    geom_point(color = final_dataset$point_colors,fill = final_dataset$fill,
               pch=21,show.legend = F)+
   
   
   #ADD ANNOTATION FOR LEAKE COUNTY
   annotate(geom = "label",x = 57.5,y = 672822000,label = toupper("Leake County"),
               size =3.25,
               family = "sans",
               hjust = "left",
               vjust = "center",
               fill = "#f0f0f0",
               color = "#282828",
               label.size = 0)+
  
   
   
    #ADD ANNOTATION FOR JACKSON COUNTY 
    annotate(geom = "label",x = 20.95,y = 1005000,label = toupper("Stone County"),
            size =3.25,
            family = "sans",
            hjust = "left",
            vjust = "center",
            fill = "#f0f0f0",
            color = "#282828",
            label.padding = unit(.005,"lines"),
            label.size = 0)+
     
   
   
   #ADD ANNOTATION FOR OTHER MISSISSIPPI COUNTIES
   geom_richtext(mapping = aes(x = 70.5,y = 29500000),
            label = toupper("Other Counties <br> In Mississippi"),
            fill = "#f0f0f0",
            color = "#003049",
            family = "sans",
            fontface ="plain",
            alpha =.35,
            size =2.55,
            hjust = "center",
            vjust = "center",
            label.padding = unit(.005,"lines"),
            label.size = 0,
            angle =45)+
     

  
   #ADD ANNOTATION FOR TOTAL DAMAGE FROM TORNADOS
   geom_label(aes(x = 90,y = 10549250,label = toupper("median\n total loss\n$10,549,250"),
                  vjust= "center",
                  hjust = "center", 
                  family = "sans",
                  alpha  = .25),
                  label.padding = unit(.25,"lines"),
                  label.r = unit(0,"lines"),
              label.size = 0,
              size = 2, fill = "#f0f0f0",
              color ="#282828")+
   
   
     theme_fivethirtyeight()+
     scale_y_continuous(labels = dollar, trans = "log10",limits = c(1000000,1000000000))+
     scale_x_continuous(breaks = seq(0,120,20),limits = c(0,120))+
     
     
     #ADD TITLE, LABELS, AND CAPTION TO THE CHART
     ylab(label = "Total Loss (In U.S. Dollars)")+
     xlab(label = "Number of Tornadoes")+
     labs(title = "Tornadoes Leave No County Untouched",
          subtitle = "Total Damages Nearing $1B For Several Mississippi Counties From 1950 - 2022.",
     caption = "#TidyTuesday 2023 - Week 20 | Source: NOAA | By: @ETTS_12")+
     
     
     
     
     #ADJUST THE THEME OF THE LABELS
     theme(axis.title.y = element_text(face = "bold",
                                       size = 7,
                                       color = "#282828",
                                       hjust = .5,
                                       margin = margin(r = 4, l = .5)
                                       ))+
     theme(axis.title.x = element_text(face = "bold",
                                       size = 7,
                                       color = "#282828",
                                       hjust = .5,
                                       margin = margin(t =5, b = -5)))+
     theme(plot.caption = element_text(size = 5,
                                       hjust = 0,
                                       vjust = -1.00,
                                       family = "sans",
                                       colour = "#282828"))+
   
     theme(legend.position = "none")
  
  
   
#PLOT CHART AND SAVE TO LOCALLY
ggsave(filename = "tornadoes.png",
       device = "png",width = 7.5,height = 5 ,units = "in",dpi = 400,plot = last_plot())

 



