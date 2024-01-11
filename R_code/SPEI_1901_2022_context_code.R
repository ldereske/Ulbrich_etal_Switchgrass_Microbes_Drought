#Beguería S. (2017) SPEIbase: R code used in generating the SPEI global database, doi:10.5281/zenodo.834462.


#Beguería, S., Vicente-Serrano, S.M. y Angulo, M., (2010): A multi-scalar global drought data set: the SPEIbase: A new gridded product for the analysis of drought variability and impacts. Bulletin of the American Meteorological Society. 91, 1351-1354
#Vicente-Serrano, S.M., Beguería, S., López-Moreno, J.I., Angulo, M., El Kenawy, A. (2010): A new global 0.5° gridded dataset (1901-2006) of a multiscalar drought index: comparison with current drought index datasets based on the Palmer Drought Severity Index. Journal of Hydrometeorology. 10: 1033-1043



#Downloading and processing SPEI data for 
#Climate Transitions and
#Shifting subordinates



library(tidync)
library(dplyr)
library(ggplot2)
library(here)
library(cowplot)
library("wesanderson")
#https://digital.csic.es/handle/10261/288226
Global_SPEI_nc_tidy <- tidync(here::here("R_data","spei01.nc"))
print(Global_SPEI_nc_tidy)



SPEI_GLBRC_BCSE<-
  Global_SPEI_nc_tidy|>
  hyper_filter(lat=lat==nth(lat,which.min(abs(lat-42.39518))),
               lon=lon==nth(lon,which.min(abs(lon--85.37339))))|>
  hyper_tibble(select_var = "spei")


SPEI_GLBRC_BCSE<-
  SPEI_GLBRC_BCSE|>
  arrange(time)|>
  mutate(year=c(rep(1901, 12),rep(1902:2021, each=12)),
         month=c(seq(from=1,to=12), rep(seq(from=1,to=12), times= (2021-1901))))|>
  mutate(Rdate=paste(month,"1",year,sep="/"))|>
  mutate(Rdate=as.Date(Rdate,"%m/%d/%Y"))

head(SPEI_GLBRC_BCSE)

summary(SPEI_GLBRC_BCSE$Rdate)

dim(SPEI_GLBRC_BCSE)
#1451    7


#Subset To have only May and October 

SPEI_GLBRC_BCSE_sub<-
  SPEI_GLBRC_BCSE|>
  filter(month==5|
           month==10)|>
  mutate(Str_month=case_when(month==5~"May",
                             month==10~"October"))
dim(SPEI_GLBRC_BCSE_sub)
#242   8

#May 2018 
#Nov 2021
wes_palette("Zissou1")
year_sample_v_line<-
  data.frame(Str_month=c("May","October"),
             sampling_SPEI=c(SPEI_GLBRC_BCSE_sub[with(SPEI_GLBRC_BCSE_sub,year==2018&month==5),]$spei,
                             SPEI_GLBRC_BCSE_sub[with(SPEI_GLBRC_BCSE_sub,year==2021&month==10),]$spei),
             sample_label=c("Greenhouse collection\n2018",
                            "Growth chamber collection\n2021"))

(on_month_SPEI_p=ggplot(SPEI_GLBRC_BCSE_sub,
       aes(x=spei))+
  geom_histogram()+
  geom_vline(data = year_sample_v_line,
             aes(xintercept=sampling_SPEI,
                 color=sample_label),
             linewidth=3)+
  geom_label(data = year_sample_v_line,
             aes(x=sampling_SPEI-0.02, label=sample_label,
                 y=10,color=sample_label),
             hjust=1,size=6)+
    scale_fill_manual(values = wes_palette("Zissou1"))+
  labs(x="One month SPEI (1901-2021)",y="Frequency")+
  facet_wrap(~Str_month, nrow = 2)+
  theme_cowplot(font_size = 26)+
    theme(legend.position = "none"))
  
ggsave(here::here("R_fig","one_month_SPEI_p_histogram.png"),on_month_SPEI_p,width =12,height = 10,device="png")
