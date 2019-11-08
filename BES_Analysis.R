library(readxl)
library(lubridate)
library(reshape2)
library(tidyverse)
library(scales)

setwd("C:/Users/Kafein/Documents/R Scripts/BES")
wd <- getwd()

ext <- read_excel("31_07_2019_Oncesi_Rapor_Datasi_BES.xlsx")

ext2 <- ext %>%   mutate_if(~sum(is.na(.x)) > 0,
                    ~if_else(is.na(.x), "_", as.character(.x))) 

ext2$ACIKLAMA <- paste0(ext2$TITLE2,ext2$ACIKLAMA)

ext2 <- ext2[rowSums(is.na(ext)) != ncol(ext), ]
ext2 <- ext2[ ,!(names(ext2) %in% "TITLE2")]
ext2 %>% mutate(RAPORTAR = ymd(RAPORTAR))
ext2$DEGER <- sapply(ext2$DEGER,as.numeric)

ext2 <- dcast(ext2,RAPORTAR+SIRKET ~ACIKLAMA, value.var = "DEGER", fun.aggregate = sum)

names(ext2) <- c("Rapor_tarihi",                                                               
                 "Sirket",                                                                 
                 "Devlet Katkisi Fon Birikimi (TL)",                                      
                 "Devlet Katkisi Fon Tutari (TL)",                                        
                 "Emekli Olan Katilimci Sayisi",                                          
                 "Katilimci Sayisi",                                                      
                 "Katilimcilarin Fon Tutari (TL)",                                        
                 "Katki Payi Tutari (TL)",                                                
                 "SS_Emeklilik Sözlesmeleri",            
                 "SS_Gruba Bagli Bireysel Emeklilik Sözlesmeleri",
                 "SS_Grup Emeklilik Sözlesmeleri",                
                 "SS_Isveren Grup Emeklilik Sertifikalari",       
                 "SS_Toplam",                                     
                 "YYT_Bireysel Emeklilik Sözlesmeleri (TL)",            
                 "YYT_Gruba Bagli Bireysel Emeklilik Sözlesmeleri (TL)",
                 "YYT_Grup Emeklilik Sözlesmeleri (TL)",                
                 "YYT_Isveren Grup Emeklilik Sertifikalari (TL)",       
                 "YYT_Toplam (TL)" )

`%nin%` = Negate(`%in%`)

ext_katilimci <- ext2 %>%  
  mutate(YIL = format(as.Date(Rapor_tarihi), "%Y")) %>% 
  select(Sirket, YIL, `Katilimci Sayisi`) %>% 
  filter(YIL %nin% c(2003,2019)) %>% 
  group_by(YIL) %>% 
  summarize(TOPLAM=sum(`Katilimci Sayisi`))

#Plot1

ggplot(ext_katilimci, aes(x=YIL, 
                          y=TOPLAM, 
                          group=1)) +
  geom_line(color="red")+
  geom_point(color="red")+
  labs(title = "BES", 
       subtitle = "Katilimci Sayisi", 
       caption="(based on data from EGM)")+
  scale_y_continuous(labels = comma) +
  theme(axis.text.x = element_text(angle= 35, vjust= 0.5))

ext_katilimci_sirket <- ext2 %>%  
  mutate(YIL = year(Rapor_tarihi)) %>% 
  select(Sirket, YIL, `Katilimci Sayisi`) %>% 
  filter(YIL %nin% c(2003,2019)) %>% 
  group_by(Sirket) %>% 
  summarize(TOPLAM=sum(`Katilimci Sayisi`)) %>% 
  mutate(TOPLAM=round(TOPLAM/1000000))

ext_katilimci_fon_ilk5 <- ext2 %>% 
  mutate(YIL = year(Rapor_tarihi)) %>% 
  select(Sirket, YIL, `Katilimcilarin Fon Tutari (TL)`) %>% 
  filter(YIL %nin% c(2003,2019)) %>% 
  group_by(Sirket) %>% 
  summarize(TOPLAM=sum(`Katilimcilarin Fon Tutari (TL)`)) %>% 
  arrange(desc(TOPLAM)) %>% slice(1:5) %>% mutate(TOPLAM=round(TOPLAM/1000000))

katilimci_fon_2 <- ext_katilimci_fon_ilk5 %>% 
  left_join(ext_katilimci_sirket,
            by = c("Sirket"), 
            suffix=c("_FON_TUTARI", "_katilimci_sayisi"))

#Plot2
ggplot(katilimci_fon_2, 
       aes(x=reorder(Sirket,TOPLAM_FON_TUTARI),
           y=TOPLAM_FON_TUTARI,
           size=TOPLAM_katilimci_sayisi)) +
  geom_point(alpha=0.5) + 
  labs(title = "BES", 
       subtitle = "Katilimci Sayisi ve Fon Tutari", 
       x="Sirket", 
       y="Fon Tutari",
       caption="(based on data from EGM)") +
  scale_size(range = c(8, 20), 
             name="Katılımcı Sayısı") +
  scale_y_continuous(labels = comma, 
                     limits=c(500000,5000000)) +
  theme(axis.text.x = element_text(angle=35, vjust=0.6)) +
  scale_x_discrete(labels=c("Vakıf Emeklilik" = "Vakıf", 
                            "Allianz Yaşam ve Emeklilik" = "Allianz",
                            "Garanti Emeklilik ve Hayat" = "Garanti", 
                            "Anadolu Hayat Emeklilik"="Anadolu",
                            "Avivasa Emeklilik ve Hayat"= "Avivasa"))



