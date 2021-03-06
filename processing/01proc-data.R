# Code 1: Process OHL 

# 1. Cargar librerias ----
pacman::p_load(tidyverse, rvest, xml2, lubridate, openxlsx, readxl, ggrepel, tibble, writexl, haven,
               dplyr, car, summarytools, ggpubr, sjmisc, sjlabelled, stargazer, srvyr, devtools, magrittr, 
               magrittr, sjPlot)
options(scipen=999)

# 2. Cargar data ---- 
ohl<-readWorkbook("input/Labor_Strikes_Dataset_1979_2018_Public.xlsx", detectDates=TRUE)

# 3. Procesamiento ---- 
ohl$ciuur2<-as.factor(ohl$ciuur2)
ohl$ciuur3<-as.factor(ohl$ciuur3)
ohl$leg<-as.factor(ohl$leg)

ohl<-ohl %>% mutate(sector=case_when(ciuur2==1 ~ "A Agriculture",
                                     ciuur2==2 ~ "B Mining",
                                     ciuur2==3 ~ "C Manufacturing industry",
                                     ciuur2==4 ~ "D-E Electricity, Water and Sanitary Services",
                                     ciuur2==5 ~ "F Construction",
                                     ciuur2==6 ~ "G-I Commerce",
                                     ciuur2==7 ~ "H-J Transportation and Communication",
                                     ciuur2==8 ~ "L-K Banks and Financial Services",
                                     ciuur2==9 ~  "O Central, Regional and Municipal Government",
                                     ciuur2==10 ~ "P Education (private, public and municipalized)",
                                     ciuur2==11 ~ "Q Health (private, public and municipalized)",
                                     ciuur2==12 ~ "Q Other Community, Social and Personal Services",
                                     ciuur2==13 ~ "Unknown or Other Activities",
                                     ciuur2==14 ~ "Unknown or Other Activities",
                                     is.na(ciuur2)&ciuur4==1 ~ "A Agriculture",
                                     is.na(ciuur2)&ciuur4==2  ~ "B Mining",
                                     is.na(ciuur2)&ciuur4==3  ~ "C Manufacturing industry",
                                     is.na(ciuur2)&ciuur4==4  ~ "D-E Electricity, Water and Sanitary Services",
                                     is.na(ciuur2)&ciuur4==5  ~ "D-E Electricity, Water and Sanitary Services",
                                     is.na(ciuur2)&ciuur4==6  ~ "F Construction",
                                     is.na(ciuur2)&ciuur4==7  ~ "G-I Commerce",
                                     is.na(ciuur2)&ciuur4==8  ~ "H-J Transportation and Communication",
                                     is.na(ciuur2)&ciuur4==9  ~ "G-I Commerce",
                                     is.na(ciuur2)&ciuur4==10 ~ "H-J Transportation and Communication",
                                     is.na(ciuur2)&ciuur4==11 ~ "L-K Banks and Financial Services",
                                     is.na(ciuur2)&ciuur4==12 ~ "L-K Banks and Financial Services",
                                     is.na(ciuur2)&ciuur4==13 ~ "Q Other Community, Social and Personal Services",
                                     is.na(ciuur2)&ciuur4==14 ~ "Q Other Community, Social and Personal Services",
                                     is.na(ciuur2)&ciuur4==15 ~ "O Central, Regional and Municipal Government",
                                     is.na(ciuur2)&ciuur4==16 ~ "P Education (private, public and municipalized)",
                                     is.na(ciuur2)&ciuur4==17 ~ "Q Health (private, public and municipalized)",
                                     is.na(ciuur2)&ciuur4==18 ~ "Q Other Community, Social and Personal Services",
                                     is.na(ciuur2)&ciuur4==19 ~ "Q Other Community, Social and Personal Services",
                                     is.na(ciuur2)&ciuur4==20 ~ "Unknown or Other Activities",
                                     is.na(ciuur2)&ciuur4==21 ~ "Unknown or Other Activities"))

ohl %>% subset(is.na(ciuur2)) %>% select(yr,ciuur2,ciuur3,ciuur4,sector)

proc_ohl <- ohl %>% select(organizacion = org, 
                         legalidad= leg, 
                         ano= yr, 
                         sector = sector,
                         tot_trabajadores = trabemp,
                         trab_comprometidos = tc,
                         tactica = tactica1, 
                         institution = inst)

sapply(proc_ohl, class)
names(proc_ohl)
head(proc_ohl)

# 3.1. Descriptivos ---- 
freq(proc_ohl$organizacion)
freq(proc_ohl$legalidad)
freq(proc_ohl$ano)
freq(proc_ohl$sector)
freq(proc_ohl$tot_trabajadores)
freq(proc_ohl$trab_comprometidos)
freq(proc_ohl$tactica)
freq(proc_ohl$institution)

sjmisc::descr(proc_ohl)

# 3.2. Recodificacion ---- 
proc_ohl$organizacion <- car::recode(proc_ohl$organizacion,"0 = 1; c(1,2,3) = 2; 4 = 2; 5 = 2; c(6,7) = 2; 8 = 2; 9 = 1; 10 = 1; 11 = 2; NA = NA", as.factor = T) #  1=Ausencia sindicato, 2= Presencia sindicato
proc_ohl$legalidad <- car::recode(proc_ohl$legalidad,"1 = 1; 2 = 2", as.factor = T) #1=Legal 2=Extralegal
proc_ohl$ano <- as.factor(proc_ohl$ano)
proc_ohl$tot_trabajadores <- as.numeric(proc_ohl$tot_trabajadores)
proc_ohl$trab_comprometidos <- as.numeric(proc_ohl$trab_comprometidos)
proc_ohl$tactica <- car::recode(proc_ohl$tactica,"c(1,2,3,4) = 1; c(5,6,7) = 1; 8 = 1; c(9,10) = 1; c(11,12) = 1; c(13,14) = 1;
                                15 = 1; c(16,17) = 1; c(18,19,20,21,22,23,24) = 2; c(25,26,27,28,29,30,31,32,33) = 3;
                                c(34,35) = 1; 36 = 1; c(37,38,39) = 2; 40 = 1; 41 = 2; 42 = 1; 43 = 2; 44 = 1; 45 = 1; 46 = 2; 
                                c(47,48,49) = 1", as.factor = T) # 1=Públicas, Convencionales y culturales, 2=Disruptivas y 3=Violentas (Fuente: OHL)
proc_ohl$tactica <- factor(proc_ohl$tactica,levels=c(1,2,3),labels=c("1","2","3"))
proc_ohl$institution <- car::recode(proc_ohl$institution, "c(1,2) = 1; 3 = 2; 41 = 2; 42 = 2; 43 = 2; c(5,6) = 2; 7 = NA; 8 = 1; 9 = NA", as.factor = T)
proc_ohl$sector <- as.factor(proc_ohl$sector)

sapply(proc_ohl, class)

# 4. Export ----
save(proc_ohl, file= "input/proc-ohl.RData")