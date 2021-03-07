# Code 2: Analysis OHL

# 1. Cargar librerias ----
pacman::p_load(tidyverse, rvest, xml2, lubridate, openxlsx, readxl, ggrepel, tibble, writexl, haven,
               dplyr, car, summarytools, ggpubr, ggplot2, sjmisc, sjlabelled, stargazer, srvyr)
options(scipen=999)

# 2. Cargar data ----
load("input/proc-ohl.RData")

# 3. Subset ----
proc_ohl <- proc_ohl[-c(1,4,5,7)]

# 4. Cuadros ----
# 4.1. Cuadro 1 legal/privado ----
a <- proc_ohl %>% group_by(ano) %>% filter(institution==1 & legalidad==1) %>% select(ano, trab_comprometidos)

cuadro_1 <- a%>%group_by(ano)%>%summarise(freq_legal = sum(trab_comprometidos, na.rm = T))

# 4.2. Cuadro 2 extralegal/privado 
b <- proc_ohl %>% group_by(ano) %>% filter(institution==1 & legalidad==2) %>% select(ano, trab_comprometidos)

cuadro_2 <- b %>% group_by(ano) %>% summarise(freq_extraleg = sum(trab_comprometidos, na.rm = T))

cuadro_3 <- merge(cuadro_1, cuadro_2, by="ano",all.x = TRUE)

cuadro_3$freq_extraleg <- car::recode(cuadro_3$freq_extraleg, "NA = 0")

cuadro_3 <- cuadro_3 %>% mutate(total_tc=(freq_legal+freq_extraleg)) %>% as.data.frame()

# 4.3. Cantidad huelgas privado ---- 
c <- proc_ohl %>% group_by(ano) %>% filter(institution==1 & legalidad==1) %>% tally() 
d <- proc_ohl %>% group_by(ano) %>% filter(institution==1 & legalidad==2) %>% tally()

cuadro_4 <- merge(c,d, by = "ano", all.x = TRUE)

names(cuadro_4) <- c("ano", "totn_legal", "totn_extraleg")
cuadro_4$totn_extraleg <- car::recode(cuadro_4$totn_extraleg, "NA = 0")

cuadro_4 <- cuadro_4 %>% mutate(total_huelgas=(totn_legal+totn_extraleg)) %>% as.data.frame()

# 4.4. Cuadro tc legal y extralegal ----
e <- proc_ohl %>% group_by(ano) %>% filter(legalidad==1) %>% summarise(freq_leg = sum(trab_comprometidos, na.rm = T))

f <- proc_ohl %>% group_by(ano) %>% filter(legalidad==2) %>% summarise(freq_extraleg = sum(trab_comprometidos, na.rm = T))

cuadro_5 <- merge(e,f, by = "ano", all.x = TRUE)

# 4.5. Cuadro cantidad huelgas y tc privado y publico ----
g <- proc_ohl %>% group_by(ano) %>% filter(institution==1) %>% summarise(tc_priv = sum(trab_comprometidos, na.rm = T))

h <- proc_ohl %>% group_by(ano) %>% filter(institution==2) %>% summarise(tc_pub = sum(trab_comprometidos, na.rm = T))

cuadro_6 <- merge(g,h, by = "ano", all.x = TRUE)

cuadro_6$tc_pub <- car::recode(cuadro_6$tc_pub, "NA = 0")

cuadro_6 <- cuadro_6 %>% mutate(total_tc=(tc_pub+tc_priv)) %>% as.data.frame()

i <- proc_ohl %>% group_by(ano) %>% filter(institution==1) %>% tally()

j <- proc_ohl %>% group_by(ano) %>% filter(institution==2) %>% tally()

cuadro_7 <- merge(i,j, by = "ano", all.x = TRUE)

names(cuadro_7) <- c("ano", "totn_priv", "totn_pub") 

cuadro_7$totn_pub <- car::recode(cuadro_7$totn_pub, "NA = 0")

cuadro_7 <- cuadro_7 %>% mutate(total_huelgas = (totn_pub+ totn_priv)) %>% as.data.frame()

# 5. Export ---- 
write_xlsx(cuadro_5,"output/tc_legal_extralegal.xlsx", col_names = TRUE,format_headers = TRUE)
write_xlsx(cuadro_6,"output/tc_privado_publico.xlsx", col_names = TRUE,format_headers = TRUE)
write_xlsx(cuadro_7,"output/huelgas_privado_publico.xlsx", col_names = TRUE,format_headers = TRUE)
write_xlsx(cuadro_3,"output/tc_legextraleg_privado.xlsx", col_names = TRUE,format_headers = TRUE)
write_xlsx(cuadro_4,"output/huelgas_legextraleg_privado.xlsx", col_names = TRUE,format_headers = TRUE)