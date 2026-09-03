install.packages(c("readxl", "tidyverse"))


library(readxl)
library(tidyverse)

data <- read_excel("dat/small_transect_date_corrected.xls") %>% 
  filter(Genero.sp %in% c("Marmosa_murina", "Didelphis_albiventris", "Marmosa_demerarae")) %>% dplyr::select(c(Genero.sp,Localidade,Data_de_coleta,
                                                                                                               Brinco,codigo, N_de_campo,Faixa_etaria, Sexo, Peso_gramas ,
                                                                                                               # Embrioes ,Vagina,Grávida, Lactante ,
                                                                                                               n_Filhotes , reprodutivo ,
                                                                                                               Habitat_origem, Expedicao,Estacao , Ocasiao , Estrato_arboreo  )) %>%
  rename(Species = Genero.sp )



table(data$Estrato_arboreo)
table(data$Habitat_origem)

data[which((data$Brinco)=="NA"),] %>% View()

table(data$Brinco) %>% as.data.frame()

resighted_individuals <- table(data$N_de_campo) %>% as.data.frame() %>% filter(Freq>1) # there are 167 individuqls resighted 

data %>% filter(N_de_campo %in% resighted_individuals$Var1) %>% filter(Species=="Marmosa_murina" )%>% filter(Localidade =="RPPN_Gargaú")%>%View()

data %>%
  filter(Species == "Marmosa_murina") %>%
  summarise(n_individus = n_distinct(N_de_campo))

# Check values for Marmosa murina

data_Marmosa <- data %>%
  filter(Species == "Marmosa_murina") 

table(data_Marmosa$Sexo)
table(data_Marmosa$Sexo, data_Marmosa$Localidade)

table(data_Marmosa$Faixa_etaria, data_Marmosa$Localidade)

table(data_Marmosa$reprodutivo, data_Marmosa$Localidade,  data_Marmosa$Sexo)


# Check values for Didelphis albiventris 

data_Didelphis <- data %>%
  filter(Species == "Didelphis_albiventris") 

table(data_Didelphis$Sexo)
table(data_Didelphis$Sexo, data_Didelphis$Localidade)

table(data_Didelphis$Faixa_etaria, data_Didelphis$Localidade)

table(data_Didelphis$reprodutivo, data_Didelphis$Localidade,  data_Didelphis$Sexo)
