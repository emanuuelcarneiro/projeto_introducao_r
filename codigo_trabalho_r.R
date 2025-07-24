

# instalação do pacote ----------------------------------------------------

 
install.packages("COVIDIBGE") 



# liberando o pacote ------------------------------------------------------


library(COVIDIBGE)
library(tidyverse) 


# visualizando os dados através de uma tabela -----------------------------


dados <- get_covid(year = 2020, month = 6, design = FALSE)


# selecionando os dados que iremos usar nas analises ----------------------

dados_brutos<-dados |> 
  select(UF,CAPITAL,V1022,V1032,A002,A003,A004,
                   A005,C007C,C013,C016,D0051) |>
  print(n=100) 


# SOMA DOS NA nas colunas -------------------------------------------------

dados_brutos |>
  summarise(soma_NA=sum(is.na(C013)), n=n())


# selecionando os sintomas ------------------------------------------------


sintomas<-dados |> 
  select(B0011,B0012,B0013,B0014,B0015,B0016,B0017,B0018,B0019,
         B00110,B00111,B00112)


# selecionando a providencia tomada ---------------------------------------


providencia<-dados |> 
  select(B002,B0031,B0032,B0033,B0034,B0035,B0036,B0037)


# fazer a tabela,ver o tipo da variavel. ----------------------------------

 


# renomeando as colunas ---------------------------------------------------


dados_brutos<-dados_brutos |> 
  rename(estado         = UF,
         capital        = CAPITAL,
         situ_domicilio = V1022,
         peso           = V1032,
         idade          = A002,
         sexo           = A003,
         raça           = A004,
         escolaridade   = A005,
         função_cargo   = C007C,
         trabalho       = C013,
         motivo_ñprocurar_emprego = C016,
         auxilio_emergencial = D0051)


# renoemando as counas de sintoma -----------------------------------------


sintomas<-sintomas|>
  rename(febre = B0011,
         tosse = B0012,
         dor_garganta = B0013,
         dificuldade_respiratoria = B0014,
         dor_cabeça = B0015,
         dor_peito = B0016,
         nauseas = B0017,
         nariz_entupido = B0018,
         fadiga = B0019,
         dor_olhos = B00110,
         perda_paladar_ofato = B00111,
         dor_muscular = B00112)


# renomeando as providencias ----------------------------------------------


providencia<-providencia|> 
  rename(procurou_ajuda = B002,
         ficou_em_casa=B0031,
         ligou_profissional=B0032,
         automedicação=B0033,
         medicação_orientação_medica=B0034,
         visita_sus=B0035,
         visita_particular=B0036,
         outra_providencia=B0037)


# juntando as tabelas -----------------------------------------------------

 
library(dplyr)
dados_quase_limpos<- bind_rows(dados_brutos, sintomas, providencia)

