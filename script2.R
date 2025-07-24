library(COVIDIBGE)
library(tidyverse) 

dados_junho <- get_covid(year = 2020, month = 6, design = FALSE)

write.csv2(dados_junho, "dados_junho.csv")
dados_junho <- read_csv("/home/emanuuel-carneiro/Downloads/Projeto_covid_pnad_mes06/Projeto_covid_pnad_mes06/dados/dados_junho.csv")



# selecionando as variaveis -----------------------------------------------

dados_junho <- dados_junho |> 
  select(
    UF,CAPITAL,V1022,V1032,A002,A003,A004,
    A005,C007C,C013,C016,D0051,B0011,B0012,B0013,B0014,B0015,B0016,B0017,B0018,B0019,
    B00110,B00111,B00112,B002,B0031,B0032,B0033,B0034,B0035,B0036,B0037, C007, C007B,
    F001, F0022, C01012
  )


# renomeando as variaveis -------------------------------------------------

dados_junho <- dados_junho |> 
  rename(
    estado                      = UF,
    capital                     = CAPITAL,
    situ_domicilio              = V1022,
    peso                        = V1032,
    idade                       = A002,
    sexo                        = A003,
    raça                        = A004,
    escolaridade                = A005,
    função_cargo                = C007C,
    trabalho                    = C013,
    motivo_ñprocurar_emprego    = C016,
    auxilio_emergencial         = D0051,
    febre                       = B0011,
    tosse                       = B0012,
    dor_garganta                = B0013,
    dificuldade_respiratoria    = B0014,
    dor_cabeça                  = B0015,
    dor_peito                   = B0016,
    nauseas                     = B0017,
    nariz_entupido              = B0018,
    fadiga                      = B0019,
    dor_olhos                   = B00110,
    perda_paladar_ofato         = B00111,
    dor_muscular                = B00112,
    procurou_ajuda              = B002,
    ficou_em_casa               = B0031,
    ligou_profissional          = B0032,
    automedicação               = B0033,
    medicação_orientação_medica = B0034,
    visita_sus                  = B0035,
    visita_particular           = B0036,
    outra_providencia           = B0037, 
    ocupacao_princial           = C007, #
    vinculo_empregatico         = C007B, #
    domicilio                   = F001, #
    valor_aluguel               = F0022, 
    faixa_salarial              = C01012 
    
  ) 


dados_junho <- dados_junho |> mutate(valor_aluguel = as.factor(valor_aluguel),trabalho = fct_recode(trabalho, "Home Office" = "Sim", "Presencial" = "Não"),
                      vinculo_empregatico = fct_recode(vinculo_empregatico, "CLT" = "Sim, tem carteira de trabalho assinada", "Servidor Público" = "Sim, é servidor público estatutário", "Desempregado" = "Não"),
                      valor_aluguel = fct_recode(valor_aluguel, "1 - 100" = "0",
                                                 "101 - 300"     = "1", 
                                                 "301 - 600"     = "2",
                                                 "601 - 800"     = "3",
                                                 "801 - 1.600"   = "4",
                                                 "1.601 - 3.000" = "5",
                                                 "3.001 - 10.000"= "6",
                                                 "10.001 - 50.000"= "7",
                                                 )
         ) 



# colocando as variaveis idade e salario em intervalos para facil --------



dados_junho <- dados_junho |> 
  mutate(
    idade_nova = case_when(
      idade >= 15 & idade <= 24 ~ "15-24",
      idade >= 25 & idade <= 34 ~"25-34",
      idade >= 35 & idade <= 49 ~"35-49",
      idade >= 50 & idade <= 64 ~"50-64",
      idade > 64 ~"65+",
    ),
    faixa_salarial_nova = factor(case_when(
      faixa_salarial <= 1044 ~ "Menos de um salário mínimo",
      faixa_salarial >= 1045 & faixa_salarial <= 2090 ~ "Entre 1 e 2",
      faixa_salarial >= 2091 & faixa_salarial <= 3135 ~ "Entre 2 e 3",
      faixa_salarial >= 3136 & faixa_salarial <= 4180 ~ "Entre 3 e 4",
      faixa_salarial >= 4181 & faixa_salarial <= 5225 ~ "Entre 3 e 4",
      faixa_salarial >= 5226 ~ "Mais de 5",
    ), levels = c("Menos de um salário mínimo",
                  "Entre 1 e 2",
                  "Entre 2 e 3",
                  "Entre 3 e 4",
                  "Entre 4 e 5",
                  "Mais de 5"))
  )


dados_junho_salvador <- dados_junho |> 
  filter(capital == "Município de Salvador (BA)")



# limpeza de dados acaba aqui ---------------------------------------------

library(survey)
dados_junho_salvador |>
  summarise(media_ponderada = weighted.mean(faixa_salarial, w = peso, na.rm = TRUE))


dados_grafico <- dados_junho_salvador |> 
  group_by(escolaridade, sexo) |> 
  summarise(total_ponderado_es = sum(peso),
            total_pon_sexo = sum(peso)
            )  |> 
  mutate(proporcao_es = total_ponderado_es / sum(total_ponderado_es) * 100,
         proporcao_sexo = total_pon_sexo / sum(total_pon_sexo) * 100
         )


ggplot(dados_grafico, aes(x = reorder(escolaridade, -proporcao), y = proporcao_es, fill = proporcao_sexo)) +
  geom_bar(stat = "identity", fill = "steelblue")

# fazeno uns testes de graficos -------------------------------------------


install.packages("srvyr")

library(srvyr)
library(scales) 


teste <- dados_junho |>
  filter(capital == "Município de Salvador (BA)") |> 
  count(sexo,raça,trabalho, wt = peso) |> 
  drop_na(sexo, raça, trabalho) |> 
  group_by(sexo, trabalho) |> 
  mutate(
    porce = percent(n/ sum(n, na.rm = TRUE))
  ) |> 
  ungroup()

ggplot(teste,
       aes(x = sexo,y = porce, fill = raça)) +
  geom_bar(position = "dodge", stat = "identity") +
  facet_wrap(~trabalho)


dados_junho |> 
  group_by(trabalho) |> 
  count(escolaridade, wt =  peso) |> 
  drop_na() |> 
  mutate(freq = n/ sum(n)) |> 
  ggplot(aes(x = escolaridade, y = freq, fill = trabalho)) +
  geom_bar(position = "dodge", stat = "identity",
           colour = "black")



# analise 5 grafico -------------------------------------------------------

table <- dados_junho_salvador |>
  select(peso, ficou_em_casa:outra_providencia) |>
  pivot_longer(
    cols = -peso,
    names_to = "providencia",
    values_to = "resposta"
  ) |>
  filter(resposta == "Sim") |>
  count(providencia, wt = peso) |>
  mutate(providencia = case_when(
    providencia == "ficou_em_casa" ~ "Ficou em casa",
    providencia == "automedicação" ~ "Comprou ou tomou remédio por conta própria",
    providencia == "medicação_orientação_medica" ~ "Comprou ou tomou remédio por orientação médica",
    providencia == "ligou_profissional" ~ "Ligou para profissional de saúde",
    providencia == "outra_providencia" ~ "Outra providência",
    providencia == "visita_sus" ~ "Recebeu visita de profissional de saúde do SUS",
    providencia == "visita_particular" ~ "Recebeu visita de profissional de saúde particular",
    TRUE ~ providencia 
  )) |> 
  arrange(desc(n))







dados_junho_salvador |>
      select(peso, ficou_em_casa:outra_providencia) |>
      pivot_longer(
        cols = -peso,
        names_to = "providencia",
        values_to = "resposta"
      ) |>
      filter(resposta == "Sim") |>
        count(providencia, wt = peso) |>
        mutate(providencia = case_when(
        providencia == "ficou_em_casa" ~ "Ficou em casa",
        providencia == "automedicação" ~ "Comprou ou tomou remédio por conta própria",
        providencia == "medicação_orientação_medica" ~ "Comprou ou tomou remédio por orientação médica",
        providencia == "ligou_profissional" ~ "Ligou para profissional de saúde",
        providencia == "outra_providencia" ~ "Outra providência",
        providencia == "visita_sus" ~ "Recebeu visita de profissional de saúde do SUS",
        providencia == "visita_particular" ~ "Recebeu visita de profissional de saúde particular",
        TRUE ~ providencia 
      ))