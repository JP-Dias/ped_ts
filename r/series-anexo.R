# Pacotes ----
required_packages <- c("readxl", "tidyverse", "zoo","imputeTS","forecast")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# Diretório ----
caminho <- "dados/Anexo Estatistico PED-DF OUTBRO 2024.xlsx"

# Dados ----
# Lê dados brutos
temp <- read_excel(caminho, sheet = "Tab1", skip = 8)

# Coluna de Data
datas <- temp[-c(1:3,(nrow(temp)-10):nrow(temp)),1] |> na.omit() |> setNames("mes_ano")

temp <- NULL

# Separar mes e ano
mes_ano_split <- strsplit(datas$mes_ano, "-")
meses <- sapply(mes_ano_split, `[`, 1)
anos <- sapply(mes_ano_split, `[`, 2)

# Preencher anos ausentes
ano_inicial <- as.numeric(anos[!is.na(anos)][1])

for (i in 1:length(anos)) {
  if (is.na(anos[i])) {
    if (i == 1) {
      anos[i] <- ano_inicial
    } else {
      if (meses[i] == "01" && meses[i-1] == "12") {
        anos[i] <- as.numeric(anos[i-1]) + 1
      } else {
        anos[i] <- anos[i-1]
      }
    }
  }
}

meses_numeros <- recode(meses, 
                        "Janeiro" = "01", 
                        "Fevereiro" = "02", 
                        "Março" = "03", 
                        "Abril" = "04", 
                        "Maio" = "05", 
                        "Junho" = "06", 
                        "Julho" = "07", 
                        "Agosto" = "08", 
                        "Setembro" = "09", 
                        "Outubro" = "10", 
                        "Novembro" = "11", 
                        "Dezembro" = "12")




# Apaga colunas desnecessárias
dados <- read_excel(caminho, sheet = "Tab1", skip = 8)
tab1 <- dados[-c(1:3,(nrow(dados)-10):nrow(dados)),-c(3,5,7,9,11)] |> na.omit()

# Renomeia dados
names(tab1) <- c("mes_ano","pia","pea","ocup","desemp","inat","tx_part","tx_desemp","pop")

dados <- NULL

# Adicionar meses e anos
tab1 <- tab1 |> 
  select(-mes_ano) |> 
  mutate(data = as.Date(paste(anos, meses_numeros, "01", sep = "-"))) |> 
  mutate(across(-data, ~ na_if(.x, "-"))) |> 
  mutate(across(-data, as.numeric)) |> 
  select(data,everything())


dados <- read_excel(caminho, sheet = "Tab8", skip = 8)

# Apaga colunas desnecessárias
tab8 <- dados[-c((nrow(dados)-14):nrow(dados)),-c(3,5,7,9,11,13)] |> na.omit()

# Renomeia dados
names(tab8) <- c("mes_ano","total","industria","construcao","comercio","servicos","adm_publica")

tab8 <- tab8 |>
  select(-mes_ano) |> 
  mutate(data = as.Date(paste(anos, meses_numeros, "01", sep = "-"))) |> 
  mutate(across(-data, ~ na_if(.x, "-"))) |> 
  mutate(across(-data, as.numeric)) |> 
  select(data,everything()) |> 
  filter(data > "2010-12-01") 


dados <- read_excel(paste0("dados/","Anexo Estatistico PED-DF JULHO 2024.xlsx"), sheet = "Tab10", skip = 8)

# Apaga colunas desnecessárias
tab10 <- dados[-c((nrow(dados)-16):nrow(dados)),-c(2,6,10)] |> na.omit()

dados <- NULL

names(tab10) <- c("mes_ano",
                  "industria_transf",
                  "construcao",
                  "comercio_reparacao",
                  "transporte",
                  "informacao_comunicacao",
                  "ativ_adm_compl",
                  "adm_publica",
                  "educacao",
                  "saude_serv_sociais",
                  "alojamento_alim_outras",
                  "servicos_domesticos")

tab10 <- tab10 |>
  select(-mes_ano) |> 
  mutate(data = as.Date(paste(anos, meses_numeros, "01", sep = "-"))) |> 
  mutate(across(-data, ~ na_if(.x, "-"))) |> 
  mutate(across(-data, as.numeric)) |> 
  select(data,everything()) |> 
  filter(data > "2010-12-01") 



lista_sazonal <- list()

# Colunas de interesse (exceto a coluna de datas)
colunas <- names(tab8)[-1]

# Loop para decompor cada série temporal e armazenar o resultado
for (coluna in colunas) {
  base <- ts(tab8[[coluna]], start = c(2011, 1), frequency = 12) |> na_seadec() |> mstl()
  sazonal <- base[, "Seasonal12"]
  dados <- data.frame(data = as.Date(time(base)), sazonal = sazonal, setor = coluna)
  lista_sazonal[[coluna]] <- dados
}

# Combine todos os data frames em um único
dados_sazonal <- bind_rows(lista_sazonal)

# Criar o gráfico
ggplot(dados_sazonal, aes(x = data, y = sazonal, color = setor)) +
  geom_line(lwd = 1) +
  scale_x_date(limits = as.Date(c("2023-01-01", "2023-12-01")),date_breaks = "3 months", date_labels = "%b %Y",expand = c(.01,.01))+
  labs(title = "Componentes Sazonais por Setor",
       x = "Data",
       y = "Componente Sazonal") +
  scale_color_viridis_d(option = "D") +
  theme_bw() +
  theme(strip.background = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=.5)) 

ggplot(dados_sazonal, aes(x = data, y = sazonal, color = setor)) +
  geom_line(lwd = 1) +
  scale_x_date(limits = as.Date(c("2023-01-01", "2023-12-01")),date_breaks = "3 months", date_labels = "%b %Y",expand = c(.01,.01))+
  labs(title = "Componentes Sazonais por Setor",
       x = "Data",
       y = "Componente Sazonal") +
  theme_bw() +
  theme(strip.background = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=.5)) +
  scale_color_viridis_d(option = "D")+
  facet_wrap(~setor)

ggplot(dados_sazonal, aes(x = data, y = sazonal, color = setor)) +
  geom_line() +
  labs(title = "Componentes Sazonais por Setor",
       x = "Data",
       y = "Componente Sazonal") +
  theme_bw() +
  theme(strip.background = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=.5)) +
  scale_color_viridis_d(option = "D")+
  facet_wrap(~setor)

# Desemprego
ts(tab1$tx_desemp, start = c(2001, 3), frequency = 12) |> na_seadec() |> stl(t.window=13,s.window = "periodic",robust = T) |> autoplot()
ts(tab1$tx_desemp, start = c(2001, 3), frequency = 12) |> na_seadec() |> stl(s.window=13) |> autoplot()

# Participaçao
ts(tab1$tx_part, start = c(2001, 3), frequency = 12) |> na_seadec() |> stl(t.window=13,s.window = "periodic",robust = T) |> autoplot()
ts(tab1$tx_part, start = c(2001, 3), frequency = 12) |> na_seadec() |> stl(s.window=13) |> autoplot()


# Industria
ts(tab8$industria, start = c(2011, 1), frequency = 12) |> na_seadec() |> stl(t.window=13,s.window = "periodic",robust = T) |> autoplot()
ts(tab8$industria, start = c(2011, 1), frequency = 12) |> na_seadec() |> stl(s.window=13) |> autoplot()

# Construção
ts(tab8$construcao, start = c(2011, 1), frequency = 12) |> na_seadec() |> stl(t.window=13,s.window = "periodic",robust = T) |> autoplot()
ts(tab8$construcao, start = c(2011, 1), frequency = 12) |> na_seadec() |> stl(s.window=13) |> autoplot()

# Comércio
ts(tab8$comercio, start = c(2011, 1), frequency = 12) |> na_seadec() |> stl(t.window=13,s.window = "periodic",robust = T) |> autoplot()
ts(tab8$comercio, start = c(2011, 1), frequency = 12) |> na_seadec() |> stl(s.window=13) |> autoplot()

# Serviços
ts(tab8$servicos, start = c(2011, 1), frequency = 12) |> na_seadec() |> stl(t.window=13,s.window = "periodic",robust = T) |> autoplot()
ts(tab8$servicos, start = c(2011, 1), frequency = 12) |> na_seadec() |> stl(s.window=13) |> autoplot()

# Adm Púb
ts(tab8$adm_publica, start = c(2011, 1), frequency = 12) |> na_seadec() |> stl(t.window=13,s.window = "periodic",robust = T) |> autoplot()
ts(tab8$adm_publica, start = c(2011, 1), frequency = 12) |> na_seadec() |> stl(s.window=13) |> autoplot()


ts(tab1$tx_desemp, start = c(2001, 3), frequency = 12) |> na_seadec() |> ggsubseriesplot()
ts(tab1$tx_part, start = c(2001, 3), frequency = 12) |> na_seadec() |> ggsubseriesplot()


ts(tab8$industria, start = c(2011, 1), frequency = 12) |> na_seadec() |> ggsubseriesplot()
ts(tab8$construcao, start = c(2011, 1), frequency = 12) |> na_seadec() |> ggsubseriesplot()
ts(tab8$comercio, start = c(2011, 1), frequency = 12) |> na_seadec() |> ggsubseriesplot()
ts(tab8$servicos, start = c(2011, 1), frequency = 12) |> na_seadec() |> ggsubseriesplot()
ts(tab8$adm_publica, start = c(2011, 1), frequency = 12) |> na_seadec() |> ggsubseriesplot()




ts(tab10$educacao, start = c(2011, 1), frequency = 12) |> na_seadec() |> stl(t.window=13,s.window = "periodic",robust = T) |> autoplot()
ts(tab10$educacao, start = c(2011, 1), frequency = 12) |> na_seadec() |>  stl(s.window=13) |> autoplot()

ts(tab10$transporte, start = c(2011, 1), frequency = 12) |> na_seadec() |> stl(t.window=13,s.window = "periodic",robust = T) |> autoplot()
ts(tab10$transporte, start = c(2011, 1), frequency = 12) |> na_seadec() |>  mstl() |> autoplot()

ts(tab10$informacao_comunicacao, start = c(2011, 1), frequency = 12) |> na_seadec() |> stl(t.window=13,s.window = "periodic",robust = T) |> autoplot()
ts(tab10$informacao_comunicacao, start = c(2011, 1), frequency = 12) |> na_seadec() |>  mstl() |> autoplot()


