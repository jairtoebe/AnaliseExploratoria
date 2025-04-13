
library(naniar)
library(simputation)
library(httr)
library(jsonlite)
library(tidyverse)
library(readr)
library(lubridate)
library(ggplot2)
library(summarytools)
library(dlookr)
library(knitr)
library(reshape2)
library(png)
library(ggpubr) 
library(naniar)
library(mice)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(corrplot)
library(GGally)
library(dplyr)
library(mice)






############ Importando dolares  ##############

# URL da API  cotocacao dolar
url <- "https://olinda.bcb.gov.br/olinda/servico/PTAX/versao/v1/odata/CotacaoDolarPeriodo(dataInicial=@dataInicial,dataFinalCotacao=@dataFinalCotacao)?@dataInicial='01-01-1996'&@dataFinalCotacao='01-01-2025'&$top=50000&$format=json"

# Fazer requisição GET para obter os dados
response <- GET(url)

# Converter a resposta para texto e depois para JSON
dados_json <- content(response, "text", encoding = "UTF-8") %>% fromJSON()

# Transformar os dados em um tibble
DolarConv <- as_tibble(dados_json$value)

# Organizando Variáveis
dadosDolar <- DolarConv %>% 
  select(dataHoraCotacao, cotacaoVenda) %>% 
  mutate(
    Dia = as.Date(dataHoraCotacao),
    cotacaoVenda = round(cotacaoVenda, 2),
  ) %>% 
  select(Dia, cotacaoVenda)


# Criar sequência completa de datas do período
data_inicio <- min(dadosDolar$Dia)
data_fim <- max(dadosDolar$Dia)
todas_datas <- tibble(Dia = seq(data_inicio, data_fim, by = "day"))

# Completar com datas faltantes (left join com a sequência completa)
dadosDolarCompleto <- todas_datas %>% 
  left_join(dadosDolar, by = "Dia") %>% 
  mutate(Dia = as.Date(Dia, format = "%d/%m/%Y"))%>%
  filter(Dia >= as.Date("2024-01-01") & Dia <= as.Date("2024-06-30"))


#dadosDolarCompleto <- dadosDolarCompleto %>% 
# mutate(Dia = as.Date(Dia, format = "%d/%m/%Y"))



#Fim de importacao de dados Dolar -------------------------- Data Frame "dadosCotacaoDolar2024"




#Importando Dados Petroleo Texas


# Definir a URL Texas
url_petroTexas <- "https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23ebf3fb&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1320&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=DCOILWTICO&scale=left&cosd=2020-02-18&coed=2025-02-18&line_color=%230073e6&link_values=false&line_style=solid&mark_type=none&mw=3&lw=3&ost=-99999&oet=99999&mma=0&fml=a&fq=Daily&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2025-02-25&revision_date=2025-02-25&nd=1986-01-02"

# Importar os dados
oil_dataTexas <- read_csv(url_petroTexas)

# Importar os dados
oil_dataTexas <- oil_dataTexas %>%
  select(observation_date, everything()) %>% 
  rename(Dia = observation_date, BarrilPorDolar_Texas = DCOILWTICO) %>% fill(BarrilPorDolar_Texas, .direction = "downup") %>% filter(Dia >= as.Date("2024-01-01") & Dia <= as.Date("2024-12-31"))


oil_dataTexas <- oil_dataTexas %>%
  mutate(Dia = as.Date(Dia, format = "%Y/%m/%d")) %>%  # Converte para data correta
  mutate(Dia = format(Dia, "%d/%m/%Y"))  # Formata como string no formato desejado %>%



#Importando Dados Petroleo Europa


# Definir a URL Europa
url_petroEuropa <- "https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23ebf3fb&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=off&txtcolor=%23444444&ts=12&tts=12&width=1320&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=DCOILBRENTEU&scale=left&cosd=2020-02-18&coed=2025-02-18&line_color=%230073e6&link_values=false&line_style=solid&mark_type=none&mw=3&lw=3&ost=-99999&oet=99999&mma=0&fml=a&fq=Daily&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2025-02-25&revision_date=2025-02-25&nd=1987-05-20"

# Importar os dados
oil_dataEuropa <- read_csv(url_petroEuropa)

# Organizar as variáveis
oil_dataEuropa <- oil_dataEuropa %>%
  select(observation_date, everything()) %>% 
  rename(Dia = observation_date, BarrilPorDolar_Europa = DCOILBRENTEU) %>% filter(Dia >= as.Date("2024-01-01") & Dia <= as.Date("2024-12-31"))   %>% fill(BarrilPorDolar_Europa, .direction = "downup")

oil_dataEuropa <- oil_dataEuropa %>%
  mutate(Dia = as.Date(Dia, format = "%Y/%m/%d")) %>%  # Converte para data correta
  mutate(Dia = format(Dia, "%d/%m/%Y"))  # Formata como string no formato desejado %>%



#Importando Dados Combustivel brasil


precosPrimeiro2024br <- read.csv2("C:/Users/jairt/OneDrive/Documents/pos/AnaliseExploratoriaDeDados/trabalhoFinal/dadosAuxiliares/precosPrimeiro2024br.csv", fileEncoding = "UTF-8")



precosPrimeiro2024br <- precosPrimeiro2024br %>%
  mutate(Data.da.Coleta = as.Date(Data.da.Coleta, format = "%d/%m/%Y")) 


precosPrimeiro2024br <- precosPrimeiro2024br %>%
  rename(Dia = Data.da.Coleta) 



dadosDolarCompleto <- dadosDolarCompleto %>% 
  mutate(Dia = as.Date(Dia, format = "%d/%m/%Y"))

oil_dataEuropa <- oil_dataEuropa %>% 
  mutate(Dia = as.Date(Dia, format = "%d/%m/%Y"))


oil_dataTexas <- oil_dataTexas %>% 
  mutate(Dia = as.Date(Dia, format = "%d/%m/%Y"))


#### Juntar tabelas

TabelaFinal <- dadosDolarCompleto %>%
  inner_join(oil_dataTexas, by = 'Dia') %>%
  inner_join(oil_dataEuropa, by = 'Dia') %>%
  inner_join(precosPrimeiro2024br, by = 'Dia')



##############termino de juntar planilhas





# Aplicar imputação kNN para valores ausentes em TabelaFinal
TabelaFinal <- TabelaFinal %>%
  as.data.frame() %>%
  simputation::impute_knn(. ~ ., seed = 512) 


numericas <- TabelaFinal %>%
  dplyr::select(where(is.numeric)) %>%
  as.data.frame()


# Aplicar imputação kNN apenas nas colunas numéricas
numericas_imputadas <- simputation::impute_knn(numericas, . ~ ., seed = 512)

# Substituir as colunas numéricas originais pelas imputadas
TabelaFinal[names(numericas_imputadas)] <- numericas_imputadas


TabelaGasolina <- TabelaFinal %>%
  dplyr::filter(tolower(Produto) == "gasolina")


# Pacotes necessários
library(shiny)
library(ggplot2)
library(dplyr)
library(colourpicker)


# UI
ui <- fluidPage(
  titlePanel("📊 Dashboard - Visualização de Séries Temporais (TabelaGasolina)"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("variavel", "Escolha a variável para o gráfico:",
                  choices = names(TabelaGasolina %>% select(where(is.numeric))),
                  selected = "cotacaoVenda"),
      
      colourInput("cor_linha", "Escolha a cor da linha:", value = "blue"),
      
      dateRangeInput("xlim", "Selecione o intervalo de datas (Eixo X):",
                     start = min(TabelaGasolina$Dia, na.rm = TRUE),
                     end = max(TabelaGasolina$Dia, na.rm = TRUE)),
      
      sliderInput("ylim", "Limites do eixo Y:",
                  min = 0, max = 100, value = c(0, 50))
    ),
    
    mainPanel(
      plotOutput("grafico_linha", height = "500px")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Atualiza limites do Y automaticamente conforme variável escolhida
  observeEvent(input$variavel, {
    var_data <- TabelaGasolina[[input$variavel]]
    updateSliderInput(session, "ylim",
                      min = floor(min(var_data, na.rm = TRUE)),
                      max = ceiling(max(var_data, na.rm = TRUE)),
                      value = c(floor(min(var_data, na.rm = TRUE)),
                                ceiling(max(var_data, na.rm = TRUE))))
  })
  
  output$grafico_linha <- renderPlot({
    ggplot(TabelaGasolina, aes(x = Dia)) +
      geom_line(aes_string(y = input$variavel), color = input$cor_linha, size = 1) +
      coord_cartesian(
        xlim = input$xlim,
        ylim = input$ylim
      ) +
      labs(title = paste("Gráfico de Linha da Variável:", input$variavel),
           x = "Data", y = input$variavel) +
      theme_minimal(base_size = 14)
  })
}

# Rodar o app
shinyApp(ui, server)
