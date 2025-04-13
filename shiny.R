---
title: "Analise exploratória de Dados"
author: "Jair Toebe"
date: "2025-04-06"
output: github_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE)
```


## Introdução

```{r importando bibliotecas, echo = FALSE}




library(usethis)


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

library(usethis)
library(gitcreds)



options(readr.show_col_types = FALSE)

```

# Importação de dados

```{r importando dados dolar, echo = FALSE}


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


```{r x descarevendo a base com box plot roubo celular, echo = FALSE}

 # Selecionar todas as variáveis numéricas da base TabelaFinal
vars_numericas <- TabelaGasolina %>% 
  select(where(is.numeric))



  # Selecionar apenas as colunas numéricas
  numericas <- TabelaGasolina %>% select(where(is.numeric))
  
  
  # Função para remover outliers com base no IQR
remove_outliers_iqr <- function(df) {
  df_num <- df %>% select(where(is.numeric))  # apenas variáveis numéricas
  for (var in names(df_num)) {
    Q1 <- quantile(df_num[[var]], 0.25, na.rm = TRUE)
    Q3 <- quantile(df_num[[var]], 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    lower <- Q1 - 1.5 * IQR
    upper <- Q3 + 1.5 * IQR
    df <- df %>% filter(df[[var]] >= lower & df[[var]] <= upper)
  }
  return(df)
}

 TabelaFinal_sem_outliers <- remove_outliers_iqr(TabelaGasolina)

 
```


## Item 2

#Foi escolhido a base de dados para entender as relações entre as variaves que podem interferir com o valor dos combustiveis no Brasil.

#É esperado entender a correlação das variaveis e ver qual tem a maior interferência




#Item 3. Foi utilizado o pacote summarytools (função descr) abaixo:


```{r descrevendo a base com box plot roubo celular, echo = FALSE}

descr(TabelaGasolina)


## Item 5a

##A escolha do número de bins do histograma é uma escolha arbitrária e está diretamente ligada à amplitude que se deseja definir como base de cada barra contígua, para o caso onde se trabalha com amplitudes iguais para todos intervalos.

## Podemos usar a regra de A regra de Freedman-Diaconis ou a de Sturge



```




#Item 4 - Gráfico com a matriz de espalhamento:


```{r dispersaossx, echo = FALSE}

library(tidyverse)


  # Criar coluna MesAno a partir da data
  TabelaGasolina <- TabelaGasolina %>%
    mutate(MesAno = format(Dia, "%Y-%m"))
  
 fd <- function(x) {
    n <- length(x)
    return((2 * IQR(x)) / n^(1/3))
  }
  
  # Função customizada para calcular a largura do bin usando a regra de Scott
  sr <- function(x) {
    n <- length(x)
    return((3.49 * sd(x)) / n^(1/3))
  }


TabelaGasolina %>%
  ggplot(aes(x = cotacaoVenda, y = BarrilPorDolar_Texas, color = Produto)) +
  geom_point(alpha = 0.7) +
  facet_wrap(~MesAno) +
  stat_cor(method = "pearson", label.x = 5, label.y = 85, size = 3, color = "black") +
  xlab("Cotação do Dólar (Venda)") +
  ylab("Barril por Dólar (Texas)") +
  labs(
    title = "Correlação entre Dólar e Petróleo (Texas) por Produto e Mês",
    color = "Tipo de Produto"
  ) +
  theme_minimal()

```



```{r correlacao, echo = FALSE}


# Selecionar apenas as variáveis numéricas da base
  vars_numericas <- TabelaGasolina %>%
    select(where(is.numeric))
  
  # Remover NAs para o cálculo da correlação
  vars_numericas_limpa <- na.omit(vars_numericas)
  
  # Calcular correlação de Spearman
  matriz_cor <- cor(vars_numericas_limpa, method = "spearman")
  
  # Plotar matriz de correlação
  corrplot(matriz_cor, 
         method = "color",      # Cores nos quadrados
         type = "upper",        # Mostrar apenas parte superior
         tl.col = "black",      # Cor dos rótulos das variáveis
         tl.cex = 0.8,          # Tamanho dos rótulos
         addCoef.col = "black", # Mostra os números de correlação
         number.cex = 0.7,      # Tamanho dos números
         title = "Correlação Spearman - Variáveis Numéricas",
         mar = c(0, 0, 2, 0))    # Margem do gráfico

  
  
  

```



```{r espalhamento 2, echo = FALSE}




# Selecionar apenas variáveis numéricas
vars_numericas <- TabelaFinal_sem_outliers %>% 
  select(where(is.numeric)) %>% 
  na.omit()  # Remove linhas com NA

# Usar GGally::ggpairs para matriz de dispersão com correlação
GGally::ggpairs(vars_numericas,
                lower = list(continuous = wrap("points", alpha = 0.5, size = 0.7)),
                diag = list(continuous = wrap("densityDiag", alpha = 0.5)),
                upper = list(continuous = wrap("cor", size = 4, method = "spearman"))
)+
  theme_minimal(base_size = 12) +
  theme(
    strip.background = element_rect(fill = "#f0f0f0", color = NA),
    panel.grid.major = element_line(color = "#eeeeee"),
    panel.background = element_rect(fill = "#ffffff")
  )

```

  



# Item 5

## A distribuição normal é a base teórica para muitas ferramentas estatísticas e é considerada uma “referência” para o comportamento de dados contínuos. No entanto, nem todos os conjuntos de dados seguem essa distribuição, e por isso é essencial verificá-la antes de aplicar testes paramétricos.

### Podemos analisar a distribuição através de:

####Visualmente: através de histogramas ou Q-Q Plots

####Estatisticamente: com o teste de Shapiro-Wilk






#Item 5a



```{r histograma, echo = FALSE}

fd <- function(x) {
  n <-length(x)
  return((2*IQR(x))/n^(1/3))
}

 sr <- function(x) {
  n <-length(x)
  return((3.49*sd(x))/n^(1/3))
}

# Calcular binwidth com a função Freedman-Diaconis
bw <- fd(TabelaGasolina$BarrilPorDolar_Texas)


# Criar coluna MesAno a partir da data
TabelaGasolina <- TabelaGasolina %>% mutate(MesAno = format(Dia, "%Y-%m"))

  
# Calcular binwidth com a função Freedman-Diaconis
bw <- fd(TabelaGasolina$BarrilPorDolar_Texas)

#Criando histogramas

TabelaGasolina %>%
  ggplot(aes(x = BarrilPorDolar_Texas)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 0.2, fill = 'lightblue', color = "black") +
  geom_density(kernel = 'epanechnikov', color = "red", size = 1) +
  facet_wrap(~MesAno, scales = "free_y") +
  labs(
    title = "Distribuição do Preço do Barril (Texas) por Mês",
    x = "BarrilPorDolar_Texas",
    y = "Densidade"
  ) +
  theme_minimal()

```


# Item 5b:

```{r Item 5b, echo = FALSE}

vars_numericas <- TabelaGasolina %>%
  select(where(is.numeric))

# Loop para criar Q-Q plots
for (var in names(vars_numericas)) {
  print(
    ggqqplot(vars_numericas[[var]],
             title = paste("Q-Q Plot -", var),
             color = "#0073C2",
             ggtheme = theme_minimal())
  )
}



```


# Item 5c:

```{r Item 5c, echo = FALSE}



# Selecionar apenas valores não NA e limitar a 5000
amostra_dados <- TabelaGasolina %>%
  filter(!is.na(cotacaoVenda)) %>%
  slice_sample(n = 5000)

# Aplicar o teste de Shapiro-Wilk
shapiro.test(amostra_dados$cotacaoVenda)




# Aplicar o teste de Shapiro-Wilk
shapiro.test(amostra_dados$BarrilPorDolar_Texas)



# Aplicar o teste de Shapiro-Wilk
shapiro.test(amostra_dados$BarrilPorDolar_Europa)

# Aplicar o teste de Shapiro-Wilk
shapiro.test(amostra_dados$Valor.de.Venda)




```


#Item 5d - Os resultados para as principais variáveis numéricas foram os seguintes:

###cotacaoVenda: W = 0.89803, p < 2.2e-16

###BarrilPorDolar_Texas: W = 0.97327, p < 2.2e-16

###BarrilPorDolar_Europa: W = 0.98595, p < 2.2e-16

###Valor.de.Venda: W = 0.91634, p < 2.2e-16

###Em todos os casos, os p-valores são menores que o nível de significância convencional de 0,05. Dessa forma, rejeita-se a hipótese nula para todas as variáveis, concluindo que nenhuma delas apresenta uma distribuição normal.

###Portanto, recomenda-se a utilização de testes estatísticos não paramétricos (como o teste de Wilcoxon ou Spearman), visto que as suposições de normalidade não são atendidas.


# Item 6:

##Completude é um dos principais aspectos da qualidade de dados e se refere à presença total ou parcial das informações esperadas em um conjunto de dados. Um dado é considerado completo quando todos os campos necessários estão preenchidos, sem valores ausentes (missing values), nulos ou indefinidos.

##Na prática, a completude diz respeito a "o quanto os dados estão presentes". Por exemplo, em uma base de preços de combustíveis, se a coluna “Valor de Venda” estiver faltando em várias linhas, dizemos que aquela variável apresenta baixa completude.

## A ausência de completude pode comprometer toda a análise exploratória de dados, gerando:

##Distorções estatísticas: Médias, medianas, desvio padrão e outras métricas podem ficar enviesadas.

##Visualizações enganosas: Gráficos com valores ausentes podem sugerir padrões incorretos.

##Resultados inconsistentes em modelos preditivos: Algoritmos de machine learning são sensíveis a valores faltantes.

##Redução do poder analítico: Em alguns casos, é necessário remover variáveis ou observações com muita ausência, perdendo informações valiosas.

# Item 7:

```{r Item 7, echo = FALSE}

# Calcular completude por variável
completude <- TabelaGasolina %>%
  summarise(across(everything(), ~ mean(!is.na(.)) * 100)) %>%
  pivot_longer(cols = everything(), names_to = "Variável", values_to = "Completude (%)") %>%
  arrange(desc(`Completude (%)`))

# Visualizar resultado
print(completude)


```

#Item 8:

```{r Item 8, echo = FALSE}


library(mice)
dados_numericos <- TabelaGasolina %>%
  dplyr::select(where(is.numeric))



imp <- mice(dados_numericos, m = 5, maxit = 5, method = "pmm", seed = 512)


stripplot(imp, pch = c(21, 20), cex = c(1, 1.5))



```

#Item 9 está em um arquivo separado que gera um relaorio em Shine.

#Os arquivos estão disponiveis no GitHub. Tive grande dificuldade na sincronização do codigo pelo Rstudio no GitHub, mas o codigo está disponivél conforme solicitado na questão 9.

# https://github.com/jairtoebe/AnaliseExploratoria

