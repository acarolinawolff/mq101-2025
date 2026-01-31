# ============================================================
# MQ101 — Métodos Quantitativos para Políticas Públicas
# Lista de Exercícios #02 
# ------------------------------------------------------------

# Nome:Ana Carolina Wolff
# RA: 23202510326
# Turma: PPU-009 - MÉTODOS QUANTITATIVOS - TPPU00920253 (2025.3 - 4T234)
# Data: 09/10/2025
# Descrição: Tipos de Variáveis e Estatísticas Descritivas


---------------------------------------------------
#EXERCÍCIO 1 - Preparação de ambiente
  
install.packages(c("tidyverse", "readr"))
library(tidyverse)
options(scipen = 999)

install.packages("ggplot2")
library(ggplot2)



#EXERCÍCIO 2 - Base de dados (contexto e dicionário)
#Verifique: número de linhas/colunas; classes das variáveis; existência de NA.


dados <- readr::read_csv("data/educ_saude.csv") 
#atribuir

dados <- read.csv (file.choose()) 
#outra forma de chamar os dados

install.packages("dplyr")
# ou
install.packages("tibble")

library(dplyr)
# ou
library(tibble)


glimpse(dados) 
summary(dados)

nrow(dados)
ncol(dados)
str (dados)

sum(is.na(dados)) 
colSums(is.na(dados))

#RESPOSTA:
# Linhas = 10.000 e Colunas = 13
#classe das variáveis: quantitaticas e qualitativas
# não há NA 


#EXERCÍCIO 3 - Classificação de variáveis
#Faça uma tabela (no seu script) indicando o tipo teórico de cada variável (nominal,ordinal, discreta, contínua).


dados <- dados |>
mutate(
sexo = factor(sexo),
rede_escolar = factor(rede_escolar),
plano_saude = factor(plano_saude),
diagnostico = factor(diagnostico),
escolaridade = factor(escolaridade,
levels = c("Fundamental","Médio","Superior"),
ordered = TRUE)
    )

str(dados)
glimpse(dados)



dfl <- data.frame (variavel = c ("sexo","escolaridade","anos_estudo",
                                "rede_escolar","municipio","UF","idade",
                                "faltas_esc","tempo_estudo_h",
                                "pressao_sistolica","diagnostico",
                                "plano_saude"),

tipoteorico = c ("qualitativa nominal","qualitativa ordinal",
                 "quantitativa discreta", "qualitativa nominal",
                 "qualitativa nominal", "qualitativa nominal",
                 "quantitativa contínua", "quantitativa discreta", 
                 "quantitativa contínua", "quantitativa contínua", 
                 "qualitativa nominal", "qualitativa nominal"),


stringAsFactors=FALSE)

dfl

#Registre, em comentários, por que cada variável é daquele tipo.


#sexo (F/M) — qualitativa nominal -> categorias sem ordem natural
#escolaridade (Fundamental/Médio/Superior) — qualitativa ordinal -> categorias com hierarquia que indica progressão/nível de instrução
#anos de estudo - quantitativa discreta - contagem inteira
#rede_escolar (pública/privada) — qualitativa nominal -> não há hierarquia entre pública e privada
#município - qualitativa nominal - categorias sem ordem natural entre elas
#UF - qualitativa nominal - categorias sem ordem natural entre elas
#idade (anos) — quantitativa contínua -> pode assumir qualquer valor real, mesmo que na prática seja registrada em inteiros
#faltas_esc (nº faltas no último mês) — quantitativa discreta -> contagem de eventos, não admite frações
#tempo_estudo_h (horas por semana) — quantitativa contínua -> pode ser fracionado
#pressao_sistolica (mmHg) — quantitativa contínua -> medida em escala contínua
#diagnostico (“sem”, “HAS”, “DM”, “outros”) — qualitativa nominal -> categorias sem ordem natural
#plano_saude (SUS/privado/ambos/nenhum) — qualitativa nominal -> não há hierarquia natural entre as opções


#EXERCÍCIO 4 - variáveis qualitativas - frequências e gráficos

tab_plano <- table(dados$plano_saude)
tab_plano

prop_plano <- prop.table(tab_plano)
prop_plano 

cbind(FA = tab_plano, FR = round(100 * prop_plano, 1))

#Tarefa 4A - Frequências absolutas e relativas
#Interprete: qual categoria é a moda? Categoria SUS (0,5024). 
#Qual a proporção dominante? 50%.

#Tarefa 4B - Gráfico de barras (qualitativa nominal)

dados |>
   count(plano_saude) |>
  ggplot(aes(x = plano_saude, y = n)) +
   geom_col() +
   labs(x = "Plano de sade", y = "Frequência",
          title = "Distribuição de plano de saúde")


#Tarefa 4C - Barras ordenadas (qualitativa ordinal)


dados |>
   count(escolaridade) |>
   ggplot(aes(x = escolaridade, y = n)) +
   geom_col() +
   labs(x = "Escolaridade (ordem substantiva)", y = "Frequência")

#Explique por que a ordem importa para interpretar a distribuição? 
#No gráfico de barras de uma variável qualitativa ordinal, como escolaridade, a ordem das categorias é fundamental porque reflete uma hierarquia natural (Fundamental < Médio < Superior). 
#Se as barras forem apresentadas fora dessa sequência, perde-se a noção de trajetória educacional e a interpretação pode ficar confusa. 
#Manter a ordem correta permite identificar tendências, como a concentração em níveis mais baixos ou o avanço para níveis mais altos, além de facilitar comparações com outras variáveis relacionadas.



#EXERCÍCIO 5 - Variáveis quantitativas — medidas e distribuição
#Escolha duas variáveis: idade e pressao_sistolica.
#Tarefa 4A. Tendência central e dispersão

sumario_idade <- dados |>
   summarise(
     n = sum(!is.na(idade)),
     media = mean(idade, na.rm = TRUE),
     mediana= median(idade, na.rm = TRUE),
     min = min(idade, na.rm = TRUE),
     max = max(idade, na.rm = TRUE),
     dp = sd(idade, na.rm = TRUE)
   )
 sumario_idade
 
 #    n media mediana   min   max    dp
 #<int> <dbl>   <dbl> <dbl> <dbl> <dbl>
 #   10000  40.4      40    18    80  12.5
 #Média = 40.4
 #mediana = 40
 
 #Repita para pressao_sistolica. 
 
 sumario_pressao_sistolica <- dados |>
   summarise(
     n = sum(!is.na(pressao_sistolica)),
     media = mean(pressao_sistolica, na.rm = TRUE),
     mediana= median(pressao_sistolica, na.rm = TRUE),
     min = min(pressao_sistolica, na.rm = TRUE),
     max = max(pressao_sistolica, na.rm = TRUE),
     dp = sd(pressao_sistolica, na.rm = TRUE)
   )
 sumario_pressao_sistolica
 
 #Resultado:
 #n media mediana   min   max    dp
 #<int> <dbl>   <dbl> <dbl> <dbl> <dbl>
 #   10000  122.     122    85   175  14.1
 #Média = 122
 #Mediana = 122
 
 
 #RESPOSTA: A média é 122 e a mediana é 122. 
 
 #Compare média vs. mediana. Há assimetria?
 
 #tanto no caso da idade como da pressão sistólica, a media e a mediana são praticamente iguais.
 #isso é um forte indicador de que a distribuição das variáveis nos dados é simétrica.
 #os valores estão bem distribuídos em torno do centro, sem uma "cauda" significativa de valores extremos (muito altos ou muito baixos) puxando a média para longe da mediana.
 
#Tarefa 4B. Histograma e boxplot
 
 ggplot(dados, aes(x = idade)) +
    geom_histogram(bins = 20) +
    labs(title = "Histograma de Idade")
 
  ggplot(dados, aes(y = idade)) +
    geom_boxplot() +
    labs(title = "Boxplot de Idade")
  
#Interprete forma, caudas e outliers. 
#RESPOSTA: No histograma de idade, observa-se um pico em torno dos 40 anos, indicando que a maior parte da amostra está concentrada nessa faixa etária. A frequência diminui gradualmente para os lados, com menos indivíduos nas idades mais jovens (abaixo de 20) e mais velhas (acima de 70), mostrando uma distribuição centralizada com caudas mais longas nas extremidades e alguns valores muito altos (outliers).
#no boxplot a mediana aparece próxima dos 40 anos, bem centralizada na caixa, sugerindo simetria, enquanto os pontos acima do limite superior representam idades significativamente maiores que o restante da amostra (outliers).
#o histograma detalha a forma da distribuição e a densidade em cada faixa, enquanto o boxplot resume em medidas de posição, dispersão e outliers.
  
  
  
#Repita para pressao_sistolica.

  ggplot(dados, aes(x = pressao_sistolica)) +
    geom_histogram(bins = 20) +
    labs(title = "Pressão Sistólica")
  
  ggplot(dados, aes(y = pressao_sistolica)) +
    geom_boxplot() +
    labs(title = "Boxplot de Pressão Sistólica")

#Interprete forma, caudas e outliers.   
#histograma: a distribuição é centrada em torno de 120–130 mmHg, com caudas mais finas nas extremidades e poucos valores extremos, sugerindo que a pressão sistólica da amostra segue um padrão bastante regular.
#boxplot: evidencia que a distribuição é relativamente simétrica, com a maior parte dos indivíduos em torno da mediana, mas também revela a presença de alguns casos de pressão elevada (outliers)
#o histograma detalha a forma e frequência dos dados, enquanto o boxplot resume em posição, dispersão e outliers.
  
  
#EXERCÍCIO 6 - Tabelas cruzadas e resumos por grupo
#Tarefa 6A. Cruzando duas qualitativas
  
  tab_cross <- table(dados$diagnostico, dados$plano_saude)
   tab_cross
   round(100 * prop.table(tab_cross, margin = 2), 1) 
  
#Qual diagnóstico é mais prevalente dentro de cada tipo de plano?

#para o caso de “ambos os planos”: a maioria está na categoria “sem diagnóstico” (81,4%); 
#para “nenhum plano”, também predomina “sem diagnóstico” (80,7%); 
#no “plano privado”, novamente, o mais prevalente é “sem diagnóstico” (80,6%); 
#no “SUS” segue o mesmo padrão, com “sem diagnóstico” (80,7%).
#etre os diagnósticos presentes, o que aparece com maior frequência relativa em todos os tipos de plano é HAS (hipertensão arterial sistêmica), em torno de 12% em cada grupo.
   
 #Tarefa 6B. Resumo de quantitativa por grupo
   
   dados |>
      group_by(sexo) |>
      summarise(
        n = n(),
        media_idade = mean(idade, na.rm = TRUE),
        dp_idade = sd(idade, na.rm = TRUE),
        mediana_idade = median(idade, na.rm = TRUE)
        )
   
   dados |>
     group_by(escolaridade) |>
     summarise(
       n = n(),
       media_idade = mean(idade, na.rm = TRUE),
       dp_idade = sd(idade, na.rm = TRUE),
       mediana_idade = median(idade, na.rm = TRUE))
   
#Compare idade por sexo e por escolaridade. Comente diferenças.
   
#por sexo: mulheres (40,3 anos em média) e homens (40,5 anos) apresentam praticamente a mesma idade média, mediana (40 anos) e desvio padrão (~12,5). Isso indica que a distribuição etária é bastante equilibrada entre os sexos.
#por escolaridade: os três níveis (Fundamental, Médio e Superior) também mostram médias muito próximas (entre 40,2 e 40,5 anos), mesma mediana (40 anos) e desvios padrão semelhantes (~12,5).
#conclusão: tanto por sexo quanto por escolaridade, a idade dos indivíduos é bastante homogênea, sem diferenças significativas. Isso sugere que a variável idade não está associada de forma marcante a essas categorias no conjunto analisado.
   
   
#EXERCÍCIO 7 - Valores ausentes e outliers
  #Tarefa 7A. Ausentes
   
colSums(is.na(dados))

#Mostre resultados com e sem NA. Explique na.rm=TRUE.

#na.rm=TRUE:significa “remover os NA” antes de calcular; se não for usado, o resultado será NA sempre que houver valores faltantes; usar na.rm = TRUE garante que os cálculos sejam feitos apenas com os dados válidos, ignorando os valores faltantes.

#Tarefa 6B. Outliers (regra do IQR)

Q <- quantile(dados$tempo_estudo_h, probs = c(.25, .75), na.rm = TRUE)
 IQRv <- IQR(dados$tempo_estudo_h, na.rm = TRUE)
 lim_inf <- Q[1] - 1.5 * IQRv
 lim_sup <- Q[2] + 1.5 * IQRv
 
 subset_out <- dados |>
   filter(tempo_estudo_h < lim_inf | tempo_estudo_h > lim_sup)
 
 nrow(subset_out)
 #348
 head(subset_out) 
 #as primeiras seis linhas

#Discuta: outliers são erros, casos raros ou informação válida?
#em alguns casos, valores extremos surgem por falhas e, por isso, devem ser removidos; 
#em outros casos, podem refletir situações pouco comuns, mas possíveis (ex. pessoa com idade muito avançada ou pressão arterial excepcionalmente alta).
#ainda, outliers podem ser os dados mais interessantes, pois revelam diversidade ou situações que merecem atenção especial do analista de políticas públicas.
 

#EXERCÍCIO 8 - Exercícios aplicados (educação e saúde)

#EDUCAÇÃO
 
#8A Distribuição de rede_escolar (FA/FR).

 #FA
 
 table(dados$rede_escolar)
 
 #privada = 2796
 #pública = 7204
 
 #FR
 prop.table(table(dados$rede_escolar)) * 100
 
 #privada = 27,96%
 #pública = 72,04%
 
 tab <- table(dados$rede_escolar)
 cbind(Frequencia_Absoluta = tab,
       Frequencia_Relativa = round(prop.table(tab)*100, 1))
 
 #Interpretação: a frequência absoluta mostra quantos indivíduos estão em cada tipo de rede escolar, enquanto a relativa mostra a proporção em relação ao total

 
 #8B Compare tempo_estudo_h por escolaridade com boxplots.
 
 library(ggplot2)
 
 ggplot(dados, aes(x = escolaridade, y = tempo_estudo_h)) +
   geom_boxplot(fill = "lightblue") +
   labs(
     x = "Escolaridade",
     y = "Tempo de estudo (horas)",
     title = "Boxplots de tempo de estudo por escolaridade"
   )
 
#8C Interprete: há padrão monotônico com a ordem da escolaridade?
 
 #Sim, há um padrão monotônico claro na relação entre escolaridade e tempo de estudo.
 #Fundamental: apresenta os menores valores de tempo de estudo, com mediana baixa e concentração próxima de poucas horas semanais.
 #Médio: já mostra um aumento, com mediana mais alta e maior dispersão, indicando que os estudantes dedicam mais tempo.
 #Superior: concentra os maiores tempos de estudo, com mediana elevada e presença de outliers que chegam a mais de 30 horas.
 #Isso caracteriza um padrão crescente (monotônico): conforme aumenta o nível de escolaridade, cresce também o tempo de estudo. 
 
 
 #SAUDE
 
 #8A Histograma de pressao_sistolica e reporte média/mediana/DP.
 
 # Histograma
 hist(dados$pressao_sistolica,
      main = "Histograma de Pressão Sistólica",
      xlab = "Pressão Sistólica (mmHg)",
      col = "lightblue",
      border = "white")
 
 
 # Estatísticas descritivas
 mean(dados$pressao_sistolica, na.rm = TRUE)     
 # média = 122.1863
 median(dados$pressao_sistolica, na.rm = TRUE)   
 # mediana = 122
 sd(dados$pressao_sistolica, na.rm = TRUE)      
 # desvio padrão = 14.05137
 
 
 
 #8B Cruzamento diagnostico × plano_saude (% por coluna).
 
 tab_cross <- table(dados$diagnostico, dados$plano_saude)
 tab_cross
 
 prop_col <- prop.table(tab_cross, margin = 2) * 100
 round(prop_col, 1)
 
 
 #ambos nenhum privado  SUS
 #DM       0.9    1.1     0.7  0.9
 #HAS     12.1   12.2    12.1 12.5
 #outros   5.5    6.0     6.6  6.0
 #sem     81.4   80.7    80.6 80.7
 
 #8C Interprete: qual diagnóstico é mais prevalente em cada plano?

 #Em todos os planos, a maioria dos indivíduos está na categoria “sem diagnóstico” (~80%). 
 #Entre os diagnósticos, o mais prevalente em todos os planos é HAS (hipertensão) (~12%). 
 #O cruzamento mostra que a distribuição de diagnósticos é bastante semelhante entre os tipos de plano de saúde, com predominância de ausência de diagnóstico e hipertensão como condição mais frequente entre os casos registrados.

 #EXERCÍCIO 9 - Desafio
 
 #Crie uma tabela-síntese por escolaridade com:
 
#n, média e mediana de idade, DP, mínimo e máximo:

 
 library(dplyr)
 
 dados %>%
   group_by(escolaridade) %>%
   summarise(
     n = n(),
     media_idade = mean(idade, na.rm = TRUE),
     mediana_idade = median(idade, na.rm = TRUE),
     dp_idade = sd(idade, na.rm = TRUE),
     min_idade = min(idade, na.rm = TRUE),
     max_idade = max(idade, na.rm = TRUE)
   )
 
 #escolaridade     n media_idade mediana_idade dp_idade min_idade max_idade
 #<ord>        <int>       <dbl>         <dbl>    <dbl>     <int>     <int>
 #1 Fundamental   3399        40.2            40     12.4        18        80
 #2 Médio         4056        40.5            40     12.6        18        80
 #3 Superior      2545        40.4            40     12.5        18        80
 
 #FA/FR de plano_saude dentro de cada escolaridade:
 
 tab <- table(dados$escolaridade, dados$plano_saude)
 tab
 #ambos nenhum privado  SUS
 #Fundamental   105    857     415 2022
 #Médio         260    719     975 2102
 #Superior      269    292    1084  900

 
  round(prop.table(tab, margin = 1) * 100, 1)
  #ambos nenhum privado  SUS
  #Fundamental   3.1   25.2    12.2 59.5
  #Médio         6.4   17.7    24.0 51.8
  #Superior     10.6   11.5    42.6 35.4
  
  
 #Produza dois gráficos:
  #1. Barras empilhadas de plano_saude por escolaridade (proporções).
  
  dados %>%
    group_by(escolaridade, plano_saude) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(escolaridade) %>%
    mutate(prop = n / sum(n) * 100) %>%
    ggplot(aes(x = escolaridade, y = prop, fill = plano_saude)) +
    geom_bar(stat = "identity") +
    labs(
      title = "Barras empilhadas de plano de saúde por escolaridade (proporções)",
      x = "Escolaridade",
      y = "Proporção (%)",
      fill = "Plano de Saúde"
    ) +
    theme_minimal()
  
  #2. Boxplot de tempo_estudo_h por escolaridade
  
  library(ggplot2)
  
  ggplot(dados, aes(x = escolaridade, y = tempo_estudo_h)) +
    geom_boxplot(fill = "lightblue", color = "darkblue") +
    labs(
      title = "Boxplot de tempo de estudo por escolaridade",
      x = "Escolaridade",
      y = "Tempo de estudo (horas)"
    ) +
    theme_minimal()
 