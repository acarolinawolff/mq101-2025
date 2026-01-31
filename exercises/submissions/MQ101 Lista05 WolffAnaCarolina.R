
# ============================================================
# MQ101 — Métodos Quantitativos para Políticas Públicas
# Lista de Exercícios #05
# ------------------------------------------------------------

# Nome:Ana Carolina Wolff
# RA: 23202510326
# Turma: PPU-009 - MÉTODOS QUANTITATIVOS - TPPU00920253 (2025.3 - 4T234)
# Data: 17/12/2025
# Descrição: Teste de Hipóteses e Regressão

knitr::opts_chunk$set(
  echo = TRUE,
  message = FALSE,
  warning = FALSE,
  dpi = 96
)
library(tidyverse)
library(broom)
set.seed(123)

#Geração da base de dados fictícia
n <- 400

dados <- tibble(
  id = 1:n,
  idade = round(rnorm(n, mean = 40, sd = 12)),
  sexo = sample(c("F", "M"), n, replace = TRUE, prob = c(0.55, 0.45)),
  renda = round(rlnorm(n, meanlog = log(2500), sdlog = 0.5), 0),
  escolarid = sample(c("fundamental", "medio", "superior"),
                     n, replace = TRUE, prob = c(0.30, 0.40, 0.30)),
  ideologia = round(runif(n, 0, 10), 0), # 0 = esquerda, 10 = direita
  apoio_gov = rbinom(n, 1, plogis(-1 + 0.015*(idade - 40) +
                                    0.4*(sexo == "F") +
                                    0.5*(renda > 3000))),
  satisf_gov = pmin(pmax(
    round(3 + 2*apoio_gov + 0.001*(renda - 2500) +
            rnorm(n, 0, 2), 0), 0), 10),
  protesto = rbinom(n, 1, plogis(-2 + 0.3*(ideologia <= 4) - 0.2*apoio_gov))
)
glimpse(dados)

#Exercício 01 - Exploração descritiva e gráficos básicos

#1.1
#média, mediana, desvio-padrão e quartis de idade, renda e satisf_gov:

summary(dados$idade)
summary(dados$renda)
summary(dados$satisf_gov)

# Média
mean(dados$idade, na.rm = TRUE)
mean(dados$renda, na.rm = TRUE)
mean(dados$satisf_gov, na.rm = TRUE)

# Mediana
median(dados$idade, na.rm = TRUE)
median(dados$renda, na.rm = TRUE)
median(dados$satisf_gov, na.rm = TRUE)

# Desvio-padrão
sd(dados$idade, na.rm = TRUE)
sd(dados$renda, na.rm = TRUE)
sd(dados$satisf_gov, na.rm = TRUE)

# Quartis
quantile(dados$idade, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
quantile(dados$renda, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
quantile(dados$satisf_gov, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)

#proporção de respondentes por categoria de sexo e escolaridade

prop.table(table(dados$sexo))
prop.table(table(dados$escolarid))

# Proporção de respondentes com apoio_gov = 1
mean(dados$apoio_gov == 1, na.rm = TRUE)


#1.2. Construa os seguintes gráficos:
#um histograma de renda (use pelo menos 20 quebras)

ggplot(dados, aes(x = renda)) +
  geom_histogram(bins = 20)

#um histograma ou gráfico de barras da distribuição de satisf_gov;

ggplot(dados, aes(x = satisf_gov)) +
geom_histogram(bins = 20)

#um gráfico de barras com a distribuição de escolaridade

ggplot(dados, aes(x = escolarid)) +
  geom_bar()

#1.3 Comente
#se a distribuição de renda parece simétrica ou assimétrica: no gráfico, a distribuição de renda aparece assimétrica, com concetração à esquerda e uma longa cauda que se estende para os valores mais altos.
#se satisf_gov parece concentrada em valores baixos, médios ou altos: a variável satisf_gov parece concentrada em valores médios, entre os valores 4 e 5.
#como se distribuem sexo e escolarid na amostra:há uma predominância feminina e de nível médio.

ggplot(dados, aes(x = escolarid, fill = sexo)) +
  geom_bar(position = "dodge")

#EXERCÍCIO 2 - Escolha de testes estatísticos

#2.1. Identifique o tipo de cada variável: categórica ou contínua. 
#2.2. Indique qual teste bivariado é mais adequado, considerando o Capítulo 8 (análise tabular e qui-quadrado, diferença de médias, correlação, regressão simples).

#a. 
#tipo de sexo: qualitativa nominal
#tipo de apoio_gov: qualitativa nominal
#teste bivariado sugerido: análise tabular/qui-quadrado

#b. 
#escolarid (3 categorias): qualitativa ordinal
#apoio_gov (0/1):qualitativa nominal
#teste bivariado sugerido:análise tabular/qui-quadrado

#c.
#satisf_gov (0–10): quantitativa discreta
#apoio_gov (0/1):qualitativa nominal
#teste bivariado sugerido:Teste t de diferença de médias

#d. 
#satisf_gov (0–10):quantitativa discreta
#renda (contínua): quantitativa contínua.
#teste bivariado sugerido:correlação e/ou regressão simples

#e. 
#satisf_gov (0–10):quantitativa discreta
#ideologia (0–10):quantitativa discreta
#teste bivariado sugerido:correlação e/ou regressão simples

#Exercício 3 – Teste qui-quadrado: sexo e apoio ao governo

#3.1. Construa uma tabela de contingência entre sexo e apoio_gov. Apresente também proporções por coluna (ou por linha) para facilitar a interpretação.

tab_sexo_apoio <- table(dados$sexo, dados$apoio_gov)
tab_sexo_apoio
prop.table(tab_sexo_apoio, margin = 2)


#3.2 Formule as hipóteses:
#\(H_0\): não há associação entre sexo e apoio_gov na população;
#\(H_1\): há associação entre sexo e apoio_gov.

#3.3. Aplique o teste de qui-quadrado em R:
chisq.test(tab_sexo_apoio)

#3.4Em texto, interprete:
#O resultado do teste indicou um valor de qui-quadrado de aproximadamente 0,68, com 1 grau de liberdade e um valor-p de 0,4063. 
#Como o valor-p é muito superior ao nível de significância usual de 5%, não rejeitamos a hipótese nula.
#Isso significa que, com base nesta amostra, não há evidências estatísticas de que o sexo influencie o apoio ao governo.

#3.5 COnstrua gráfico

ggplot(dados, aes(x = sexo, fill = factor(apoio_gov))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(fill = "Apoio ao governo")

#Comente: sim, há coerência entre o gráfico e o resultado do teste.
#embora visualmente os homens pareçam apresentar um apoio ligeiramente maior, a diferença é pequena e não significativa do ponto de vista estatístico.

#Exercício 4 – Diferença de médias: renda entre apoiadores e não apoiadores

#4.1. Produza um resumo numérico de renda por apoio_gov (média, desvio-padrão, tamanho de cada grupo).

dados %>%
  group_by(apoio_gov) %>%
  summarise(
    media_renda = mean(renda),
    sd_renda = sd(renda),
    n = n()
  )

#4.2. Formule as hipóteses:
# \(H_0: \mu_{\text{apoia}} = \mu_{\text{não apoia}}\)
#\(H_1: \mu_{\text{apoia}} \neq \mu_{\text{não apoia}}\)

#4.3. Aplique o teste \(t\) para diferença de médias (assuma, inicialmente, variâncias desiguais):

t.test(renda ~ apoio_gov, data = dados)

#4.4. Em texto, interprete:

#Estimativa da diferença de médias: a média da renda entre os que não apoiam o governo foi de 2.772,36 reais, enquanto entre os que apoiam foi de 3.025,48 reais. A diferença estimada entre as médias é de aproximadamente 253,12 reais (3025,48 – 2772,36).
#Intervalo de confiança de 95%: o intervalo de confiança para a diferença de médias foi de [-576,66 ; 70,41]. Como esse intervalo inclui o valor 0, não podemos afirmar que exista diferença estatisticamente significativa entre os grupos.
#O valor-p obtido foi 0,1247. Esse valor é maior que 0,05, indicando que a diferença observada pode ser explicada pelo acaso.
#Conclusão sobre H_0 a 5%: como o valor-p é maior que o nível de significância de 5%, não rejeitamos a hipótese nula.
#Portanto, não há evidências estatísticas de que a renda difira entre apoiadores e não apoiadores do governo nesta amostra.
# embora os apoiadores tenham uma média de renda ligeiramente maior, essa diferença não é estatisticamente significativa.

#4.5. Construa um boxplot de renda por apoio_gov e comente brevemente a relação entre o gráfico e o resultado do teste.

ggplot(dados, aes(x = factor(apoio_gov), y = renda)) +
  geom_boxplot()

#Comentário:
#No gráfico, observa-se que as medianas são próximas e as caixas e limites externos se sobrepõem bastante, sugerindo distribuições semelhantes entre os grupos. Isso sugere que a distribuição das rendas nos dois grupos é semelhante e que a diferença visual não é muito marcada.
#No teste t, essa impressão visual se confirma: a diferença média foi de cerca de R$ 253, mas o intervalo de confiança inclui o zero ([-576 ; 70]) e o valor-p (0,1247) é maior que 0,05. Isso significa que a diferença observada não é estatisticamente significativa.
#o gráfico mostra uma diferença pequena e com grande sobreposição entre os grupos, e o teste t confirma que essa diferença não é significativa do ponto de vista estatístico.


#Exercício 5 – Correlação: renda, ideologia e satisfação com o governo

#5.1. Calcule a matriz de correlações de Pearson entre renda, ideologia e satisf_gov                                                                               completos):
  
dados %>%
  select(renda, ideologia, satisf_gov) %>%
  cor(use = "complete.obs")


#5.2 Para cada par de variáveis, com base na matriz:

#Renda e ideologia: correlação +0,0205 (positiva); associação muito fraca;não há associação relevante entre renda e ideologia na amostra
#Renda e satisf_gov: correlação +0,5934 (positiva); associação moderada a forte; assim, quanto maior a renda, maior tende a ser a satisfação com o governo, em termos gerais.
#Ideologia e satisf_gov: correlação −0,0796 (negativa); associação muito fraca; sugere que a ideologia tem pouca relação com a satisfação com o governo.

#5.3 Construa um gráfico de dispersão de renda (eixo x) por satisf_gov (eixo y), com linha de tendência linear:
  
ggplot(dados, aes(x = renda, y = satisf_gov)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = TRUE)

#5.4. Em texto, comente o padrão visual do gráfico, relacionando-o ao sinal e magnitude da correlação estimada.

#O gráfico de dispersão com linha de regressão mostra uma tendência clara de associação positiva entre renda e satisfação com o governo: conforme a renda aumenta, a satisfação tende também a crescer. 
#Esse padrão visual está em plena concordância com o coeficiente de correlação estimado (+0,59), que indica uma relação moderada a forte. 
#A inclinação positiva da linha e a proximidade relativa dos pontos reforçam a ideia de que indivíduos com maior renda apresentam, em média, níveis mais altos de satisfação, ainda que exista alguma dispersão nos dados.

#Exercício 6 – Regressão linear simples:  satisfação e renda

#Considere o modelo:  [ _i = _0 + _1 ,_i + _i]

#6.1. Estime o modelo em R:

mod1 <- lm(satisf_gov ~ renda, data = dados)
summary(mod1)

#6.2. Em texto, interprete:

#O modelo de regressão linear simples indica que, quando a renda é igual a zero, a satisfação esperada com o governo seria de aproximadamente 1,35 pontos, embora essa interpretação literal do intercepto não seja muito significativa na prática. 
#Já o coeficiente da renda mostra um efeito positivo: para cada aumento de 1.000 reais na renda, espera-se um acréscimo médio de cerca de 1 ponto na satisfação com o governo. 
#O valor-p associado à renda é extremamente baixo (menor que 0,001), fornecendo forte evidência estatística de associação linear entre renda e satisfação. 
#assim, conclui-se que a renda é um preditor significativo e que indivíduos com maior renda tendem, em média, a apresentar maior satisfação com o governo.

#6.3. Utilize o broom para obter uma tabela organizada:
  
  tidy(mod1)
  
#O output do modelo mostra que o intercepto estimado é 1,35, com erro-padrão de 0,227, resultando em uma estatística t de 5,94 e valor-p de 6,3e-9, o que indica que ele é estatisticamente diferente de zero, embora sua interpretação literal (satisfação quando a renda é zero) não seja muito relevante. 
#já o coeficiente da renda é 0,00102, com erro-padrão extremamente baixo (0,0000692), o que gera uma estatística t muito elevada (14,7) e um valor-p praticamente nulo (2,08e-39). 
#Esses resultados evidenciam que a renda tem efeito positivo e altamente significativo sobre a satisfação com o governo, confirmando a robustez da associação linear encontrada.
  
  
#6.4. Refaça o gráfico de dispersão com reta de regressão (como no Exercício 5) e escreva um parágrafo relacionando:

  ggplot(dados, aes(x = renda, y = satisf_gov)) +
    geom_point(alpha = 0.4) +
    geom_smooth(method = "lm", se = TRUE)

#O gráfico de dispersão com a reta de regressão mostra uma tendência positiva clara entre renda e satisfação com o governo, em linha com o coeficiente estimado \hat {\beta }_1=0,00102. 
#A inclinação positiva da reta reflete o sinal do coeficiente, indicando que maiores valores de renda estão associados a maiores níveis de satisfação. 
#Além disso, o valor elevado da estatística t (14,7) e o valor-p praticamente nulo (< 0,001) confirmam a significância estatística desse efeito, evidenciando que a associação linear observada no gráfico não é fruto do acaso, mas sim uma relação consistente entre as variáveis
  
#Exercício 7 – Diagnóstico simples do modelo
  
#Usando o modelo do Exercício 6 (mod1):
  
  mod1 <- lm(satisf_gov ~ renda, data = dados)
  summary(mod1)
  
#7.1. Extraia resíduos e valores ajustados:
  dados_diag <- augment(mod1)
  glimpse(dados_diag)
  
#7.2. Construa:

#um gráfico de resíduos vs valores ajustados (.resid vs .fitted);
  
  ggplot(dados_diag, aes(x = .fitted, y = .resid)) +
    geom_point(alpha = 0.4) +
    geom_hline(yintercept = 0, linetype = "dashed")
  
#um gráfico QQ-plot dos resíduos.
  
  ggplot(dados_diag, aes(sample = .resid)) +
    stat_qq() +
    stat_qq_line()
  
#7.3. Em texto, com base nos gráficos, comente:
#se há indícios fortes de heterocedasticidade (padrão em funil, por exemplo);
#se a distribuição dos resíduos parece muito distante de algo aproximadamente normal.
 
#no gráfico de resíduos versus valores ajustados, não se observa um padrão em funil ou outro indício forte de heterocedasticidade: a dispersão dos resíduos parece relativamente constante ao longo da reta, sugerindo que a variância dos erros é aproximadamente homogênea. 
#o gráfico Q-Q mostra que os resíduos seguem de forma razoável a linha de referência, com pequenas desvios nas extremidades. Isso indica que a distribuição dos resíduos não está muito distante da normalidade, havendo apenas leves discrepâncias nas caudas. 
# os diagnósticos sugerem que as principais suposições do modelo linear — homocedasticidade e normalidade dos resíduos — estão suficientemente atendidas para que os resultados sejam considerados confiáveis.
  
  
#7.4. Explique, em poucas linhas, por que vale a pena olhar pelo menos esses dois gráficos antes de confiar inteiramente nas inferências do modelo.
  
#Vale a pena olhar esses dois gráficos, porque eles ajudam a verificar se as condições básicas do modelo estão sendo atendidas. 
#O gráfico de resíduos mostra se os erros estão distribuídos de forma equilibrada, sem padrões que indiquem problemas como variação desigual. 
#Já o gráfico de normalidade indica se os erros seguem aproximadamente a distribuição esperada. 
#Se esses pontos não forem avaliados, corre-se o risco de confiar em resultados que podem estar distorcidos pelas limitações do modelo.
  

#Exercício 8 – Regressão com variável dummy e diferença de médias
  
#Considere o modelo:
#[ _i = _0 + _1 ,_i + _i]
#onde apoio_gov é uma variável indicadora (0 = não apoia; 1 = apoia).

#8.1. Estime o modelo:
  
    mod2 <- lm(satisf_gov ~ apoio_gov, data = dados)
  summary(mod2)
  
#8.2. Calcule as médias de satisf_gov nos dois grupos (apoio_gov = 0 e apoio_gov = 1) e compare com os coeficientes de mod2.
#No modelo com variável dummy, o intercepto representa a média de satisfação dos que não apoiam o governo (3,41). 
#O coeficiente da dummy mostra a diferença entre os grupos (2,27), e somando os dois obtemos a média dos apoiadores (5,68). 
#Ou seja, os coeficientes da regressão reproduzem exatamente as médias observadas nos dois grupos.
  
  
#8.3. Mostre, com base nos resultados numéricos, que:
# \(\hat{\beta}_0\) corresponde à média de satisf_gov para apoio_gov = 0;
# \(\hat{\beta}_1\) corresponde à diferença entre as médias dos dois grupos.
  
#O intercepto (\hat {\beta }_0=3,412) corresponde exatamente à média de satisfação com o governo para o grupo que não apoia (apoio_gov = 0).
#O coeficiente da dummy (\hat {\beta }_1=2,268) representa a diferença entre as médias dos dois grupos.
#Assim, a média dos que apoiam o governo é dada por: \hat {\beta }_0+\hat {\beta }_1=3,412+2,268=5,680
#Comparando:
#Média dos que não apoiam = 3,41
#Média dos que apoiam = 5,68
#Diferença = 2,27 → que coincide com \hat {\beta }_1.
  
#8.4. Aplique:
  
    t.test(satisf_gov ~ apoio_gov, data = dados)
    
#Compare o valor-p de apoio_gov no summary(mod2) com o valor-p do t.test. Em texto, discuta a relação entre os dois resultados.
    
#O valor-p do coeficiente de apoio_gov na regressão é praticamente zero (< 2e-16), assim como o valor-p do teste t (< 2,2e-16). 
#Isso acontece porque os dois testes são, na prática, equivalentes: ambos verificam se há diferença significativa entre as médias de satisfação dos que apoiam e dos que não apoiam o governo. 
#assim, os dois resultados confirmam a mesma conclusão: a diferença entre os grupos é estatisticamente muito significativa.
    