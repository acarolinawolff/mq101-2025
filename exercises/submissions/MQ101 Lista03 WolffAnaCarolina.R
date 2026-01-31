# ============================================================
# MQ101 — Métodos Quantitativos para Políticas Públicas
# Lista de Exercícios #03 
# ------------------------------------------------------------

# Nome:Ana Carolina Wolff
# RA: 23202510326
# Turma: PPU-009 - MÉTODOS QUANTITATIVOS - TPPU00920253 (2025.3 - 4T234)
# Data: 09/10/2025
# Descrição: Gráficos e Visualização de Dados


# Instale apenas se necessário (preparação de ambiente)

install.packages(c("tidyverse","readr","ggplot2","scales","viridis",
"electionsBR"))
install.packages(c("tidyverse", "readr", "ggplot2", "scales"))

set.seed(101)
library(tidyverse)

#XERCÍCIO 1 - Aquecimento – Tabela vs. Gráfico (dados internos)

#Carregue a base cars; exiba as 10 primeiras linhas (tabela); produza um gráfico de dispersão speed × dist; interprete.

data(cars) 
head(cars, 10)


ggplot(cars, aes(x = speed, y = dist)) +
  geom_point(alpha = 0.6) +
  labs(title = "speed x distance", x = "speed", y = "distance") +
  theme_minimal()

#INTERPRETAÇÃO: os pontos formam um padrão que sobe da esquerda para a direita. Isso indica uma correlação positiva: à medida que uma variável (velocidade) aumenta, a outra (distância) também tende a aumentar.

#EXERCÍCIO 2 - Distribuições univariadas e grupos

data(mtcars)
mtcars <- mtcars |>
mutate(cyl = as.factor(cyl))

#histogramas/densidades de mpg (milhas por galão) e boxplot por cyl (cilindros). Interpretação breve.

library(ggplot2)

#histograma
ggplot(mtcars, aes(x = mpg )) +
  geom_histogram(bins = 10) +
  labs(title = "Histograma MPG",
       x = "MPG", y = "Frequência") +
  theme_minimal()

#o histograma de MPG mostra como os valores de consumo de combustível (milhas por galão) estão distribuídos entre os veículos da amostra;
#a distribuição é centrada em torno de valores médios, revelando que veículos extremamente econômicos ou muito gastadores são menos frequentes.


#densidade
ggplot(mtcars, aes(x = mpg)) +
  geom_density(alpha = 0.4) +
  labs(title = "Densidade MPG",   # Tirei o ')' de dentro do título
       x = "MPG",                   # Removido o ')"' extra
       y = "Densidade") +
  theme_minimal()

#o gráfico de densidade de MPG mostra como os valores de consumo de combustível estão distribuídos entre os veículos da amostra;
#o pico da curva está em torno de 17 MPG, indicando que essa é a faixa mais comum;
#como a densidade diminui conforme os valores se afastam desse centro, percebe-se que veículos muito econômicos ou pouco econômicos são menos frequentes.


#boxplot
ggplot(mtcars, aes(x = cyl, y = mpg, fill = cyl)) +
  geom_boxplot(show.legend = TRUE, outlier.alpha = 0.4) +
  coord_flip() +
  labs(title = "Boxplot",
       x = "Cyl", y = "MPG") +
  theme_minimal()

#O boxplot mostra como o consumo de combustível (MPG) varia conforme o número de cilindros dos veículos.
#quanto mais cilindros um carro tem (8 > 6 > 4), menor tende a ser sua eficiência (menor o mpg).

#EXERCICIO 3 - Série temporal simples
#escolha uma base (AirPassengers ou airquality), crie gráfico(s) de linha/pontos e destaque tendência.

data ("airquality")

library(tibble)

aq <- airquality |>
   as_tibble() |>
   drop_na(Ozone) |>
   mutate(Month = factor(Month),
            Day = as.integer(Day))

#grafico de linhas e pontos

ggplot(aq, aes(x = Day, y = Ozone)) +
  geom_line(color = "blue") +      # Adiciona a linha conectando os pontos
  geom_point(alpha = 0.6) +     # Adiciona os pontos
  facet_wrap(~ Month, scales = "free_x") + # Separa o gráfico por Mês
  labs(title = "Níveis de Ozônio por Dia e Mês",
       x = "Dia do Mês",
       y = "Ozônio (ppb)") +
  theme_minimal()

#o gráfico da base airquality apresenta os níveis de ozônio (em ppb) ao longo dos dias, separados por mês (maio a setembro).
#o gráfico de linhas e pontos facilita acompanhar a variação do ozônio ao longo do tempo.
#a tendência geral mostra que os níveis de ozônio tendem a ser mais altos nos meses de verão (julho e agosto) e mais baixos em maio e setembro.


#EXERCÍCIO 4 - Relações bivariadas e transformações
#Relacionar mpg e wt; testar tendências (lm/loess) e escala log quando fizer sentido

data(mtcars)

ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point(color = "steelblue", size = 2, alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.8, color = "darkred") +
  labs(
    title = "Relação entre peso do carro e consumo de combustível",
    x = "peso do carro (1000 lb)", y = "milhas por galão (mpg)"
  ) +
  theme_minimal()


ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point(color = "steelblue", size = 2, alpha = 0.6) +
  geom_smooth(method = "loess", se = TRUE, linewidth = 0.8, color = "darkred") +
  labs(
    title = "Relação entre peso do carro e consumo de combustível",
    x = "peso do carro (1000 lb)", y = "milhas por galão (mpg)"
  ) +
  theme_minimal()

#na relação mpg (milhas por galão) e wt (peso do carro, em milhares de libras) há uma tendência negativa clara: conforme o peso aumenta, o consumo de combustível (mpg) diminui.
#o teste de ajuste lm mostra uma linha decrescente, captando bem a tendência geral.
#o teste de ajuste loess revela pequenas curvaturas, mas a relação é quase linear.
#a diferença é que o loess pode captar curvaturas sutis, enquanto o lm força uma linha reta.
#na relação entre mpg e wt em mtcars, a escala log não é necessária porque os valores não variam em ordens de grandeza muito diferentes, mas pode ser usada como diagnóstico para verificar se a relação segue uma lei de potência.


#EXERCÍCIO 5 - Facetas (comparar subgrupos)

mtcars <- mtcars |>
  mutate(am = factor(am, labels = c("Automtico","Manual")))

ggplot(mtcars, aes(x = hp, y = mpg)) +
  geom_point(color = "steelblue", size = 2, alpha = 0.6) +
  facet_wrap(~am) +
  labs(
    title = "Relação entre potência do motor e consumo de combustível",
    x = "potência do motor (hp)", y = "milhas por galão (mpg)"
  ) +
  theme_minimal()


#ajuste lm por painel

ggplot(mtcars, aes(x = hp, y = mpg)) +
  geom_point(color = "steelblue", size = 2, alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.8, color = "darkred") +
  facet_wrap(~am) +
labs(
    title = "Relação entre potêcia do motor e consumo de combustível",
    x = "potência do motor", y = "milhas por galão (mpg)"
  ) +
  theme_minimal()


#ao comparar como a relação entre potência do motor (hp) e consumo de combustível (mpg) se comporta em carros automáticos versus manuais verificou-se que, em ambos os casos, a relação é negativa, ou seja, motores mais potentes consomem mais combustível. No entanto, nos carros manuais, os dados se concentram mais em motores de menor potência, com consumo relativamente melhor.


#EXERCÍCIO 6 - Simulação I – Correlação controlada
#Simule três níveis de correlação (p = 0.2, 0.6, 0.9); faça três dispersões; compare.

n <- 1000; rhos <- c(0.2, 0.6, 0.9)
 sim <- purrr::map_dfr(rhos, \(rho) {
 x <- rnorm(n); e <- rnorm(n)
  y <- rho*x + sqrt(1 - rho^2)*e
  tibble(rho = rho, x = x, y = y)
  })

 ggplot(sim, aes(x = x, y = y)) +
   geom_point(color = "steelblue", size = 1, alpha = 0.6) +
   geom_smooth(method = "lm", se = TRUE, linewidth = 0.8, color = "darkred") +
   facet_wrap(~rho, scales = "free") +
   labs(
     title = "Comparação de dados simulados com diferentes correlações (rho)",
     x = "variável x", y = "variável y"
   ) +
   theme_minimal()
 
# 0.2 (Correlação Fraca): o padrão é de alta dispersão, com pontos espalhados aleatoriamente, e a linha de tendência (regressão) é altamente incerta;
# 0.6 (Correlação Moderada): Os pontos mostram um padrão ascendente perceptível, mas com dispersão notável, indicando uma relação razoável.
# 0.9 (Correlação Forte): O padrão é altamente linear, com os pontos agrupados firmemente em torno da linha de regressão, demonstrando alta previsibilidade.
# Em todos os casos, a área sombreada (erro) diminui à medida que a correlação se torna mais forte, refletindo um melhor ajuste do modelo.
 
 
#EXERCÍCIO 7 - Simulação II – Diferenças entre grupos
 
#Simule grupo A: N(0, 1); grupo B: N(1, 1.8);faça histogramas/densidades/boxplot/violin; 
#interprete
  

 n <- 1000
 df_grupos <- tibble(
 grupo = rep(c("A","B"), each = n),
  valor = c(rnorm(n, 0, 1), rnorm(n, 1, 1.8)))
 
#Histograma
 
 ggplot(df_grupos, aes(x = valor, fill = grupo)) +
   geom_histogram(aes (y = after_stat (density)), binwidth = 0.4, alpha = 0.6, position = "identity") +
   labs(title = "Comparação de Distribuições",
        x = "valor", y = "Densidade", fill = "grupo") +
   theme_minimal()
 
#DENSIDADE
 
 ggplot(df_grupos, aes(x = valor, fill = grupo)) +
   geom_density(aes (y = after_stat (density)), binwidth = 0.4, alpha = 0.6, position = "identity") +
   labs(title = "Comparação de Distribuições",
        x = "valor", y = "Densidade", fill = "grupo") +
   theme_minimal()
 
 
#BLOXPOT
 
 ggplot(df_grupos, aes(x = grupo, y = valor, fill = grupo)) +
   geom_boxplot(alpha = 0.7) +
   labs(title = "Comparação de Distribuições",
        x = "grupo", y = "valor", fill = "grupo") +
   theme_minimal()
 
 
 #VIOLIN
 
 ggplot(df_grupos, aes(x = grupo, y = valor, fill = grupo)) +
   geom_violin(alpha = 0.3, trim = FALSE) +
   labs(title = "Comparação de Distribuições",
        x = "grupo", y = "valor", fill = "grupo") +
   theme_minimal()

#Interpretação:
#os quatro gráficos mostram diferentes formas de comparar as distribuições (centro, dispersão e forma) dos grupos A (vermelho) e B (azul);
#o histograma mostra a frequência/densidade dos valores, permitindo ver a sobreposição entre os grupos e onde cada um se concentra;
#o gráfico de densidade destaca melhor os picos e a forma da distribuição, facilitando perceber se os grupos têm médias ou dispersões diferentes;
#o boxplot resume a distribuição com mediana, quartis e possíveis outliers, sendo útil para comparar rapidamente posição central e variabilidade entre os grupos;
#o violin combina o boxplot com a densidade, mostrando tanto resumo estatístico quanto a forma da distribuição.
 


#EXERCÍCIO 8 - educ_saude.csv – exploração e gráficos
 
 
 library(readr)
 educ <- read.csv (file.choose())
 glimpse(educ)
 
#Importe; explore; escolha variáveis; 
#faça histogramas/densidades/boxplots/dispersões/facetas; 
#interprete.
 
#variáveis escolhidas: plano de saúde e pressão sistolica
 
#Histograma
 
 ggplot(educ, aes(x = pressao_sistolica, fill = plano_saude)) +
   geom_histogram(aes(y = after_stat(density)), 
                  binwidth = 5, color = "black", alpha = 0.6) +
   labs(
     title = "Distribuição de Pressão Sistólica por Plano de Saúde",
     x = "Pressão Sistólica (mmHg)",
     y = "Densidade"
   ) +
   theme_minimal()
 

#Densidades
 
 ggplot(educ, aes(x = pressao_sistolica, color = plano_saude)) +
   geom_density(linewidth = 1.1) +
   labs(
     title = "Distribuição de Pressão Sistólica por Plano de Saúde",
     subtitle = "Curvas de Densidade Estimada",
     x = "Pressão Sistólica (mmHg)",
     y = "Densidade Estimada",
     color = "Plano de Saúde"
   ) +
   theme_minimal()
 
 
 #Boxplot
 
 
 ggplot(educ, aes(x = pressao_sistolica, fill = plano_saude)) +
     geom_boxplot(alpha = 0.7) +
     labs(title = "Comparação da Pressão Sistólica por Tipo de Plano de Saúde",
           x = "Pressão Sistólica (mmHg)",
     fill = "Plano de Saúde") +
    theme_minimal()

 
 #Dispersões
 
 ggplot(educ, aes(x = plano_saude, y = pressao_sistolica, color = plano_saude)) +
   geom_jitter(width = 0.2, alpha = 0.6, size = 1.8) +
   labs(
     title = "Dispersão da Pressão Sistólica por Plano de Saúde",
     x = "Plano de Saúde",
     y = "Pressão Sistólica (mmHg)",
     color = "Plano de Saúde"
   ) +
   theme_minimal()
 
 
 #facetas
 
 ggplot(educ, aes(x = pressao_sistolica, color = plano_saude)) +
   geom_density(linewidth = 1.1) +
   facet_wrap(~ plano_saude, scales = "free_y") +
   labs(
     title = "Distribuição de Pressão Sistólica por Plano de Saúde",
     subtitle = "Curvas de Densidade Estimada",
     x = "Pressão Sistólica (mmHg)",
     y = "Densidade Estimada",
     color = "Plano de Saúde"
   ) +
   theme_minimal()
 
#Interpretação: a pressão sistólica tem distribuição semelhante entre os diferentes tipos de plano de saúde, sem indícios de que algum grupo se destaque por valores muito mais altos ou baixos.


#EXERCÍCIO 9 - educ_saude.csv – figura final
 
#Contar uma história em um gráfico (título, eixos, legenda, caption, escala adequada).
#um gráfico bem rotulado que responda a uma pergunta clara.

#PERGUNTA: O nível de escolaridade (anos de estudo concluídos) difere significativamente entre os grupos de indivíduos segundo o tipo de plano de saúde?
 
 ggplot(educ, aes(x = plano_saude, y = anos_estudo, fill = plano_saude)) +
   geom_boxplot(
     alpha = 0.8,              
     ) +
   labs(
     title = "Comparação da Escolaridade por Tipo de Plano de Saúde",
     x = "Plano de Saúde",
     y = "Anos de Estudo Concluídos",
     fill = "Plano de Saúde"
   ) +
   theme_minimal()
   
   #Interpretação:O boxplot sugere uma associação entre tipo de plano de saúde e escolaridade. Indivíduos com planos privados ou ambos apresentam medianas mais altas de anos de estudo, enquanto usuários do SUS ou sem plano concentram-se em níveis mais baixos. Essa diferença reflete desigualdades socioeconômicas.