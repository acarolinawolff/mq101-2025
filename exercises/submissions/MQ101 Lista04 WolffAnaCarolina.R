# ============================================================
# MQ101 — Métodos Quantitativos para Políticas Públicas
# Lista de Exercícios #04 
# ------------------------------------------------------------

# Nome:Ana Carolina Wolff
# RA: 23202510326
# Turma: PPU-009 - MÉTODOS QUANTITATIVOS - TPPU00920253 (2025.3 - 4T234)
# Data: 23/11/2025
# Descrição: Probabilidade e inferência estatística

#EXERCICIO 1: Probabilidade como frequência de longo prazo (moeda viesada)

set.seed(123)

#Probabilidade de cara
p_cara <- 0.3

#Tamanhos de amostra

n_vec  <- c(1, 10, 100, 1000, 10000)

#Data frame para armazenar os resultados

result <- data.frame(
  n      = n_vec,
  p_cara = NA_real_
)

#completar o loop para simular os lançamentos


for (i in seq_along(n_vec)) {
    x <- rbinom(n = 1, size = n_vec[i], prob = 0.3)   
    result$p_cara[i] <- x / n_vec[i]
}

print(result)

#fazer um gráfico de linha da proporção de caras vs n

plot(result$n, result$p_cara,
     #type = "b",               
     pch = 19,                  
     col = "blue",              
     xlab = "Tamanho da amostra (n)",
     ylab = "Proporção de caras",
     main = "Simulação: proporção de caras vs n")

abline(h = p_cara, lty = 2, col = "red")  

#Interpretação: quanto maior a amostra, mais a frequência relativa converge para a probabilidade teórica de 0,3.


#EXERCÍCIO 2:Bernoulli, Binomial e probabilidades exatas (satisfação em saúde)

set.seed(123)

#Parâmetros
p <- 0.65
# probabilidade verdadeira de satisfação

n <- 20
# tamanho da amostra

# (1) Modelar satisfação como variável Bernoulli
# Cada usuário: Y ~ Bernoulli(p)

# (2) Simular uma amostra de n usuários
y <- rbinom(n, size = 1, prob = p)

print(y)

# Contar o número de satisfeitos
num_satisfeitos <- sum(y)
print(num_satisfeitos)

# (3a) Probabilidade de observar exatamente 12 satisfeitos
prob_12 <- dbinom(12, size = n, prob = p)

# (3b) Probabilidade de observar pelo menos 12 satisfeitos
prob_12_ou_mais <- 1 - pbinom(11, size = n, prob = p)

# (4) Resultados

print(prob_12)
print(prob_12_ou_mais)

#Interpretação:com p = 0,65, é comum encontrar 12 ou mais satisfeitos em 20 entrevistas, pois o valor esperado é cerca de 13.

#EXERCÍCIO 3 - Probabilidade condicional e independência (base saúde)


set.seed(123)

N <- 5000

dados_saude <- data.frame(
  sexo       = sample(c("F", "M"), size = N, replace = TRUE, prob = c(0.55, 0.45)),
  fumante    = rbinom(N, 1, 0.22),
  hipertenso = rbinom(N, 1, 0.30)
)

#(2) Estimar as probabilidades
P_fumante <- mean(dados_saude$fumante == 1)
P_fumante_F <- mean(dados_saude$fumante[dados_saude$sexo == "F"] == 1)
P_fumante_M <- mean(dados_saude$fumante[dados_saude$sexo == "M"] == 1)

#(4) Gráfico de barras da proporção de fumantes por sexo

#Dica: usar dplyr + ggplot2

library(dplyr); library(ggplot2)
tab_fumo <- dados_saude |>
  dplyr::group_by(sexo) |>
  dplyr::summarise(prop_fumante = mean(fumante))


ggplot(tab_fumo, aes(x = sexo, y = prop_fumante, fill=sexo)) +
  geom_col()+
  scale_fill_manual(values = c("F" = "pink", "M" = "lightblue"))+
  labs(x="Sexo", y="Proporçãoo de fumantes",
       title="Proporção de fumantes por sexo")

#Interpretação:
#A proporção de fumantes entre mulheres aparece ligeiramente maior do que entre homens.
#As diferenças, no entanto, são pequenas — ambas giram em torno de 0,22 (22%).
#Isso sugere que, nesta base simulada, o hábito de fumar não varia muito entre os sexos, ou seja, fumar é aproximadamente independente de sexo.


#EXERCÍCIO 4 - Probabilidade conjunta e regra do produto (saúde)


# (1) Probabilidades marginais e conjunta
P_hipertenso <- mean(dados_saude$hipertenso == 1)
P_fumante    <- mean(dados_saude$fumante == 1)
P_hip_e_fum  <- mean(dados_saude$hipertenso == 1 & dados_saude$fumante == 1)

# (2) Probabilidade condicional
P_hip_dado_fum <- mean(dados_saude$hipertenso[dados_saude$fumante == 1] == 1)

# (3) Regra do Produto:
P_produto <- P_hip_dado_fum * P_fumante

# (4) Interpretação

#Na base simulada, cerca de 30% dos indivíduos são hipertensos e 22% fumantes.
#A probabilidade conjunta de ser fumante e hipertenso coincide com o valor obtido pela regra do produto, mostrando consistência.
#Fumantes têm aproximadamente 30% de chance de serem hipertensos, um pouco acima da média geral, sugerindo associação entre tabagismo e hipertensão.

#EXERCÍCIO 5 - Bayes
# Objetivo: aplicar a fórmula de Bayes em um problema de triagem de fraudes em benefícios sociais


set.seed(123)

P_F         <- 0.02
P_T_dado_F  <- 0.9
P_T_dado_Fc <- 0.05
P_Fc <- 1 - P_F

# (1) Cálculo analítico de P(F|Alerta) pela fórmula de Bayes
P_F_dado_T <- (P_T_dado_F * P_F) /
  (P_T_dado_F * P_F + P_T_dado_Fc * P_Fc)


# (2) Simulação
N <- 100000
fraude <- rbinom(N, 1, P_F) 
alerta <- ifelse(           
  fraude == 1,
  rbinom(N, 1, P_T_dado_F), 
  rbinom(N, 1, P_T_dado_Fc) 
)


# (3) Estimar empiricamente P(F|Alerta)
P_empirico <- mean(fraude[alerta == 1] == 1)

# (4) Comparar e interpretar em texto.

c(
  Empirico   = P_empirico,
  Analitico  = P_F_dado_T,
  Diferencia = P_empirico - P_F_dado_T
)

#O cálculo mostra que, mesmo com sensibilidade de 90% e especificidade de 95%, a prevalência baixa (2%) faz com que apenas cerca de 27% dos alertas sejam fraudes reais. 
#Isso significa que a maioria dos alertas é de falsos positivos, e o sistema precisa de triagem adicional para evitar desperdício de tempo e recursos


#EXERCÍCIO 6 - Teorema Central do Limite (TCL) com renda

set.seed(123)

# (1) Gerar população assimétrica de rendas
N <- 100000
renda_pop <- rgamma(N, shape = 2, rate = 1/2500)

# (2) Função auxiliar para simular médias amostrais

simular_medias <- function(n, n_rep = 5000) {
  medias <- numeric(n_rep)
  for (i in seq_len(n_rep)) {
    amostra <- sample(renda_pop, n, replace = TRUE)
    medias[i] <- mean(amostra)
  }
  medias
}

# (3) Simular médias para n = 30
medias_n30 <- simular_medias(30)

# (4) Simular médias para n = 200
medias_n200 <- simular_medias(200)

# (5) Histogramas comparativos

hist(medias_n30, main = "Médias amostrais de renda (n = 30)",
     xlab = "Média da amostra", col = "lightblue", breaks = 30)
par(mar = c(4, 4, 2, 1)) 

hist(medias_n200, main = "Médias amostrais de renda (n = 200)",
     xlab = "Média da amostra", col = "lightgreen", breaks = 30)
par(mfrow = c(1, 1))

#Interpretação:
#A renda da população é desigual, mas quando tiramos várias amostras e calculamos a média, os resultados ficam cada vez mais parecidos com uma curva “normal”.
#Com 30 pessoas já dá para ver essa tendência, e com 200 fica ainda mais claro.
#quanto maior a amostra, mais as médias se organizam em torno de um padrão regular, mesmo que os dados originais sejam desiguais.


#EXERCÍCIO 7 - Intervalo de confiança para proporção (saúde)

set.seed(123)


N <- 5000
dados_saude <- data.frame(
  sexo       = sample(c("F", "M"), size = N, replace = TRUE, prob = c(0.55, 0.45)),
  fumante    = rbinom(N, 1, 0.22),
  hipertenso = rbinom(N, 1, 0.30)
)

# (1) Sorteio de amostra de tamanho n = 400
n <- 400
amostra_saude <- dados_saude[sample(1:nrow(dados_saude), n), ]


# (2) Proporcao amostral e Intervalo de confiança (IC)
p_hat <- mean(amostra_saude$hipertenso)
SE_p  <- sqrt(p_hat * (1 - p_hat) / n) 
IC_95 <- c(
  inferior = p_hat - 1.96 * SE_p,
  superior = p_hat + 1.96 * SE_p
)        

# (3) Proporcao verdadeira na população
p_verdadeiro <- mean(dados_saude$hipertenso)


# (4) Resultados e interpretação

print(p_hat)
print(p_verdadeiro)
print (IC_95)

#Na amostra de 400 pessoas, cerca de 30% tinham hipertensão. O intervalo calculado mostra que, na população, essa proporção deve estar entre aproximadamente 26% e 34%. O valor verdadeiro da base completa (30%) está dentro desse intervalo, o que confirma que a estimativa é confiável. Em termos práticos, isso significa que podemos ter segurança de que a taxa de hipertensão na população está nesse intervalo, informação útil para planejar políticas e recursos de saúde.


#EXERCÍCI0 8 - Correlação, regressão simples e inferência (educação)

set.seed(123)


# (1) Simular base de dados

N <- 2000 

dados_educacao <- data.frame(
  ideb        = rnorm(N, mean = 5.5, sd = 0.7),   
  gasto_aluno = rnorm(N, mean = 6000, sd = 1500)  
)
#IDEB:desempenho escolar

# Introduzir correlação positiva leve 
dados_educacao$ideb <- dados_educacao$ideb +
  0.0002 * (dados_educacao$gasto_aluno - 6000)

# (2) Correlação de Pearson
cor_ideb_gasto <- cor(dados_educacao$ideb, dados_educacao$gasto_aluno)
print(cor_ideb_gasto)


# (3) Regressão simples
modelo <- lm(ideb ~ gasto_aluno, data = dados_educacao)
summary(modelo) 
#a cada 1,00 real adicional do gasto, espero incrementar o ideb em 0.0001938;
#a cada 1.000,00 reais adicionais do gasto, espero incrementar o ideb em 0.1938.

confint(modelo) 
#o incremento pode ser entre 0.173 e 0.2146 por cada R$ 1.000,00 adicionais.

#Interpretação:
#O coeficiente angular é positivo, indicando que maiores gastos por aluno estão associados a IDEB ligeiramente maior. 
#O valor é pequeno, mostrando que o efeito é leve. 
#O valor-p é muito baixo, sugerindo que a relação não é fruto do acaso. 
#O intervalo de confiança não inclui zero, reforçando a evidência de associação. 
#Do ponto de vista inferencial, obter um coeficiente diferente de zero significa que há indícios de que o gasto influencia o IDEB, ainda que de forma modesta.
#portanto, há sinais de que investir mais por aluno está ligado a uma melhora no IDEB, mas o efeito é pequeno; ainda assim, é improvável que seja apenas coincidência.
#Para políticas públicas, isso significa que o aumento de recursos deve ser acompanhado de estratégias complementares para garantir avanços significativos na qualidade da educação.

#OPCIONAL:

ggplot(dados_educacao, aes(x = gasto_aluno, y = ideb)) +
  geom_point(alpha = 0.8, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "darkgreen") +
  labs(title = "Relação entre gasto por aluno e IDEB",
       x = "Gasto por aluno",
       y = "IDEB") +
  theme_minimal()


# Exercício 9 – Margem de erro e tamanho amostral (TSE)

set.seed(123)

# (1) População de municípios
N <- 5000
dados_tse <- data.frame(
  id_mun       = 1:N,
  prop_partido = rbeta(N, shape1 = 10, shape2 = 15)
)

# (2) Parâmetros da pesquisa
n_amostra <- 600
n_rep     <- 1000

# (3) Simulação de pesquisas
p_hat_vec <- numeric(n_rep)
for (r in 1:n_rep) {
  id_escolhido <- sample(dados_tse$id_mun, 1)
  p_mun <- dados_tse$prop_partido[id_escolhido]
  
  votos <- rbinom(n_amostra, size = 1, prob = p_mun)
  p_hat_vec[r] <- mean(votos)
}

# (4) Margem de erro "de praxe" (pior caso)
ME <- 1.96 * sqrt(0.25 / n_amostra)
print(paste("Margem de erro padrão:", round(ME*100,1), "%"))

# (5) Histograma das proporções simuladas
hist(p_hat_vec,
     breaks = 30,
     col = "lightblue",
     border = "white",
     main = "Distribuição das proporções estimadas",
     xlab = "Proporção de votos no partido X")

# (6) Estatísticas resumidas
print(paste("Média das proporções estimadas:", round(mean(p_hat_vec)*100,1), "%"))
print(paste("Desvio padrão das proporções estimadas:", round(sd(p_hat_vec)*100,1), "%"))


#Interpretação:
#A margem de erro é de cerca de 4%
#Na prática, a variabilidade observada nas simulações pode ser menor ou maior, dependendo da proporção verdadeira de votos em cada município.
#Isso mostra que a margem de erro divulgada em pesquisas eleitorais é uma aproximação, e não captura toda a incerteza ligada à escolha do município ou às diferenças locais.
#explicar que se trata de uma aproximação aumenta a confiança da população nas pesquisas e no processo democrático.



#EXERCíCIO 10: Regressão e distribuição amostral do coeficiente (TSE)

set.seed(123)


# (1) População de municípios
N <- 5000
dados_tse <- data.frame(
  id_mun = 1:N,
  renda_media = rnorm(N, mean = 2500, sd = 600)
)

# (2) Gerar proporção de votos dependente de renda_media
dados_tse$prop_partido <- plogis(
  -1 + 0.0006 * dados_tse$renda_media + rnorm(N, 0, 0.3)
)

# (3) Parâmetros da pesquisa
n_mun_amostra <- 300
n_eleitores   <- 400
n_rep         <- 500

# (4–6) Simulações
coef_angular <- numeric(n_rep)

for (r in 1:n_rep) {
  # Sortear municípios
  mun_sorteados <- sample(dados_tse$id_mun, n_mun_amostra)
  base_pesq <- dados_tse[dados_tse$id_mun %in% mun_sorteados, ]
  
  # Para cada município, simular votos e calcular proporção p_hat
  p_hat <- numeric(n_mun_amostra)
  for (i in seq_len(n_mun_amostra)) {
    p_true <- base_pesq$prop_partido[i]
    votos <- rbinom(n_eleitores, size = 1, prob = p_true)
    p_hat[i] <- mean(votos)
  }
  
  base_pesq$p_hat <- p_hat
  
  # Ajustar regressão simples
  modelo <- lm(p_hat ~ renda_media, data = base_pesq)
  coef_angular[r] <- coef(modelo)[2]
}

# (7) Histograma da distribuição dos coeficientes
hist(coef_angular,
     breaks = 30,
     col = "lightgreen",
     border = "white",
     main = "Distribuição amostral dos coeficientes angulares",
     xlab = "Coeficiente angular (p_hat ~ renda_media)")

#O gráfico dos coeficientes mostra que os resultados da regressão mudam quando a gente repete a pesquisa em diferentes municípios. Ou seja, o número não é sempre igual, ele varia.
#A média desses resultados fica perto do valor “real” do efeito da renda sobre o voto. Isso quer dizer que, em geral, o método está acertando.
#A variação dos resultados mostra o quanto eles podem oscilar de uma pesquisa para outra. Se essa variação é pequena, temos mais confiança no número.
#O intervalo de confiança é uma forma de mostrar essa variação: em vez de dar só um número, damos uma faixa de valores possíveis.
#Para políticas públicas, isso significa que é preciso considerar a incerteza e trabalhar com intervalos de confiança, não só com valores pontuais.


