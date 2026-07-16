# ==============================================================================
# SCRIPT ATUALIZADO: INCLUSÃO DO ÍNDICE DE TIGMOTAXIA (It)
# ==============================================================================

library(imager)   #pacote que faz o processamento da imagem
library(tidyverse)

arquivo <- file.choose()
im <- load.image(arquivo)

#Processo de limpeza e filtragem de imagem
df_pixels <- as.data.frame(im) %>%
  pivot_wider(names_from = cc, values_from = value) %>%
  rename(R = `1`, G = `2`, B = `3`) %>%
  mutate(
    mask = (R < 0.99) | (G < 0.99) | (B < 0.99), #o mask faz com q n tenha o pixel branco puro
    intensidade = 1 - (R + G + B) / 3
  ) #transforma a cor em um peso , Quanto mais escura ou forte for a cor no mapa de calor, maior será a intensidade (mais tempo o animal passou ali).



R <- R(im) #512
G <- G(im) #512
B <- B(im) #1
dim(im) #3

#vai mostrar a imagem preto e branco
plot(R)
plot(G)
plot(B)


#Medidas das cores
summary(R)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#0.5686  1.0000  1.0000  0.9993  1.0000  1.0000

summary(G)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#0.1882  1.0000  1.0000  0.9967  1.0000  1.0000

summary(B)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#0.0000  1.0000  1.0000  0.9513  1.0000  1.0000

#mostra a escala o fundo preto para identificar exatamente o que é quantificavel
mask <- (R < 0.99) | (G < 0.99) | (B < 0.99)
plot(mask)
sum(mask)
#[1] 28117

table(mask)
#mask
#FALSE   TRUE
#234027  28117

#retorna a imagem que foi enviada porém apenas o bruto, sem o fundo branco
layout(matrix(c(1,2),1,2))

plot(im)

plot(mask)
mask <- as.cimg(mask)

class(mask)

dim(mask)
#[1] 512 512   1   1

is.logical(mask)
#FALSE

df_ativos <- df_pixels %>% filter(mask == TRUE)

x_min <- min(df_ativos$x)
x_max <- max(df_ativos$x)
y_min <- min(df_ativos$y)
y_max <- max(df_ativos$y)

centro_x_real <- (x_min + x_max) / 2
centro_y_real <- (y_min + y_max) / 2
raio_real <- max((x_max - x_min) / 2, (y_max - y_min) / 2)

proporcao_centro <- 0.8
limite_borda_real <- raio_real * proporcao_centro

df_analise <- df_ativos %>%
  mutate(
    dist_centro = sqrt((x - centro_x_real)^2 + (y - centro_y_real)^2),
    zona = if_else(dist_centro >= limite_borda_real, "Borda", "Centro")
  )

# --- CÁLCULO DAS MÉTRICAS ---
metrica_borda_centro <- df_analise %>%
  count(zona) %>%
  mutate(percentual_ocupacao = (n / sum(n)) * 100)

metrica_ponderada <- df_analise %>%
  group_by(zona) %>%
  summarise(total_intensidade = sum(intensidade), .groups = "drop") %>%
  mutate(percentual_permanencia = (total_intensidade / sum(total_intensidade)) * 100)

# Extrair os valores de tempo para o cálculo do Índice de Tigmotaxia
t_borda <- metrica_ponderada %>% filter(zona == "Borda") %>% pull(percentual_permanencia)
t_centro <- metrica_ponderada %>% filter(zona == "Centro") %>% pull(percentual_permanencia)

# Garante que se alguma zona for zero, o código não quebre
if(length(t_borda) == 0) t_borda <- 0
if(length(t_centro) == 0) t_centro <- 0

indice_tigmotaxia <- (t_borda - t_centro) / (t_borda + t_centro)

# --- CÁLCULO DO GINI (Gb) ---
tamanho_bloco <- 8
df_grade <- df_analise %>%
  mutate(
    grade_x = floor((x - x_min) / tamanho_bloco) + 1,
    grade_y = floor((y - y_min) / tamanho_bloco) + 1
  ) %>%
  group_by(grade_x, grade_y) %>%
  summarise(intensidade_total = sum(intensidade), .groups = "drop")

calcular_gini <- function(v) {
  v <- sort(v)
  n <- length(v)
  if (sum(v) == 0) return(0)
  pari <- sum(seq_along(v) * v)
  return((2 * pari) / (n * sum(v)) - (n + 1) / n)
}

gini_comportamental <- calcular_gini(df_grade$intensidade_total)

# ==============================================================================
# IMPRESSÃO DOS RESULTADOS FINAIS
# ==============================================================================
cat("\n==================================================\n")
cat("          NOVOS ENDPOINTS COMPORTAMENTAIS         \n")
cat("==================================================\n\n")

cat("1. ÍNDICE DE TIGMOTAXIA (It):", round(indice_tigmotaxia, 4), "\n")
#1. ÍNDICE DE TIGMOTAXIA (It): 0.169

cat("(+1.0 = Tigmotaxia Pura | 0.0 = Neutro | -1.0 = Centrofilia Pura)\n\n")
#(+1.0 = Tigmotaxia Pura | 0.0 = Neutro | -1.0 = Centrofilia Pura)

cat("2. GINI COMPORTAMENTAL (Gb):", round(gini_comportamental, 4), "\n")
#2. GINI COMPORTAMENTAL (Gb): 0.4605

cat("(1.0 = Concentração Extrema/Hotspots | 0.0 = Exploração Uniforme)\n")
#(1.0 = Concentração Extrema/Hotspots | 0.0 = Exploração Uniforme)
