#GEMINI

# ==============================================================================
# ANÁLISE ESPACIAL DE HEATMAPS (BORDA/CENTRO & GINI COMPORTAMENTAL)
# ==============================================================================

# Carregar pacotes
library(imager)
library(tidyverse)

# ==============================================================================
# SCRIPT CORRIGIDO: ANÁLISE ESPACIAL COM AUTO-CALIBRAÇÃO DE ARENA
# ==============================================================================

library(imager)
library(tidyverse)

# 1. Carregar a imagem
arquivo <- file.choose()
im <- load.image(arquivo)

# 2. Converter em Data Frame e criar a máscara
df_pixels <- as.data.frame(im) %>%
  pivot_wider(names_from = cc, values_from = value) %>%
  rename(R = `1`, G = `2`, B = `3`) %>%
  mutate(
    mask = (R < 0.99) | (G < 0.99) | (B < 0.99),
    intensidade = 1 - (R + G + B) / 3
  )

# 3. Filtrar apenas os pixels onde há atividade (rastro do heatmap)
df_ativos <- df_pixels %>% filter(mask == TRUE)

# --- PASSO CRÍTICO: AUTO-CALIBRAÇÃO GEOMÉTRICA ---
# Encontra os limites reais (mínimos e máximos) de onde há cor na imagem
x_min <- min(df_ativos$x)
x_max <- max(df_ativos$x)
y_min <- min(df_ativos$y)
y_max <- max(df_ativos$y)

# Calcula o centro real e o raio real da arena baseados nos dados da imagem
centro_x_real <- (x_min + x_max) / 2
centro_y_real <- (y_min + y_max) / 2
raio_real <- max((x_max - x_min) / 2, (y_max - y_min) / 2)

# Define a proporção da borda (0.8 significa que o centro ocupa 80% do raio, e a borda os 20% finais)
proporcao_centro <- 0.8
limite_borda_real <- raio_real * proporcao_centro
# --------------------------------------------------

# 4. Recalcular as distâncias usando a geometria real detectada
df_analise <- df_ativos %>%
  mutate(
    dist_centro = sqrt((x - centro_x_real)^2 + (y - centro_y_real)^2),
    # Classifica usando os limites corrigidos
    zona = if_else(dist_centro >= limite_borda_real, "Borda", "Centro")
  )

# 5. Gerar as métricas corrigidas
metrica_borda_centro <- df_analise %>%
  count(zona) %>%
  mutate(percentual_ocupacao = (n / sum(n)) * 100)

metrica_ponderada <- df_analise %>%
  group_by(zona) %>%
  summarise(total_intensidade = sum(intensidade), .groups = "drop") %>%
  mutate(percentual_permanencia = (total_intensidade / sum(total_intensidade)) * 100)

# ==============================================================================
# EXIBIÇÃO DOS RESULTADOS DIAGNÓSTICOS
# ==============================================================================

cat("\n=============================================\n")
cat("      DIAGNÓSTICO GEOMÉTRICO DA ARENA        \n")
cat("=============================================\n")
cat("Centro X detectado: ", centro_x_real, "\n")
cat("Centro Y detectado: ", centro_y_real, "\n")
cat("Raio útil da Arena (em pixels): ", raio_real, "\n")
cat("Sua borda começa a partir de: ", limite_borda_real, " pixels do centro\n\n")

cat("--- 1. Ocupação de Área Corrigida (Borda vs Centro) ---\n")
print(metrica_borda_centro)

cat("\n--- 2. Tempo de Permanência Ponderado Corrigido ---\n")
print(metrica_ponderada)

# 6. Gráfico de Validação Visual
ggplot(df_analise, aes(x = x, y = y, color = zona)) +
  geom_point(alpha = 0.5) +
  scale_y_reverse() + # Inverte o eixo Y para o padrão de imagem
  coord_fixed() +
  theme_minimal() +
  labs(title = "Gráfico de Validação (Agora deve mostrar a Borda correta)",
       color = "Zona Classificada")


# ==============================================================================
# SCRIPT DEFINITIVO: BORDA/CENTRO AUTO-CALIBRADO + GINI COMPORTAMENTAL (Gb)
# ==============================================================================



# 1. Carregar a imagem
arquivo <- file.choose()
im <- load.image(arquivo)

# 2. Processar pixels e criar máscara de atividade
df_pixels <- as.data.frame(im) %>%
  pivot_wider(names_from = cc, values_from = value) %>%
  rename(R = `1`, G = `2`, B = `3`) %>%
  mutate(
    mask = (R < 0.99) | (G < 0.99) | (B < 0.99),
    intensidade = 1 - (R + G + B) / 3
  )

# 3. Filtrar pixels ativos para Auto-Calibração
df_ativos <- df_pixels %>% filter(mask == TRUE)

x_min <- min(df_ativos$x)
x_max <- max(df_ativos$x)
y_min <- min(df_ativos$y)
y_max <- max(df_ativos$y)

# Definição geométrica real
centro_x_real <- (x_min + x_max) / 2
centro_y_real <- (y_min + y_max) / 2
raio_real <- max((x_max - x_min) / 2, (y_max - y_min) / 2)

# Divisão Borda/Centro (20% mais externos para a Borda)
proporcao_centro <- 0.8
limite_borda_real <- raio_real * proporcao_centro

# 4. Criar o dataset da Arena Realizada
df_analise <- df_ativos %>%
  mutate(
    dist_centro = sqrt((x - centro_x_real)^2 + (y - centro_y_real)^2),
    zona = if_else(dist_centro >= limite_borda_real, "Borda", "Centro")
  )

# ==============================================================================
# CALCULO DAS MÉTRICAS
# ==============================================================================

# Métrica 1: Ocupação Espacial
metrica_borda_centro <- df_analise %>%
  count(zona) %>%
  mutate(percentual_ocupacao = (n / sum(n)) * 100)

# Métrica 2: Permanência Ponderada (Tempo real estimado pelo calor)
metrica_ponderada <- df_analise %>%
  group_by(zona) %>%
  summarise(total_intensidade = sum(intensidade), .groups = "drop") %>%
  mutate(percentual_permanencia = (total_intensidade / sum(total_intensidade)) * 100)

# Métrica 3: Coeficiente de Gini Comportamental (Gb) adaptado à arena real
# Dividimos o raio real em blocos proporcionais para criar a grade
tamanho_bloco <- 8  # Ajustado para o tamanho real menor da sua arena

df_grade <- df_analise %>%
  mutate(
    grade_x = floor((x - x_min) / tamanho_bloco) + 1,
    grade_y = floor((y - y_min) / tamanho_bloco) + 1
  ) %>%
  group_by(grade_x, grade_y) %>%
  summarise(intensidade_total = sum(intensidade), .groups = "drop")

# Função matemática do Coeficiente de Gini
calcular_gini <- function(v) {
  v <- sort(v)
  n <- length(v)
  if (sum(v) == 0) return(0)
  pari <- sum(seq_along(v) * v)
  gini <- (2 * pari) / (n * sum(v)) - (n + 1) / n
  return(gini)
}

gini_comportamental <- calcular_gini(df_grade$intensidade_total)

# ==============================================================================
# IMPRESSÃO DOS RESULTADOS CONSOLIDADOS
# ==============================================================================

cat("\n==================================================\n")
cat("   RESULTADOS CONSOLIDADOS (GEOMETRIA CORRIGIDA)  \n")
cat("==================================================\n\n")

cat("--- 1. Tigmotaxia: Ocupação de Área ---\n")
print(metrica_borda_centro)

cat("\n--- 2. Tigmotaxia: Permanência Ponderada (Tempo) ---\n")
print(metrica_ponderada)

cat("\n--- 3. Heterogeneidade Espacial: Gini Comportamental (Gb) ---\n")
cat("Valor de Gb:", round(gini_comportamental, 4), "\n")
cat("(Próximo de 1 = Concentrado em hotspots/bordas | Próximo de 0 = Exploração uniforme)\n\n")

# Gráfico de validação
ggplot(df_analise, aes(x = x, y = y, color = zona)) +
  geom_point(alpha = 0.5) +
  scale_y_reverse() +
  coord_fixed() +
  theme_minimal() +
  labs(title = "Validação Visual da Arena Corrigida", color = "Zona")
