library(tidyverse)
library(writexl)
library (imager)
library (devtools)
library (ggplot2)

#IMPORTANTÍSSIMO
devtools::load_all()

source("R/core_analysis.R")

caminho_do_meu_heatmap <- "C:/Users/natal/OneDrive/Apackage"
# 1. Abre a janela para você clicar no arquivo .jpg ou .png do seu mapa de calor
arquivo <- file.choose()
im <- load.image(arquivo)
# 2. Agora sim, passa o caminho do arquivo selecionado para a função
#foi criado o objeto com o arena processada
process_heatmap <- function(file_path, proportion_center = 0.8)

arena_processada <- process_heatmap(file_path = arquivo)

# Calcule as métricas finais usando a arena que acabou de ser processada
meus_resultados <- calculate_metrics(processed_heatmap = arena_processada)

# Visualize os resultados na tela
print(meus_resultados)

names(arena_processada)
names(arena_processada$geometry)

library(ggplot2)


# Como o 'arena_processada' é o seu df_analise filtrado, podemos plotar direto

ggplot(arena_processada$data, aes(x = x, y = y, color = zona)) +
  geom_point(alpha = 0.5) +
  scale_y_reverse() +
  coord_fixed() +

  # Define as cores personalizadas para cada zona
  scale_color_manual(values = c("Borda" = "#eda547", "Centro" = "#6095a5")) +

  theme_minimal() +
  labs(
    title = "Validação Visual da Arena Corrigida",
    color = "Zona",
    x = "Pixels (X)",
    y = "Pixels (Y)"
  )
#salvando os resultados



# 1. Organiza os resultados em uma única linha
resumo <- meus_resultados$resumo_zonas
nova_linha <- tibble(
  Arquivo           = basename(caminho_do_meu_heatmap),
  It_Tigmotaxia     = meus_resultados$indice_tigmotaxia,
  Gb_Gini           = meus_resultados$gini_comportamental,
  Dist_Centro       = meus_resultados$distancia_media_centro,
  Centroide_X       = meus_resultados$centroide_permanencia["x"],
  Centroide_Y       = meus_resultados$centroide_permanencia["y"],
  Pct_Borda         = resumo$percentual_permanencia[resumo$zona == "Borda"],
  Pct_Centro        = resumo$percentual_permanencia[resumo$zona == "Centro"]
)

# 2. Define o nome do arquivo Excel
arq_excel <- "resultados_comportamento.xlsx"

# 3. Se o arquivo já existir, lê os dados antigos e junta com a nova linha.
# Se não existir, começa uma tabela nova.
if (file.exists(arq_excel)) {
  tabela_final <- bind_rows(readxl::read_excel(arq_excel), nova_linha)
} else {
  tabela_final <- nova_linha
}

# 4. Grava tudo no arquivo Excel
write_xlsx(tabela_final, arq_excel)

# Mostra na tela o que foi salvo
print(nova_linha)
#personalizando o gráfico
