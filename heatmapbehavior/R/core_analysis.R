

#' Processar Heatmap e Extrair Geometria da Arena
#'
#' Esta funcao le a imagem do mapa de calor (PNG/JPEG), remove o fundo branco
#' e calcula as coordenadas do centro geometrico e raio util da arena.
#'
#' @param file_path Caminho para o arquivo de imagem do heatmap.
#' @param proportion_center Proporcao do raio para definir a zona central (padrao 0.8).
#' @return Uma lista com o dataframe de pixels ativos e metadados geometricos.
#' @export
process_heatmap <- function(file_path, proportion_center = 0.8) {

  # 1. Carregar imagem
  im <- imager::load.image(file_path)

  # 2. Converter para coordenadas e calcular intensidade (1 - branco)
  # Correção feita aqui: usando "cc" e "value" para evitar os avisos do tidyselect
  df_pixels <- as.data.frame(im) %>%
    tidyr::pivot_wider(names_from = "cc", values_from = "value") %>%
    dplyr::rename(R = `1`, G = `2`, B = `3`) %>%
    dplyr::mutate(
      mask = (R < 0.99) | (G < 0.99) | (B < 0.99),
      intensidade = 1 - (R + G + B) / 3
    )

  df_ativos <- df_pixels %>% dplyr::filter(.data$mask == TRUE)

  if (nrow(df_ativos) == 0) {
    stop("Nenhum rastro detectado. Verifique se a imagem contem cores (heatmap).")
  }

  # 3. Auto-calibracao dos limites da arena (independente do animal)
  x_min <- min(df_ativos$x); x_max <- max(df_ativos$x)
  y_min <- min(df_ativos$y); y_max <- max(df_ativos$y)

  centro_x_real <- (x_min + x_max) / 2
  centro_y_real <- (y_min + y_max) / 2
  raio_real <- max((x_max - x_min) / 2, (y_max - y_min) / 2)
  limite_borda_real <- raio_real * proportion_center

  # 4. Classificacao espacial basica
  df_analise <- df_ativos %>%
    dplyr::mutate(
      dist_centro = sqrt((x - centro_x_real)^2 + (y - centro_y_real)^2),
      zona = dplyr::if_else(.data$dist_centro >= limite_borda_real, "Borda", "Centro")
    )

  return(list(
    data = df_analise,
    geometry = list(
      centro_x = centro_x_real,
      centro_y = centro_y_real,
      raio = raio_real,
      limite_borda = limite_borda_real,
      limites_eixos = c(x_min, x_max, y_min, y_max)
    )
  ))
}

#' Calcular Metricas Comportamentais Universais
#'
#' Calcula o Indice de Tigmotaxia, Gini Comportamental, Distancia Media ao Centro
#' e o Centroide de Permanencia ponderados pela intensidade do mapa de calor.
#'
#' @param processed_heatmap Objeto retornado pela funcao `process_heatmap`.
#' @param block_size Tamanho do bloco em pixels para a grade do Gini (padrao 8).
#' @return Uma lista com as 4 metricas calculadas e tabelas resumo.
#' @export
calculate_metrics <- function(processed_heatmap, block_size = 8) {

  df_analise <- processed_heatmap$data
  geom <- processed_heatmap$geometry

  # --- METRICA 1: INDICE DE TIGMOTAXIA (It) ---
  permanencia <- df_analise %>%
    dplyr::group_by(.data$zona) %>%
    dplyr::summarise(total_intensidade = sum(.data$intensidade), .groups = "drop") %>%
    dplyr::mutate(percentual_permanencia = (total_intensidade / sum(total_intensidade)) * 100)

  t_borda <- permanencia %>% dplyr::filter(.data$zona == "Borda") %>% dplyr::pull(.data$percentual_permanencia)
  t_centro <- permanencia %>% dplyr::filter(.data$zona == "Centro") %>% dplyr::pull(.data$percentual_permanencia)

  if(length(t_borda) == 0) t_borda <- 0
  if(length(t_centro) == 0) t_centro <- 0

  indice_tigmotaxia <- (t_borda - t_centro) / (t_borda + t_centro)

  # --- METRICA 2: GINI COMPORTAMENTAL (Gb) ---
  x_min <- geom$limites_eixos[1]; y_min <- geom$limites_eixos[3]

  df_grade <- df_analise %>%
    dplyr::mutate(
      grade_x = floor((x - x_min) / block_size) + 1,
      grade_y = floor((y - y_min) / block_size) + 1
    ) %>%
    dplyr::group_by(.data$grade_x, .data$grade_y) %>%
    dplyr::summarise(intensidade_total = sum(.data$intensidade), .groups = "drop")

  calcular_gini <- function(v) {
    v <- sort(v); n <- length(v)
    if (sum(v) == 0) return(0)
    pari <- sum(seq_along(v) * v)
    return((2 * pari) / (n * sum(v)) - (n + 1) / n)
  }
  gini_comportamental <- calcular_gini(df_grade$intensidade_total)

  # --- METRICA 3: DISTANCIA MEDIA AO CENTRO ---
  distancia_media_centro <- sum(df_analise$dist_centro * df_analise$intensidade) / sum(df_analise$intensidade)

  # --- METRICA 4: CENTROIDE DE PERMANENCIA ---
  centroide_x <- sum(df_analise$x * df_analise$intensidade) / sum(df_analise$intensidade)
  centroide_y <- sum(df_analise$y * df_analise$intensidade) / sum(df_analise$intensidade)

  vies_espacial <- sqrt((centroide_x - geom$centro_x)^2 + (centroide_y - geom$centro_y)^2)

  return(list(
    indice_tigmotaxia = indice_tigmotaxia,
    gini_comportamental = gini_comportamental,
    distancia_media_centro = distancia_media_centro,
    centroide_permanencia = c(x = centroide_x, y = centroide_y),
    vies_espacial = vies_espacial,
    resumo_zonas = permanencia
  ))
}

#' Plotar Analise Espacial Completa
#'
#' Gera um grafico contendo o rastro do animal, a divisao da arena e o
#' Centroide de Permanencia (marcado como uma estrela vermelha).
#'
#' @param processed_heatmap Objeto retornado pela funcao `process_heatmap`.
#' @param metrics Objeto retornado pela funcao `calculate_metrics`.
#' @return Um grafico ggplot2.
#' @export
plot_behavior <- function(processed_heatmap, metrics) {
  df_analise <- processed_heatmap$data
  geom <- processed_heatmap$geometry
  cent <- metrics$centroide_permanencia

  ggplot2::ggplot() +
    # 1. Desenha os pontos ativos da arena
    ggplot2::geom_point(data = df_analise, ggplot2::aes(x = .data$x, y = .data$y, color = .data$zona), alpha = 0.4) +

    # 2. Marca o Centro Geométrico com uma cruz preta (+)
    ggplot2::geom_point(ggplot2::aes(x = geom$centro_x, y = geom$centro_y), color = "black", size = 4, shape = 3) +

    # 3. Marca o Centroide de Permanência com um losango vermelho (◆)
    ggplot2::geom_point(ggplot2::aes(x = cent["x"], y = cent["y"]), color = "red", size = 5, shape = 18) +

    # Ajustes de eixos e proporções
    ggplot2::scale_y_reverse() +
    ggplot2::coord_fixed() +

    # Correção feita aqui: Adicionando a escala de cores personalizada
    ggplot2::scale_color_manual(values = c("Borda" = "#eda547", "Centro" = "#6095a5")) +

    # Estilo e textos
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "heatmapbehavior: Analise Espacial do Campo Aberto",
      subtitle = "Cruz Preta (+) = Centro Geometrico | Diamante Vermelho (◆) = Centroide de Permanencia",
      color = "Zona",
      x = "Pixels (X)",
      y = "Pixels (Y)"
    )
}
