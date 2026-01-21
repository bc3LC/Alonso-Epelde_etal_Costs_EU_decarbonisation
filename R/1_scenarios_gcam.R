# Define language
Sys.setenv(LANG = "en")

#' split_if_long
#'
#' Function to split string in half at the nearest space
#' @param s target string
#' @param ref_s maximum number of characters allowed before splitting
#' @return target string split by half if length longer than ref_s
split_if_long <- function(s, ref_s = 15) {
  # Only split if longer than the reference string
  if (nchar(s) <= ref_s) {
    return(s)
  }
  
  # Find positions of all spaces
  spaces <- gregexpr(" ", s)[[1]]
  
  # If no spaces are found, return the string as is
  if (length(spaces) == 1 && spaces[1] == -1) {
    return(s)
  }
  
  # Find the space closest to the midpoint
  midpoint <- nchar(s) / 2
  best_space <- spaces[which.min(abs(spaces - midpoint))]
  
  # Replace only that specific space with a newline
  substr(s, best_space, best_space) <- "\n"
  
  return(s)
}



#' basic_graph_eu
#'
#' Function to create a basic graph to summarize the distributional impact in the EU
#' based in one or more socioeconomic or demographic variable (one plot per variable).
#' @param data a dataset with the input data needed to generate a basic graph.
#' @param var variable(s) according to which you want to generate the graph. If
#' graph_labels_eu$VARIABLE (by default) creates a graph with the distributional
#' impacts for each of the variables specified in the package. If not, you can
#' indicate a variable or a vector of variables to crate the graph.If you want to
#' see the variables for which the function is available run `available_var_impact_eu()`.
#' @importFrom dplyr %>%
#' @return a graph per selected variable/s summarizing distributional impacts.
basic_graph_eu <- function(data, var = graph_labels_eu$VARIABLE) {
  if (!dir.exists("figures")) dir.create("figures")

  # Si es una lista de dataframes (como en el output original de impact_eu)
  if (is.list(data) && !is.data.frame(data)) {
    for (g in var) {
      df <- data[[paste0("di_", g)]]

      if (is.null(df)) next

      source_tag <- unique(df$SOURCE)
      folder <- ifelse(length(source_tag) == 1 && source_tag %in% c("EU"), "EU", source_tag)

      folder_path <- file.path("figures", folder)
      if (!dir.exists(folder_path)) dir.create(folder_path)

      datapl <- df %>%
        tidyr::pivot_longer(cols = dplyr::starts_with("DI_"), names_to = "Scenario", values_to = "Impact") %>%
        dplyr::mutate(
          Scenario = stringr::str_replace(Scenario, "^DI_", ""),
          LABELS = as.character(LABELS)
        ) %>%
        dplyr::filter(!LABELS %in% c("Not provided", "NA", "Others", "Other", "Not applicable")) %>%
        dplyr::filter(!is.na(LABELS)) %>%
        droplevels()

      datapl$Scenario <- factor(datapl$Scenario,
                                levels = c("EU_FF55_FREE", "EU_FF55", "EU_NECP"))

      # Añadir la media de la UE si g == "country"
      if (g == "country") {
        eu_mean <- datapl %>%
          dplyr::group_by(Scenario) %>%
          dplyr::summarise(
            LABELS = "EU",
            Impact = weighted.mean(Impact, w = WEIGHT, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          dplyr::mutate(VARIABLE = g, SOURCE = "EU")

        datapl <- dplyr::bind_rows(datapl, eu_mean)
      }

      # NUEVO: si datapl está vacío, omitir
      if (nrow(datapl) == 0) {
        message(sprintf("Atención: Gráfico omitido para variable '%s' (sin datos tras filtrado).", g))
        next
      }

      # NUEVO: si datapl está vacío, omitir
      if (nrow(datapl) == 1) {
        message(sprintf("Atención: Gráfico omitido para variable '%s' (solo 1 valor).", g))
        next
      }

      # MODIFICACIÓN 1: Ordenar todos los países y EU juntos dentro de cada escenario
      if (g == "country") {
        datapl <- datapl %>%
          dplyr::group_by(Scenario) %>%
          dplyr::mutate(LABELS_ORDERED = forcats::fct_reorder(LABELS, Impact)) %>%
          dplyr::ungroup()

      } else if (g %in% c("decile", "decile_eu", "ventile", "ventile_eu", "percentile", "percentile_eu")) {
        datapl <- datapl %>%
          dplyr::mutate(LABELS = as.numeric(LABELS),
                        LABELS_ORDERED = LABELS)
      } else if (!g %in% c("children", "birth_country", "education", "contract_type",
                           "activity", "age", "employment_sector", "household_type", "income_source")) {
        # Solo aplicar reordenamiento si no es una variable con orden predefinido
        datapl <- datapl %>%
          dplyr::group_by(Scenario) %>%
          dplyr::mutate(LABELS_ORDERED = forcats::fct_reorder(LABELS, Impact)) %>%
          dplyr::ungroup()
      }

      if (g %in% c("decile", "decile_eu", "ventile", "ventile_eu", "percentile", "percentile_eu")) {
        datapl <- datapl %>% dplyr::mutate(LABELS = as.numeric(LABELS))
      }

      clean_g <- graph_labels_eu %>%
        dplyr::filter(VARIABLE == toupper(g)) %>%
        dplyr::pull(VAR_CLEAN)

      # Add full countries' names
      if (g == "country") {
        datapl$CountryName <- countrycode::countrycode(as.character(datapl$LABELS_ORDERED),
                                                       origin = "iso2c",
                                                       destination = "country.name",
                                                       warn = F)
        datapl <- datapl %>%
          dplyr::mutate(CountryName = dplyr::if_else(LABELS_ORDERED == 'EL', 'Greece',
                                                     dplyr::if_else(LABELS_ORDERED == 'EU', 'EU', CountryName))) %>%
          dplyr::select("LABELS","VARIABLE","WEIGHT","SOURCE","Scenario","Impact","LABELS_ORDERED"="CountryName")

        pl <- ggplot2::ggplot(datapl,
                              ggplot2::aes(y = tidytext::reorder_within(LABELS_ORDERED, Impact, Scenario),
                                           x = Impact, fill = Scenario)) +
          ggplot2::geom_col(position = ggplot2::position_dodge(width = 1)) +
          ggplot2::facet_wrap(~Scenario, scales = "free_y", ncol = 1) + 
          tidytext::scale_y_reordered() +
          # Destacar EU con color diferente si es país
          ggplot2::geom_col(data = datapl %>% filter(LABELS == "EU"),
                            fill = "darkblue", position = position_dodge(width = 1))
        
        # MODIFICACIÓN 2: Crear gráfico con ordenamiento independiente por facet
        pl <- pl +
          ggplot2::scale_fill_manual(values = c("#3ed8d8", "#7ee5b2", "#e5e57e", "#e5b27e", "#e57f7e", "#e78ae7", "#b98ae7")) +
          ggplot2::scale_x_continuous(labels = scales::label_percent(scale = 1)) +
          ggplot2::labs(x = "Change in welfare (%)", y = clean_g) +
          ggplot2::theme_classic(base_size = 12) +
          ggplot2::theme(axis.text = ggplot2::element_text(size = 11),
                         strip.text = ggplot2::element_text(size = 12),
                         axis.title = ggplot2::element_text(size = 12))
        

      } else {
        pl <- ggplot2::ggplot(datapl,
                              ggplot2::aes(x = LABELS_ORDERED,
                                           y = Impact, fill = Scenario)) +
          ggplot2::geom_col(position = ggplot2::position_dodge(width = 1)) +
          ggplot2::facet_wrap(~Scenario, scales = "free_x")

        # MODIFICACIÓN 2: Crear gráfico con ordenamiento independiente por facet
        pl <- pl +
          ggplot2::scale_fill_manual(values = c("#3ed8d8", "#7ee5b2", "#e5e57e", "#e5b27e", "#e57f7e", "#e78ae7", "#b98ae7")) +
          ggplot2::scale_y_continuous(labels = scales::label_percent(scale = 1)) +
          ggplot2::labs(y = "Change in welfare (%)", x = clean_g) +
          ggplot2::theme_classic(base_size = 12) +
          ggplot2::theme(axis.text = ggplot2::element_text(size = 11),
                         strip.text = ggplot2::element_text(size = 12),
                         axis.title = ggplot2::element_text(size = 12))
      }


      if (g %in% c("decile", "decile_eu")) {
        pl <- pl + ggplot2::scale_x_continuous(breaks = 1:10, labels = paste0("D", 1:10))
      }

      if (g %in% c("quintile", "quintile_eu")) {
        pl <- pl + ggplot2::scale_x_continuous(breaks = 1:5, labels = paste0("Q", 1:5))
      }

      if (g %in% c("ventile", "ventile_eu")) {
        pl <- pl + ggplot2::scale_x_continuous(breaks = seq(0, 100, by = 2))
      }

      if (g %in% c("percentile", "percentile_eu")) {
        pl <- pl + ggplot2::scale_x_continuous(breaks = seq(0, 100, by = 10))
      }

      if (g %in% c("zone", "household_type", "children", "income_source", "COUNTRYRP", "activity", "education")) {
        pl <- pl + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.25))
      }

      adj_wh <- adjust_wh(datapl, var_w = "Scenario", var_h = NULL)

      # Aumenta anchura si g == "country"
      if (g == "country") {
        adj_wh$width <- adj_wh$width + 60
        adj_wh$tmp <- adj_wh$width
        adj_wh$width <- adj_wh$heigth
        adj_wh$heigth <- adj_wh$tmp
      }

      ggplot2::ggsave(pl,
                      file = file.path(folder_path, paste0("DI_", g, ".png")),
                      width = adj_wh$width,
                      height = adj_wh$heigth,
                      units = "mm")
      return(pl)
    }
  } else {
    warning("The `data` object must be a list of data frames as returned by `impact_eu().")
    return(NULL)
  }
}

#' adjust_wh
#'
#' Function to adjust the width and height of a basic graph depending
#' on the number of scenarios and labels.
#' @param data a dataset used to create a basic graph.
#' @param var_w variable on which the width of the basic graph depends.
#' @param var_h variable on which the height of the basic graph depends.
#' @return a list containing the width and the height of the basic graph to be created.
adjust_wh <- function(data, var_w, var_h) {
  base_w <- 110
  a_w <- 90
  base_h <- 150
  a_h <- 160
  if(!is.null(var_w)) {
    n_elem <- length(unique(data[[var_w]]))
    final_w <- base_w + a_w * (n_elem -1)
  } else {
    final_w <- base_w
  }
  if(!is.null(var_h)) {
    n_elem <- length(unique(data[[var_h]]))
    final_h <- base_h + a_h * (n_elem -1)
  } else {
    final_h <- base_h
  }

  return(list(width = final_w, heigth = final_h))

}


#' order_var_eu
#'
#' Function to order the labels of the socioeconomic and demographic variables
#' @param data dataset in which we want to order the labels of the socioeconomic and demographic variables
#' @param g variable for which we want to sort the labels
#' @importFrom dplyr %>%
#' @return a dataset in which the labels are ordered for the selected socioeconomic or demographic variable
order_var_eu <- function(data, g){
  if (g == "children"){
    data <- data %>%
      dplyr::mutate(LABELS = factor(LABELS, levels = c("No children", "With children", "Large family")))
  } else if (g == "birth_country") {
    data <- data %>%
      dplyr::mutate(LABELS = factor(LABELS, levels = c("National", "EU", "Non-EU", "Non-national")))
  } else if (g == "education") {
    data <- data %>%
      dplyr::mutate(LABELS = factor(LABELS, levels = c("Early childhood education", "Primary education", "Secondary education", "Post-secondary education",  "Tertiary education", "Higher education")))
  } else if (g == "contract_type") {
    data <- data %>%
      dplyr::mutate(LABELS = factor(LABELS, levels = c("Permanent contract", "Fixed-term contract", "Not applicable")))
  } else if (g == "activity") {
    data <- data %>%
      dplyr::mutate(LABELS = factor(LABELS, levels = c("Employed", "Unemployed", "Retired", "Student", "Domestic tasks", "Disabled", "Military service")))
  } else if (g == "age") {
    data <- data %>%
      dplyr::mutate(LABELS = factor(LABELS, levels = c("Young", "Adult", "Elder")))
  } else if (g == "employment_sector") {
    data <- data %>%
      dplyr::mutate(LABELS = factor(LABELS, levels = c("Private sector", "Public sector")))
  } else if (g == "household_type") {
    data <- data %>%
      dplyr::mutate(LABELS = factor(LABELS, levels = c("Adult alone", "Couple", "Couple with children", "More than 2 adults", "More than 2 adults with children", "Single parent"  )))
  } else if (g == "income_source") {
    data <- data %>%
      dplyr::mutate(LABELS = factor(LABELS, levels = c( "Wages", "Self-employment", "Property", "Pensions", "Unemployment")))
  } else if (g == "feminization_degree") {
    data <- data %>%
      dplyr::mutate(LABELS = factor(LABELS, levels = c( "FD1", "FD2", "FD3", "FD4", "FD5")))
  }
  return(data)
}


#' adjust_wh_is
#'
#' Function to adjust the width and height of a intersectional graph
#' depending on the number of scenarios and labels.
#' @param data a dataset used to create an intersectional graph.
#' @param var_w variable on which the width of the intersectional graph depends.
#' @param var_h variable on which the height of the intersectional graph depends.
#' @return a list containing the width and the height of the intersectional graph to be created.
adjust_wh_is <- function(data, var_w, var_h) {
  base_w <- 90
  a_w <- 90
  base_h <- 150
  a_h <- 50
  if(!is.null(var_w)) {
    n_elem <- length(unique(data[[var_w]]))
    final_w <- base_w + a_w * (n_elem -1)
  } else {
    final_w <- base_w
  }
  if(!is.null(var_h)) {
    n_elem <- length(unique(data[[var_h]]))
    final_h <- base_h + a_h * (n_elem -1)
  } else {
    final_h <- base_h
  }

  return(list(width = final_w, heigth = final_h))

}


#' order_vars_eu
#'
#' Function to order the labels of the socioeconomic and demographic variables in intersectional graphs.
#' @param data dataset in which we want to order the labels of the socioeconomic and demographic variables
#' @param g variable for which we want to sort the labels
#' @importFrom dplyr %>%
#' @return a dataset in which the labels are ordered for the selected socioeconomic or demographic variable
order_vars_eu <- function(data, var_col, col_name = "LABELS") {
  levels_list <- list(
    children = c("No children", "With children", "Large family"),
    birth_country = c("National", "EU", "Non-EU", "Non-national"),
    education = c("Early childhood education", "Primary education", "Secondary education",
                  "Post-secondary education", "Tertiary education", "Higher education"),
    contract_type = c("Permanent contract", "Fixed-term contract", "Not applicable"),
    activity = c("Employed", "Unemployed", "Retired", "Student", "Domestic tasks", "Disabled", "Military service"),
    age = c("Young", "Adult", "Elder"),
    employment_sector = c("Private sector", "Public sector"),
    household_type = c("Adult alone", "Couple", "Couple with children", "More than 2 adults",
                       "More than 2 adults with children", "Single parent"),
    income_source = c("Wages", "Self-employment", "Property", "Pensions", "Unemployment")
  )

  if (var_col %in% names(levels_list)) {
    lvls <- levels_list[[var_col]]
    data[[col_name]] <- factor(data[[col_name]], levels = lvls)
  }

  return(data)
}


#' intersectional_graph_eu
#'
#' Function to create an intersectional graph to summarize the distributional
#' impact based in the intersection of two socioeconomic or demographic variables
#' (2 variables per plot) for EU countries.
#' @param data a dataset with the input data needed to generate the intersectional graph.
#' @param pairs set of variables (2) according to which you want to create the
#' intersectional graph. If is_categories (by default), it generates the intersectional
#' graph for each of the combinations of variables specified in the package. If not,
#' you can indicate the set of variables according to which you want to generste the
#' intersectional graph. If you wish to see the set of variables for which the
#' calculation is available, run `available_var_intersec_eu()`. To enter a set of
#' variables for the calculation, it must follow the same format as the output of
#' `available_var_intersec_eu()`, i.e. a table whose columns have category_a and
#' category_b as their titles.
#' @importFrom dplyr %>%
#' @return a graph per selected set of variables summarizing the distributional impacts.
intersectional_graph_eu <- function(data, pairs = is_categories_eu) {
  if (!dir.exists("figures")) dir.create("figures")

  for (r in 1:nrow(pairs)) {
    var_a <- pairs$category_a[r]
    var_b <- pairs$category_b[r]

    # Buscar todos los objetos que coincidan con este par de variables
    # matching_keys <- grep(paste0("^di_", var_a, "_", var_b), names(data), value = TRUE)
    matching_keys <- paste0("di_", var_a, "_", var_b)

    for (key in matching_keys) {
      df <- data[[key]]
      if (is.null(df)) next

      source_tag <- unique(df$SOURCE)
      folder <- ifelse(length(source_tag) == 1 && source_tag %in% c("EU"), "EU", source_tag)
      folder_path <- file.path("figures", folder)
      if (!dir.exists(folder_path)) dir.create(folder_path)

      datapl <- df %>%
        tidyr::pivot_longer(cols = dplyr::starts_with("DI_"), names_to = "Scenario", values_to = "Impact") %>%
        dplyr::mutate(
          Scenario = stringr::str_replace(Scenario, "^DI_", ""),
          LABELS_A = as.character(LABELS_A),
          LABELS_B = as.character(LABELS_B)
        )

      datapl$Scenario <- factor(datapl$Scenario,
                                levels = c("EU_FF55_FREE", "EU_FF55", "EU_NECP"))

      # Ordenar
      datapl <- order_vars_eu(datapl, var_a, "LABELS_A")
      datapl <- order_vars_eu(datapl, var_b, "LABELS_B")

      datapl <- datapl %>%
        dplyr::filter(
          !LABELS_A %in% c("Not provided", "Others", "Other", "Not applicable"),
          !LABELS_B %in% c("Not provided", "Others", "Other", "Not applicable")
        ) %>%
        dplyr::filter(!is.na(LABELS_A) & LABELS_A != "NA") %>%
        dplyr::filter(!is.na(LABELS_B) & LABELS_B != "NA") %>%
        droplevels()

      clean_a <- graph_labels_eu %>% dplyr::filter(VARIABLE == var_a) %>% dplyr::pull(VAR_CLEAN) %>% split_if_long()
      clean_b <- graph_labels_eu %>% dplyr::filter(VARIABLE == var_b) %>% dplyr::pull(VAR_CLEAN) %>% split_if_long()

      # Determinar si se usa faceta por país
      if ("by_country" %in% names(datapl)) {
        facet_formula <- as.formula("LABELS_B ~ Scenario + by_country")
      } else {
        facet_formula <- as.formula("LABELS_B ~ Scenario")
      }

      if (var_a %in% c("decile", "decile_eu", "quintile", "quintile_eu", "ventile", "ventile_eu", "percentile", "percentile_eu") & !var_b %in% c('gender','zone')) {
        pl <- ggplot2::ggplot(datapl, ggplot2::aes(x = LABELS_A, y = Impact, fill = Scenario)) +
          ggplot2::geom_col(position = ggplot2::position_dodge(width = 1)) +
          ggplot2::facet_grid(facet_formula) +
          ggplot2::scale_y_continuous(labels = scales::label_percent(scale = 1)) +
          ggplot2::scale_fill_manual(values = c("#3ed8d8", "#7ee5b2", "#e5e57e", "#e5b27e", "#e57f7e", "#e78ae7", "#b98ae7")) +
          ggplot2::labs(y = "Change in welfare (%)", x = clean_a) +
          ggplot2::theme_classic(base_size = 12)
      } else if (!(var_a == 'quintile' & var_b %in% c('gender','zone'))) {
        pl <- ggplot2::ggplot(datapl, ggplot2::aes(x = LABELS_A, y = Impact, fill = Scenario)) +
          ggplot2::geom_col(position = ggplot2::position_dodge(width = 1)) +
          ggplot2::facet_grid(facet_formula) +
          ggplot2::scale_y_continuous(labels = scales::label_percent(scale = 1)) +
          ggplot2::scale_fill_manual(values = c("#3ed8d8", "#7ee5b2", "#e5e57e", "#e5b27e", "#e57f7e", "#e78ae7", "#b98ae7")) +
          ggplot2::labs(y = "Change in welfare (%)", x = clean_a) +
          ggplot2::theme_classic(base_size = 12)
      } else {
        pl <- ggplot2::ggplot(datapl, ggplot2::aes(x = LABELS_A, y = Impact, 
                                                   fill = Scenario, pattern = LABELS_B)) +
          ggpattern::geom_col_pattern(
            position = ggplot2::position_dodge(width = 0.9),
            pattern_fill = "black",
            pattern_angle = 45,
            pattern_density = 0.1,
            color = "black",
          ) +
          ggplot2::facet_grid(~Scenario) +
          ggplot2::scale_y_continuous(labels = scales::label_percent(scale = 1)) +
          ggplot2::scale_fill_manual(values = c("#3ed8d8", "#7ee5b2", "#e5e57e", "#e5b27e", "#e57f7e", "#e78ae7", "#b98ae7")) +
          ggplot2::labs(y = "Change in welfare (%)", x = clean_a) +
          ggplot2::theme_classic(base_size = 12) +
          ggplot2::theme(panel.spacing = unit(2, "lines")) +
          ggpattern::scale_pattern_manual(values = c("circle", "crosshatch", "none")) +
          ggplot2::labs(pattern = clean_b) +
          ggplot2::guides(pattern = ggplot2::guide_legend(override.aes = list(fill = "white")))
      }

      if (var_a %in% c("decile", "decile_eu")) {
        pl <- pl + ggplot2::scale_x_continuous(breaks = 1:10, labels = paste0("D",1:10))
      }

      if (var_a %in% c("quintile", "quintile_eu")) {
        pl <- pl + ggplot2::scale_x_discrete(labels = paste0("Q",1:5))
      }

      if (var_a %in% c("country", "zone", "household_type", "children", "income_source", "COUNTRYRP", "activity", "education", "contract_type")) {
        pl <- pl + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.25))
      }

      adj_wh <- adjust_wh_is(datapl, var_w = "Scenario", var_h = "LABELS_B")

      if ("by_country" %in% names(datapl)) {
        npaises <- length(unique(datapl$by_country))
        adj_wh$width <- adj_wh$width + 20 * npaises
      } else if (var_a == "country") {
        adj_wh$width <- adj_wh$width + 50
      }

      # Validar dimensiones
      if (is.na(adj_wh$width) || is.na(adj_wh$heigth) ||
          adj_wh$width <= 0 || adj_wh$heigth <= 0 ||
          !is.numeric(adj_wh$width) || !is.numeric(adj_wh$heigth)) {
        message(sprintf("Warning: Skipping '%s' plot for country '%s': insufficient data after filtering",
                        key, folder))
        next
      }

      # Guardar gráfico con manejo de errores
      output_path <- file.path(folder_path, paste0(key, ".png"))
      message(sprintf("Saving '%s'  plot in folder '%s'", key, folder))

      ggplot2::ggsave(pl,
                      file = output_path,
                      width = adj_wh$width,
                      height = adj_wh$heigth,
                      units = "mm",
                      limitsize = FALSE)

      return(pl)
    }
  }

  return(invisible(NULL))
}


#' expenditure_pattern
#'
#' Function to create the expenditure by decile figure
#' @param data a dataset with the input data needed to generate graph.
#' @importFrom dplyr %>%
#' @return a graph
expenditure_pattern <- function(data) {

  colors <- c("#e5b27e", "#e57f7e", "#b98ae7")

  # Create graph
  pl <- ggplot(data, aes(x = EU_DECILE, y = share, color = coicop, group = coicop)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2.5) +
    scale_color_manual(values = colors) +
    scale_y_continuous(labels = scales::percent_format(scale = 1),
                       limits = c(0, 5.5),
                       breaks = seq(0, 5, by = 0.5)) +
    scale_x_continuous(breaks = 1:10,
                       labels = paste0("D", 1:10)) +
    labs(x = "Income decile",
         y = "Expenditure over total (%)",
         color = "Energy goods") +
    theme_classic(base_size = 12)

  return(pl)

}




#' fig_ms_map
#'
#' Function to create the MS maps' figure
#' @param data a dataset with the input data needed to generate graph.
#' @importFrom dplyr %>%
#' @return a graph
fig_ms_map <- function(data) {

  # map plot
  europe <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
    dplyr::select(adm0_a3, fips_10, iso_a2, geometry) %>%
    dplyr::mutate(iso_a2 = dplyr::if_else(iso_a2 == '-99', fips_10, iso_a2))

  europe_data <- europe %>%
    dplyr::left_join(data %>%
                       # fix Greece
                       dplyr::mutate(LABELS_ORDERED = dplyr::if_else(LABELS_ORDERED == 'EL', 'GR', LABELS_ORDERED)),
                     by = c("iso_a2" = "LABELS_ORDERED"),
                     relationship = "many-to-many")

  missing_ctry <- europe_data %>%
    dplyr::filter(is.na(scenario)) %>%
    dplyr::select(-scenario) %>%
    tidyr::crossing(scenario = unique(europe_data$scenario[!is.na(europe_data$scenario)]))
  missing_ctry_sf <- sf::st_sf(missing_ctry, sf_column_name = "geometry")
  missing_ctry_sf <- sf::st_transform(missing_ctry_sf, sf::st_crs(europe_data))
  missing_ctry_sf <- missing_ctry_sf %>%
    dplyr::select(names(europe_data))

  data_all <- rbind(
    europe_data %>%
      dplyr::filter(!is.na(scenario)),
    missing_ctry_sf
  ) %>% 
    dplyr::filter(scenario %in% unique(data$scenario))

  pl_maps <- ggplot2::ggplot(data_all) +
    geom_sf(fill = "grey80") +
    geom_sf(aes(fill = Impact)) +
    ggplot2::scale_fill_gradientn(
      colours = c("#6f00a6", "#de6e00", "#fae0cd"),
      na.value = "grey90",
      name = 'Change in\nwelfare (%)',
      labels = function(x) paste0(x, "%")
    ) +
    ggplot2::theme_minimal() +
    ggplot2::xlim(-10,40) + ggplot2::ylim(37,70) +
    ggplot2::facet_wrap(~scenario, ncol = 1) +
    ggplot2::theme_classic() +
    ggplot2::theme(text = ggplot2::element_text(size = 16),
                   axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank()
    )


  data_eu = data %>%
    dplyr::filter(LABELS == 'EU')

  # function to scale from old_min-old_max to new_min-new_max a given x
  scaled_x <- function(x, old_min = min(data$Impact), old_max = max(data$Impact),
                       new_min = -0.15, new_max = 0.45) {
    ((new_max - new_min) * (x - old_min) / (old_max - old_min) + new_min)/1.1
  }

  blank_p <- patchwork::plot_spacer() + theme_void()

  bar_plot <- cowplot::get_legend(pl_maps +
                                    ggplot2::theme(legend.text = ggplot2::element_text(size = 12),
                                                   legend.title = element_text(size = 13, margin = margin(b = 25))) +
                                    guides(fill = guide_colourbar(barwidth = grid::unit(7, "cm"),
                                                                  barheight = grid::unit(0.5, "cm"),
                                                                  direction = 'horizontal',
                                                                  reverse = T)))
  # function to create bullet - segment - label
  lab_tag <- function(lab, reverse = F) {
    if (reverse) {
      y0 = 0.1
      y1 = 0
    } else {
      y0 = 0
      y1 = 0.1
    }

    pl_tag <- ggplot() +
      geom_segment(aes(x = 0, xend = 0, y = y0, yend = y1),
                   linetype = "dotted") +
      geom_point(aes(x = 0, y = y0), size = 2.5, color = 'black') +
      geom_text(aes(x = 0, y = y1, label = lab), vjust = -0.5, size = 3.5) +
      theme_void() +
      coord_cartesian(clip = "off") +
      ylim(0,1)

    return(pl_tag)
  }


  pl_line <- ggdraw() +
    draw_plot(blank_p, x = 0, y = 0, width = 1, height = 1) +
    draw_plot(bar_plot, x = 0.1, y = 0.1, width = 0.8, height = 0.3) +
    # EU_FF55_FREE
    draw_plot(lab_tag('EU_FF55_FREE'),
              x = 0.2525 - scaled_x(data_eu %>%
                                    dplyr::filter(scenario == 'EU_FF55_FREE') %>%
                                    dplyr::pull(Impact)),
              y = 0.205, width = 1, height = 3) +
    # EU_FF55
    draw_plot(lab_tag('EU_FF55', reverse = T),
              x = 0.24 - scaled_x(data_eu %>%
                                    dplyr::filter(scenario == 'EU_FF55') %>%
                                    dplyr::pull(Impact)),
              y = -0.35, width = 1, height = 5) +
    # EU_NECP
    draw_plot(lab_tag('  EU_NECP'),
              x = 0.24 - scaled_x(data_eu %>%
                                    dplyr::filter(scenario == 'EU_NECP') %>%
                                    dplyr::pull(Impact)),
              y = 0.27, width = 1, height = 1.5)


  # join plots
  final_plot_v <- plot_grid(
    ggdraw() +
      draw_plot(pl_maps +
                  theme(legend.position = 'none'),
                x = 0, y = 0, height = 1),
    ggdraw() +
      draw_plot(pl_line,
                x = 0, y = 0.2, height = 1, width = 1),
    ncol = 1,
    rel_heights = c(1, 0.1)
  ) + theme(
    plot.background = element_rect(fill = "white", color = NA)
  )

  return(final_plot_v)

}



