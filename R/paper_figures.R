# load pkgs
library(tidyr)
library(dplyr)
library(magrittr)
library(ggplot2)
library(rgcam)
library(gcamdata)
library(countrycode)
library(tibble)
library(tidyverse)
library(here)
library(cowplot)
library(rnaturalearth)
library(rnaturalearthdata)


# load functions
source('R/1_scenarios_gcam.R')


# load data
d_impacts <- get(load('data/outputs_di/D_impacts.RData'))
d_impacts$di_zone = d_impacts$di_zone %>%
  dplyr::mutate(LABELS = dplyr::if_else(LABELS == 'Densely populated', 'Densely\npopulated',
                                        dplyr::if_else(LABELS == 'Intemediate','Intemediate\npopulated',
                                                       dplyr::if_else(LABELS == 'Sparsely populated','Sparsely\npopulated',
                                                                      LABELS))))

is_d_impacts <- get(load('data/outputs_dii/DII_impact.RData'))
is_d_impacts$di_quintile_zone = is_d_impacts$di_quintile_zone %>%
  dplyr::mutate(LABELS_B = dplyr::if_else(LABELS_B == 'Densely populated', 'Densely\npopulated',
                                        dplyr::if_else(LABELS_B == 'Intemediate','Intemediate\npopulated',
                                                       dplyr::if_else(LABELS_B == 'Sparsely populated','Sparsely\npopulated',
                                                                      LABELS_B))))

datapl_ctry <- get(load('data/datapl_ctry.RData')) # datapl obtained from running basic_graph_eu(d_impacts, var = 'country')
graph_labels_eu <- read.csv(file = 'data/graph_labels_eu.csv')
c_structure_eu <- xlsx::read.xlsx("data/c_structure_eu.xlsx", sheetIndex = 1) %>%
  tidyr::pivot_longer(cols = -EU_DECILE,
                      names_to = "Categoria",
                      values_to = "Porcentaje") %>%
  dplyr::rename(coicop = Categoria,
                 share = Porcentaje) %>%
  dplyr::mutate(coicop = str_replace_all(coicop, "\\.", " "),
                 share = share *100)
c_structure_national <- xlsx::read.xlsx("data/c_structure_national.xlsx", sheetIndex = 1) %>%
  tidyr::pivot_longer(cols = -EU_DECILE,
                      names_to = "Categoria",
                      values_to = "Porcentaje") %>%
  dplyr::rename(coicop = Categoria,
                 share = Porcentaje) %>%
  dplyr::mutate(coicop = str_replace_all(coicop, "\\.", " "),
                 share = share *100)

prj <- rgcam::loadProject("data/study11_fin.dat")
mypal = c(jgcricolors::jgcricol()$pal_all,"district heat"="goldenrod3")

pal_scen <- c(
  "EU_FF55_free" = "#3ed8d8",
  "EU_FF55" = "#7ee5b2",

  "EU_NECP_free" = "#ff7f00",
  "EU_NECP" = "#e5e57e",

  "EU_NOCLIMPOLICY"  = "#984ea3"
)


EU_COUNTRIES <- c("Austria", "Belgium", "Bulgaria", "Croatia",
                  "Cyprus", "Czech Republic", "Denmark",
                  "Estonia", "Finland", "France", "Germany",
                  "Greece", "Hungary", "Ireland", "Italy",
                  "Latvia", "Lithuania", "Luxembourg", "Malta",
                  "Netherlands", "Poland", "Portugal", "Romania",
                  "Slovakia", "Slovenia", "Spain", "Sweden")


folder_paper <- 'figures/paper'
if (!dir.exists("figures")) dir.create("figures")
if (!dir.exists("figures/paper")) dir.create("figures/paper")
if (!dir.exists("figures/extra")) dir.create("figures/extra")


# rename and reorder scenarios
rename_sce <- function(df){

  df <- df %>%
    mutate(scenario = if_else(scenario == "EU_FF55_LTT_FREE", "EU_FF55_free", scenario),
           scenario = if_else(scenario == "EU_FF55_LTT", "EU_FF55", scenario),
           scenario = if_else(scenario == "EU_NECP_LTT_FREE", "EU_NECP_free", scenario),
           scenario = if_else(scenario == "EU_NECP_LTT", "EU_NECP", scenario),
           scenario = if_else(scenario == "EU_NOPOLICY", "EU_NOCLIMPOLICY", scenario)) %>%
    # Filter final scenarios:
    filter(scenario %in% c("EU_NOCLIMPOLICY", "EU_FF55_free", "EU_FF55", "EU_NECP")) %>%
    mutate(scenario = factor(scenario, levels = c('EU_FF55_free','EU_FF55','EU_NECP','EU_NOCLIMPOLICY')))

  invisible(df)

}


########################
# figure 3 - MS
fig_v_welfare_ms <- basic_graph_eu(d_impacts, var = 'country') +
  theme(legend.position = 'bottom')
  

ggplot2::ggsave(fig_v_welfare_ms,
                file = file.path(folder_paper, "final_plot_MS_bar.png"),
                width = 150,
                height = 330,
                units = "mm")

# maps
fig_v_welfare_ms_map <- fig_ms_map(datapl_ctry)

ggplot2::ggsave(fig_v_welfare_ms_map,
                file = file.path(folder_paper, "final_plot_MS_map.png"),
                width = 150,
                height = 330,
                units = "mm")


# join

fig_v_welfare_ms_map_cropped <- fig_v_welfare_ms_map + 
  theme(plot.margin = margin(t = 0, r = -6, b = 0, l = -8, unit = "cm"))

final_plot_ms <- 
  cowplot::plot_grid(fig_v_welfare_ms,
                     fig_v_welfare_ms_map_cropped,
                     ncol = 2, rel_widths = c(1.5, 1),
                     labels = c("a)", "b)")
) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )


ggplot2::ggsave(final_plot_ms,
                file = file.path(folder_paper, "final_plot_MS.png"),
                width = 330,
                height = 300,
                units = "mm")


########################
# figure 4 - deciles EU vs National level
fig_v_welfare_d_eu <- basic_graph_eu(d_impacts, var = 'decile_eu')
fig_v_welfare_d_national <- basic_graph_eu(d_impacts, var = 'decile')
legend <- cowplot::get_legend(fig_v_welfare_d_eu +
                                ggplot2::theme(legend.direction = "horizontal",
                                               legend.text = ggplot2::element_text(size = 12)))


final_plot_v <- cowplot::plot_grid(
  # figure
  cowplot::plot_grid(fig_v_welfare_d_eu +
                       labs(x = 'Income decile (EU level)') +
                       theme(legend.position = 'none',
                             axis.text.x = ggplot2::element_text(size = 11)),
                     fig_v_welfare_d_national +
                       labs(x = 'Income decile (National level)') +
                       theme(legend.position = 'none'),
                     ncol = 1, rel_heights = c(1, 1),
                     labels = c("a)", "b)")),
  # legend
  legend,
  # options
  ncol = 1,
  rel_heights = c(1, 0.05)
) +
theme(
  plot.background = element_rect(fill = "white", color = NA),
  panel.background = element_rect(fill = "white", color = NA)
)


ggplot2::ggsave(final_plot_v,
                file = file.path(folder_paper, "final_plot_deciles_eu_national.png"),
                width = 330,
                height = 300,
                units = "mm")


########################
# figure SM - deciles by ctry

# function to read each file and extract $decile
read_decile <- function(f) {
  obj <- get(load(f))
  dec <- obj$di_decile

  df <- as.data.frame(dec)
  df
}

files <- list.files('data/outputs_di', pattern = "^D_impacts_", full.names = TRUE)
ctry_data <- do.call(rbind, lapply(files, read_decile)) %>%
  # fix mismatch with iso2 codes
  dplyr::mutate(CountryName = countrycode::countrycode(as.character(SOURCE),origin = "iso2c",
                                                       destination = "country.name",warn = F)) %>%
  dplyr::mutate(CountryName = dplyr::if_else(SOURCE == 'EL', 'Greece',
                                             dplyr::if_else(SOURCE == 'EU', 'EU', CountryName))) %>%
  # fix scenario names
  tidyr::pivot_longer(cols = dplyr::starts_with("DI_"), names_to = "Scenario", values_to = "Impact") %>%
  dplyr::mutate(
    Scenario = stringr::str_replace(Scenario, "^DI_", ""),
    LABELS = as.character(LABELS)
  ) %>%
  dplyr::filter(!LABELS %in% c("Not provided", "NA", "Others", "Other", "Not applicable")) %>%
  dplyr::filter(!is.na(LABELS)) %>%
  droplevels() %>%
  # clean dataset
  dplyr::select("LABELS","VARIABLE","WEIGHT","Scenario","Impact","CountryName") %>%
  # order names
  dplyr::mutate(Scenario = factor(Scenario, levels = c("EU_FF55_free", "EU_FF55", "EU_NECP")),
                CountryName = factor(CountryName, levels = sort(unique(CountryName))),
                LABELS = factor(LABELS, levels = 1:10),
                LABELS = as.numeric(LABELS))

final_plot_MS_si <- ggplot2::ggplot(ctry_data,
                                    ggplot2::aes(x = LABELS, y = Impact, fill = Scenario)) +
  ggplot2::geom_col(position = ggplot2::position_dodge(width = 1)) +
  ggplot2::facet_grid(CountryName~Scenario, scales = 'free') +
  ggplot2::scale_fill_manual(values = pal_scen) +
  ggplot2::scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  ggplot2::scale_x_continuous(breaks = 1:10, labels = paste0("D", 1:10)) +
  ggplot2::labs(y = "Change in welfare (%)", x = 'Income decile (National level)') +
  ggplot2::theme_classic(base_size = 12) +
  ggplot2::theme(axis.text = ggplot2::element_text(size = 11),
                 strip.text = ggplot2::element_text(size = 12),
                 axis.title = ggplot2::element_text(size = 12),
                 legend.position = 'bottom',
                 strip.text.y = ggplot2::element_text(angle = 0))


ggplot2::ggsave(final_plot_MS_si,
                file = file.path(folder_paper, "final_plot_MS_si_free.png"),
                width = 420,
                height = 594,
                units = "mm")


########################
# figure SM - expenditures
fig_expenditure_eu <- expenditure_pattern(c_structure_eu)
fig_expenditure_national <- expenditure_pattern(c_structure_national)
legend <- cowplot::get_legend(fig_expenditure_national +
                                ggplot2::theme(legend.direction = "horizontal",
                                               legend.text = ggplot2::element_text(size = 12)))

final_plot_expenditure <- cowplot::plot_grid(
  # figure
  cowplot::plot_grid(fig_expenditure_eu +
                       labs(x = 'Income decile (EU level)') +
                       theme(legend.position = 'none',
                             axis.text.x = ggplot2::element_text(size = 11)),
                     fig_expenditure_national +
                       labs(x = 'Income decile (National level)') +
                       theme(legend.position = 'none'),
                     ncol = 1, rel_heights = c(1, 1),
                     labels = c("a)", "b)")),
  # legend
  legend,
  # options
  ncol = 1,
  rel_heights = c(1, 0.05)
) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

ggplot2::ggsave(final_plot_expenditure,
                file = file.path(folder_paper, "final_plot_expenditure.png"),
                width = 330,
                height = 300,
                units = "mm")


########################
# figure gender
fig_h_welfare_gender <- basic_graph_eu(d_impacts, var = 'gender')
fig_h_gender_quintile <- intersectional_graph_eu(is_d_impacts,
                                                  pairs = data.frame(category_a = 'quintile',category_b = 'gender'))
legend <- cowplot::get_legend(fig_h_welfare_gender +
                                ggplot2::theme(legend.direction = "horizontal",
                                               legend.text = ggplot2::element_text(size = 12)))


final_plot_v <- cowplot::plot_grid(
  # figure
  cowplot::plot_grid(fig_h_welfare_gender +
                       labs(x = 'Gender of the reference person') +
                       theme(legend.position = 'none'),
                     fig_h_gender_quintile +
                       labs(x = 'Income decile (National level)') +
                       theme(legend.position = 'none'),
                     ncol = 1, rel_heights = c(1, 1),
                     labels = c("a)", "b)")),
  # legend
  legend,
  # options
  ncol = 1,
  rel_heights = c(1, 0.05)
) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )


ggplot2::ggsave(final_plot_v,
                file = file.path(folder_paper, "final_plot_gender.png"),
                width = 330,
                height = 300,
                units = "mm")



########################
# figure zone
fig_h_welfare_zone <- basic_graph_eu(d_impacts, var = 'zone') +
  theme(axis.text.x = element_text(angle = 0, hjust = .5))
fig_h_zone_quintile <- intersectional_graph_eu(is_d_impacts,
                                                  pairs = data.frame(category_a = 'quintile',category_b = 'zone'))
legend <- cowplot::get_legend(fig_h_welfare_zone +
                                ggplot2::theme(legend.direction = "horizontal",
                                               legend.text = ggplot2::element_text(size = 12)))


final_plot_v <- cowplot::plot_grid(
  # figure
  cowplot::plot_grid(fig_h_welfare_zone +
                       theme(legend.position = 'none'),
                     fig_h_zone_quintile +
                       labs(x = 'Income decile (National level)') +
                       theme(legend.position = 'none'),
                     ncol = 1, rel_heights = c(1, 1),
                     labels = c("a)", "b)")),
  # legend
  legend,
  # options
  ncol = 1,
  rel_heights = c(1, 0.05)
) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )


ggplot2::ggsave(final_plot_v,
                file = file.path(folder_paper, "final_plot_zone.png"),
                width = 330,
                height = 300,
                units = "mm")


########################
# figure no-intersectional
fig_h_welfare_gender <- basic_graph_eu(d_impacts, var = 'gender')
fig_h_welfare_zone <- basic_graph_eu(d_impacts, var = 'zone') +
  theme(axis.text.x = element_text(angle = 0, hjust = .5))
legend <- cowplot::get_legend(fig_h_welfare_gender +
                                ggplot2::theme(legend.direction = "horizontal",
                                               legend.text = ggplot2::element_text(size = 12)))


final_plot_nointersectional <- cowplot::plot_grid(
  # figure
  cowplot::plot_grid(fig_h_welfare_gender +
                       theme(legend.position = 'none'),
                     fig_h_welfare_zone +
                       theme(legend.position = 'none'),
                     ncol = 1, rel_heights = c(1, 1),
                     labels = c("a)", "b)")),
  # legend
  legend,
  # options
  ncol = 1,
  rel_heights = c(1, 0.05)
) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )


ggplot2::ggsave(final_plot_nointersectional,
                file = file.path(folder_paper, "final_plot_nointersectional.png"),
                width = 330,
                height = 300,
                units = "mm")


########################
# figure intersectional
fig_h_gender_quintile <- intersectional_graph_eu(is_d_impacts,
                                                 pairs = data.frame(category_a = 'quintile',category_b = 'gender'))
  
fig_h_zone_quintile <- intersectional_graph_eu(is_d_impacts,
                                               pairs = data.frame(category_a = 'quintile',category_b = 'zone'))
legend1 <- cowplot::get_legend(fig_h_zone_quintile +
                                ggplot2::guides(pattern = 'none', fill = guide_legend(override.aes = list(pattern = "none"))) +
                                ggplot2::theme(legend.direction = "horizontal",
                                               legend.text = ggplot2::element_text(size = 12)))
legend2 <- cowplot::get_legend(fig_h_gender_quintile +
                                ggplot2::guides(fill = 'none') +
                                ggplot2::theme(legend.direction = "horizontal",
                                               legend.text = ggplot2::element_text(size = 12)) +
                                 ggplot2::guides(pattern = ggplot2::guide_legend(override.aes = list(fill = "white"))))
legend3 <- cowplot::get_legend(fig_h_zone_quintile +
                                ggplot2::guides(fill = 'none', pattern = ggplot2::guide_legend(keywidth = unit(5, "mm"),
                                                                                               keyheight = unit(5, "mm"),
                                                                                               nrow = 1)) +
                                ggplot2::theme(legend.direction = "horizontal",
                                               legend.text = ggplot2::element_text(size = 12),
                                               legend.key.height = unit(5, "mm"), 
                                               legend.key.width = unit(5, "mm")) +
                                 ggplot2::guides(pattern = ggplot2::guide_legend(override.aes = list(fill = "white"))))
blank_p <- patchwork::plot_spacer() + theme_void()


final_plot_intersectional <- cowplot::plot_grid(
  # figure
  cowplot::plot_grid(fig_h_gender_quintile +
                       theme(legend.position = 'none'),
                     fig_h_zone_quintile +
                       theme(legend.position = 'none'),
                     ncol = 1, rel_heights = c(1, 1),
                     labels = c("a)", "b)")),
  # legend
  cowplot::plot_grid(legend1,legend2,legend3,
                     ncol = 3, rel_widths = c(1, 0.75, 1)),
  # options
  ncol = 1,
  rel_heights = c(1, 0.05)
) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )


ggplot2::ggsave(final_plot_intersectional,
                file = file.path(folder_paper, "final_plot_intersectional.png"),
                width = 330,
                height = 300,
                units = "mm")


########################
# co2 emissions

# CO2 emission trajectories
co2 <- rgcam::getQuery(prj, "CO2 emissions by region" ) %>%
  dplyr::filter(region %in% EU_COUNTRIES,
                year <= 2030) %>%
  # transform to MtCO2
  dplyr::mutate(value = value  * 3.666667) %>%
  # Select and rename scenarios
  rename_sce()

# Filter some countries for visualization
sel_co2_ctry <- c("France", "Germany",
                  "Greece", "Italy",
                  "Spain", "Sweden",
                  "Poland", "Lithuania",
                  "Netherlands")

co2_ctry <- co2 %>%
  dplyr::filter(region %in% sel_co2_ctry)

# Add EU emissions
co2_eu <- co2 %>%
  group_by(scenario, year, Units) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  mutate(region = "EU")


# Plot for the EU as a whole
pl_co2_eu <- ggplot(co2_eu %>%
                      filter(year >= 2015), aes(x = year, y = value, color = scenario)) +
  geom_line(linewidth = 1.5) +
  # historical emissions
  geom_line(data = co2_eu %>% filter(year >= 2005, year <= 2015),
            color = "black", linewidth = 1.2, linetype = "solid") +
  geom_vline(xintercept = 2015, linetype = "dashed", color = "gray40") +
  scale_color_manual(values = pal_scen, name = 'Scenario') +
  theme_classic() +
  theme(legend.position = 'bottom') +
  labs(
    title = "EU CO2 Emissions",
    #subtitle = "Emissions from 2005 onward with scenario projections from 2015",
    x = "",
    y = expression("MtCO"[2]),
    #caption = "Note: Black line shows historical emissions. Source: [Your Source Here]"
  ) +
  ggplot2::theme(text = ggplot2::element_text(size = 16))

ggsave("figures/extra/co2_emissions_eu.tiff", plot = last_plot(),
       width = 12, height = 8, dpi = 300,
       device = "tiff", compression = "lzw")

# Plot for the selected individual countries
pl_co2_ctry <- ggplot(co2_ctry %>%
                        filter(year >= 2015),
                      aes(x = year, y = value, color = scenario)) +
  geom_line(linewidth = 1.2) +
  # historical emissions
  geom_line(data = co2_ctry %>% filter(year >= 2005, year <= 2015),
            aes(x = year, y = value, group = region),
            color = "black", linewidth = 1.1, linetype = "solid", inherit.aes = FALSE) +
  geom_vline(xintercept = 2015, linetype = "dashed", color = "gray40") +
  facet_wrap(~region, scales = "free_y") +
  scale_color_manual(values = pal_scen, name = 'Scenario') +
  labs(
    title = "National CO2 Emissions",
    #subtitle = "Emissions from 2005 onward with scenario projections from 2015",
    x = "",
    y = expression("MtCO"[2]),
    # caption = "Note: Black line shows historical emissions."
  ) +
  theme_classic() +
  theme(axis.title = element_text(size = 13),
        strip.text = element_text(size = 12))

ggsave("figures/extra/co2_emissions_ctry.tiff", plot = last_plot(),
       width = 12, height = 8, dpi = 300,
       device = "tiff", compression = "lzw")



# join plots
legend <- cowplot::get_legend(pl_co2_ctry +
                                ggplot2::theme(legend.direction = "horizontal",
                                               legend.text = ggplot2::element_text(size = 12)))


final_plot_co2 <- cowplot::plot_grid(
  # figure
  cowplot::plot_grid(pl_co2_eu +
                       labs(title = '') +
                       theme(legend.position = 'none'),
                     pl_co2_ctry +
                       labs(title = '') +
                       theme(legend.position = 'none'),
                     ncol = 1, rel_heights = c(0.8, 1),
                     labels = c("a)", "b)")),
  # legend
  legend,
  # options
  ncol = 1,
  rel_heights = c(1, 0.05)
) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )


ggplot2::ggsave(final_plot_co2,
                file = file.path(folder_paper, "final_plot_co2.png"),
                width = 330,
                height = 300,
                units = "mm")



########################
# price changes
prices <- read.csv("data/price_changes_fig.csv") %>%
  select(scenario, region, eurostat_code, sector, year, price_diff) %>%
  replace_na(list(price_diff = 0)) %>%
  mutate(region_sector = paste(region, sector, sep = " - ")) %>%
  distinct() %>%
  # Filter some regions/sectors fo visulization
  filter(
    region %in% c("Germany", "France", "Spain", "Italy", "Greece", "Romania"),
    sector %in% c("elect_td_bld", "refined liquids transport", "delivered gas")
  ) %>%
  dplyr::mutate(sector = if_else(sector == "elect_td_bld", "Electricity", sector),
                sector = if_else(sector == "refined liquids transport", "Transport fuels", sector),
                sector = if_else(sector == "delivered gas", "Gas", sector)) %>%
  rename_sce()

ggplot(prices, aes(
  x = scenario,
  y = price_diff,
  fill = scenario
)) +
  geom_bar(stat = "identity", width = 0.7, color = "black", linewidth = 0.2) +  # add black outline for clarity
  facet_grid(sector ~ region) +  # move countries to columns
  scale_fill_manual(values = pal_scen, name = 'Scenario') +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_hline(yintercept = 1, linetype = "dotted", color = "black", linewidth = 0.8) +
  theme_classic(base_size = 12) +
  theme(
    axis.text.x = element_blank(),
    strip.text = element_text(face = "bold", size = 12),
    strip.background = element_rect(fill = "grey90", color = NA),
    legend.position = "bottom",
    panel.spacing = unit(1, "lines"),
    strip.placement = "outside"
  ) +
  labs(
    x = "",
    y = "Price Change (Relative to 1)"
  )

ggsave(
  filename = file.path(folder_paper, "final_plot_price_changes_2030.tiff"),
  plot = last_plot(),
  device = "tiff",
  width = 12,       # adjust as needed
  height = 8,       # adjust as needed
  units = "in",
  dpi = 100         # high resolution for print
)


# Other prices:
other_cat <- c( "regional corn",  "regional wheat", "regional beef", "regional pork",
                "district heat", "N fertilizer" ,
                "Bus", "HSR", "Passenger Rail", "Domestic Aviation", "trn_aviation_intl")

prices_other <- read.csv("data/price_changes_fig.csv") %>%
  select(scenario, region, eurostat_code, sector, year, price_diff) %>%
  replace_na(list(price_diff = 0)) %>%
  mutate(region_sector = paste(region, sector, sep = " - ")) %>%
  distinct() %>%
  # Filter some regions/sectors fo visulization
  filter(
    region %in% c("Germany", "France", "Spain", "Italy", "Greece", "Romania"),
    sector %in% other_cat
  ) %>%
  dplyr::mutate(sector = gsub("regional ", "", sector),
                sector = if_else(sector == "trn_aviation_intl", "Int aviation", sector),
                sector = if_else(sector == "Passenger Rail", "Pass Rail", sector),
                sector = if_else(sector == "Domestic Aviation", "Dom Aviation", sector),
                sector = if_else(sector != 'HSR', stringr::str_to_sentence(sector), sector)) %>%
  group_by(scenario, region, eurostat_code, sector, year, region_sector) %>%
  summarise(price_diff = mean(price_diff)) %>%
  ungroup() %>%
  mutate(sector = stringr::str_wrap(sector, width = 10)) %>%
  rename_sce()


ggplot(prices_other, aes(
  x = scenario,
  y = price_diff,
  fill = scenario
)) +
  geom_bar(stat = "identity", width = 0.7, color = "black", linewidth = 0.2) +  # add black outline for clarity
  facet_grid(region ~ sector) +  # move countries to columns
  scale_fill_manual(values = pal_scen, name = 'Scenario') +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_hline(yintercept = 1, linetype = "dotted", color = "black", linewidth = 0.8) +
  theme_classic(base_size = 12) +
  theme(
    axis.text.x = element_blank(),
    strip.text = element_text(face = "bold", size = 12),
    strip.background = element_rect(fill = "grey90", color = NA),
    legend.position = "bottom",
    panel.spacing = unit(1, "lines"),
    strip.placement = "outside"
  ) +
  labs(
    x = "",
    y = "Price Change (Relative to 1)"
  )

ggsave(
  filename = "figures/paper/final_plot_price_changes_others_2030.tiff",
  plot = last_plot(),
  device = "tiff",
  width = 12,       # adjust as needed
  height = 8,       # adjust as needed
  units = "in",
  dpi = 100         # high resolution for print
)

########################
# final energy
fe <- rgcam::getQuery(prj, "final energy consumption by fuel") %>%
  rename_sce() %>%
  dplyr::filter(input != "refined liquids bunkers",
                input != "refined liquids bunkers av") %>%
  dplyr::filter(region %in% EU_COUNTRIES) %>%
  dplyr::mutate(
    input = if_else(grepl("refined liquids", input), "refined liquids", input),
    input = if_else(grepl("biomass", input), "biomass", input),
    input = if_else(grepl("gas", input), "gas", input),
    input = if_else(grepl("coal", input), "coal", input),
  )  %>%
  rename(fuel = input) %>%
  filter(fuel != "solar",
         fuel != "process heat dac") %>%
  mutate(region = factor(region, levels = sort(unique(region)))) %>%
  rename_sce()

fe_2030 <- fe %>%
  filter(year == 2030)

# Figure for EU_2030
fe_2030_eu <- fe_2030 %>%
  group_by(scenario, year, fuel, Units) %>%
  summarise(value = sum(value)) %>%
  ungroup()


plot_fe_2030_eu <- ggplot(fe_2030_eu, aes(x = scenario,
                                          y = value,
                                          fill = fuel,
                                          alpha = scenario)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  labs(
    x = "",
    y = "Final Energy Consumption (EJ)",
    fill = "Fuel"
  ) +
  scale_fill_manual(values = mypal[names(mypal) %in% unique(fe_2030$fuel)],
                    label = function(x) stringr::str_to_sentence(x)) +
  scale_alpha_manual(values = c(1,1,1,0.5)) +
  theme_classic(base_size = 13) +
  theme(
    legend.direction = "horizontal",
    legend.key.size = unit(1, "lines"),
    axis.text = element_text(hjust = .5, size = 11),
    panel.grid.major.y = element_line(color = "grey90"),
    panel.spacing = unit(1.2, "lines")
  ) +
  guides(alpha = "none")

ggsave(
  plot_fe_2030_eu +
    theme(legend.position = 'bottom'),
  filename = "figures/extra/TFE_2030_eu.tiff",
  width = 12, height = 8, units = "in",
  dpi = 300,
  compression = "lzw",
  bg = "white"  # Prevents transparent background
)

pl_fe_2030 <- ggplot(fe_2030, aes(x = scenario,
                                  y = value,
                                  fill = fuel,
                                  alpha = scenario)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  facet_wrap(~ region, scales = "free_y") +
  labs(
    x = "",
    y = "Final Energy Consumption (EJ)",
    fill = "Fuel"
  ) +
  scale_fill_manual(values = mypal[names(mypal) %in% unique(fe_2030$fuel)],
                    label = function(x) stringr::str_to_sentence(x)) +
  scale_alpha_manual(values = c(1,1,1,0.5)) +
  theme_classic(base_size = 13) +
  theme(
    legend.direction = "horizontal",
    legend.key.size = unit(1, "lines"),
    axis.text = element_text(hjust = .5, size = 11),
    panel.grid.major.y = element_line(color = "grey90"),
    panel.spacing = unit(1.2, "lines"),
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1)
  ) +
  guides(alpha = "none")

ggsave(
  pl_fe_2030 +
    theme(legend.position = 'bottom'),
  filename = "figures/extra/TFE_2030.tiff",
  width = 12, height = 8, units = "in",  # adjust as needed
  dpi = 300,                             # print-quality resolution
  compression = "lzw",                    # common TIFF compression
  bg = "white"  # Prevents transparent background
)


# join plots
legend <- cowplot::get_legend(pl_fe_2030 +
                                ggplot2::theme(legend.direction = "horizontal",
                                               legend.text = ggplot2::element_text(size = 12)))


final_plot_fe <- cowplot::plot_grid(
  # figure
  cowplot::plot_grid(plot_fe_2030_eu +
                       labs(title = '') +
                       theme(legend.position = 'none'),
                     pl_fe_2030 +
                       labs(title = '') +
                       theme(legend.position = 'none'),
                     ncol = 1, rel_heights = c(0.6, 1),
                     labels = c("a)", "b)")),
  # legend
  legend,
  # options
  ncol = 1,
  rel_heights = c(1, 0.05)
) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )


ggplot2::ggsave(final_plot_fe,
                file = file.path(folder_paper, "final_plot_fe.png"),
                width = 330,
                height = 300,
                units = "mm")


