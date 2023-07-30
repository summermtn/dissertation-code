
# All baseline vs adaptation ----------------------------------------------

draw_key_polygon3 <- function(data, params, size) {
  lwd <- min(data$size, min(size) / 4)

  grid::rectGrob(
    width = grid::unit(0.6, "npc"),
    height = grid::unit(0.6, "npc"),
    gp = grid::gpar(
      col = data$colour,
      fill = alpha(data$fill, data$alpha),
      lty = data$linetype,
      lwd = lwd * .pt,
      linejoin = "mitre"
    ))
}


## Data
col_base_means <- c(mean_base_AA, mean_base_VV, mean_base_AV, mean_base_VA);
col_adapt_means <- c(mean_adapt_AA, mean_adapt_VV, mean_adapt_AV, mean_adapt_VA);
col_base_sds <- c(sd_base_AA, sd_base_VV, sd_base_AV, sd_base_VA);
col_adapt_sds <- c(sd_adapt_AA, sd_adapt_VV, sd_adapt_AV, sd_adapt_VA);
col_base_adapt_means <- c(col_base_means, col_adapt_means);
col_base_adapt_sds <- c(col_base_sds, col_adapt_sds);
col_type <- c("a_base","a_base","a_base","a_base","b_adapt","b_adapt","b_adapt","b_adapt")
conditions <- c("A", "B", "C", "D", "A", "B", "C", "D")
df_means <- data.frame(col_type, conditions, col_base_adapt_means, col_base_adapt_sds);

df_means %>%
  ggplot(aes(x = conditions,
             y = col_base_adapt_means,
             fill = col_type,
             ymin = col_base_adapt_means - col_base_adapt_sds,
             ymax = col_base_adapt_means + col_base_adapt_sds)) +
  geom_col(width = .5,position = position_dodge(.6),
           color = "black", key_glyph = "polygon3") +
  geom_errorbar(width = .1 , position = position_dodge(.6)) +
  scale_fill_manual(values = c("#FED966","#95B8D9"), labels = c("Baseline", "Adaptation")) +
  scale_x_discrete(labels=c('AA', 'VV', 'AV', 'VA')) +
  scale_y_continuous(expand = expansion(0),
                     limits = c(0, 800)) +
  labs(
    x = "Test-Sensory Pairing",
    y = "Reproduction of 500ms Duration",
    fill = NULL,
  ) +
  theme(
    # plot.title = element_text(size = 22, 
    #                           face ="bold", 
    #                           hjust = 0.5,
    #                           margin = margin(b=15)),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    panel.background = element_blank(),
    axis.line = element_line(colour  = "black"),
    axis.title = element_text(size = 22, color = "black",
                              face = "bold"),
    axis.text = element_text(size = 22, color = "black"),
    axis.text.x = element_text(margin = margin(t = 10)),
    axis.text.y = element_text(size = 17),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.ticks.x = element_blank(),
    legend.position = c(0.15, 0.85),
    legend.background = element_rect(color = "black"),
    legend.text = element_text(size = 15),
    legend.margin = margin(5,5,5,5),
    legend.key = element_rect(color = NA, fill = NA)
  ) +
  guides(
    fill = guide_legend( 
      keywidth = 1.2,
      keyheight = 1.2,
      default.unit = "cm")
  )

ggsave("all-baseline-v-adapatation.png", width = 10, height = 7, dpi = 300)


# pre-modal-df ------------------------------------------------------------

modalities <- c("a", "a", "b", "b")
pre_conditions <- c("a", "b", "c", "d")
pre_modal_df <-data.frame(col_base_means, col_base_sds, modalities, pre_conditions)
view(pre_modal_df)

pre_modal_df %>%
  ggplot(aes(x = pre_conditions,
             y = col_base_means,
             fill = modalities,
             ymin = col_base_means - col_base_sds,
             ymax = col_base_means + col_base_sds)) +
  geom_col(width = .5,position = position_dodge(.6),
           color = "black", key_glyph = "polygon3") +
  geom_errorbar(width = .1 , position = position_dodge(.6)) +
  scale_fill_manual(values = c("#FED966","#95B8D9"), labels = c("Unimodal", "Crossmodal")) +
  scale_x_discrete(labels=c('AA', 'VV', 'AV', 'VA')) +
  scale_y_continuous(expand = expansion(0),
                     limits = c(0, 800)) +
  labs(
    x = "Test-Sensory Pairing",
    y = "Reproduction of 500ms Duration",
    fill = NULL,
  ) +
  theme(
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    panel.background = element_blank(),
    axis.line = element_line(colour  = "black"),
    axis.title = element_text(size = 22, color = "black",
                              face = "bold"),
    axis.text = element_text(size = 22, color = "black"),
    axis.text.x = element_text(margin = margin(t = 10)),
    axis.text.y = element_text(size = 17),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.ticks.x = element_blank(),
    legend.position = c(0.15, 0.85),
    legend.background = element_rect(color = "black"),
    legend.text = element_text(size = 15),
    legend.margin = margin(5,5,5,5),
    legend.key = element_rect(color = NA, fill = NA)
  ) +
  guides(
    fill = guide_legend( 
      keywidth = 1.2,
      keyheight = 1.2,
      default.unit = "cm")
  )

ggsave("baseline-unimodal-v-crossmodal.png", width = 10, height = 7, dpi = 300)

# post_modal_plot ---------------------------------------------------------
post_modal_df <-data.frame(col_adapt_means, col_adapt_sds, modalities, pre_conditions)
post_modal_df
pre_modal_df

post_modal_df %>%
  ggplot(aes(x = pre_conditions,
             y = col_adapt_means,
             fill = modalities,
             ymin = col_adapt_means - col_adapt_sds,
             ymax = col_adapt_means + col_adapt_sds)) +
  geom_col(width = .5,position = position_dodge(.6),
           color = "black", key_glyph = "polygon3") +
  geom_errorbar(width = .1 , position = position_dodge(.6)) +
  scale_fill_manual(values = c("#FED966","#95B8D9"), labels = c("Unimodal", "Crossmodal")) +
  scale_x_discrete(labels=c('AA', 'VV', 'AV', 'VA')) +
  scale_y_continuous(expand = expansion(0),
                     limits = c(0, 800)) +
  labs(
    x = "Test-Sensory Pairing",
    y = "Reproduction of 500ms Duration",
    fill = NULL,
  ) +
  theme(
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    panel.background = element_blank(),
    axis.line = element_line(colour  = "black"),
    axis.title = element_text(size = 22, color = "black",
                              face = "bold"),
    axis.text = element_text(size = 22, color = "black"),
    axis.text.x = element_text(margin = margin(t = 10)),
    axis.text.y = element_text(size = 17),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.ticks.x = element_blank(),
    legend.position = c(0.15, 0.85),
    legend.background = element_rect(color = "black"),
    legend.text = element_text(size = 15),
    legend.margin = margin(5,5,5,5),
    legend.key = element_rect(color = NA, fill = NA)
  ) +
  guides(
    fill = guide_legend( 
      keywidth = 1.2,
      keyheight = 1.2,
      default.unit = "cm")
  )


ggsave("adaptation-unimodal-v-crossmodal.png", width = 10, height = 7, dpi = 300)

# aud_vis_AA_VV_plot ------------------------------------------------------
AA_VV_df


AA_VV_df %>%
  ggplot(aes(x = aud_vis_AA_VV_conditions,
             y = aud_vis_AA_VV_means,
             fill = aud_vis_AA_VV_types,
             ymin = aud_vis_AA_VV_means - aud_vis_AA_VV_sd,
             ymax = aud_vis_AA_VV_means + aud_vis_AA_VV_sd)) +
  geom_col(width = .5,position = position_dodge(.6),
           color = "black", key_glyph = "polygon3") +
  geom_errorbar(width = .1 , position = position_dodge(.6)) +
  scale_fill_manual(values = c("#FED966","#95B8D9"), labels = c("Audio", "Visual")) +
  scale_x_discrete(labels=c('AA', 'VV', 'AV', 'VA')) +
  scale_y_continuous(expand = expansion(0),
                     limits = c(0, 800)) +
  labs(
    x = "Test-Sensory Pairing",
    y = "Reproduction of 500ms Duration",
    fill = NULL,
  ) +
  theme(
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    panel.background = element_blank(),
    axis.line = element_line(colour  = "black"),
    axis.title = element_text(size = 22, color = "black",
                              face = "bold"),
    axis.text = element_text(size = 22, color = "black"),
    axis.text.x = element_text(margin = margin(t = 10)),
    axis.text.y = element_text(size = 17),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.ticks.x = element_blank(),
    legend.position = c(0.15, 0.85),
    legend.background = element_rect(color = "black"),
    legend.text = element_text(size = 15),
    legend.margin = margin(5,5,5,5),
    legend.key = element_rect(color = NA, fill = NA)
  ) +
  guides(
    fill = guide_legend( 
      keywidth = 1.2,
      keyheight = 1.2,
      default.unit = "cm")
  )


ggsave("adaptaion-unimodal.png", width = 10, height = 7, dpi = 300)


# aud_vis_AV_VA_means -----------------------------------------------------
AV_VA_df

AV_VA_df %>%
  ggplot(aes(x = aud_vis_AV_VA_conditions,
             y = aud_vis_AV_VA_means,
             fill = aud_vis_AV_VA_types,
             ymin = aud_vis_AV_VA_means - aud_vis_AV_VA_sd,
             ymax = aud_vis_AV_VA_means + aud_vis_AV_VA_sd)) +
  geom_col(width = .5,position = position_dodge(.6),
           color = "black", key_glyph = "polygon3") +
  geom_errorbar(width = .1 , position = position_dodge(.6)) +
  scale_fill_manual(values = c("#FED966","#95B8D9"), labels = c("Audio", "Visual")) +
  scale_x_discrete(labels=c('AV', 'VA', 'AV', 'VA')) +
  scale_y_continuous(expand = expansion(0),
                     limits = c(0, 800)) +
  labs(
    x = "Test-Sensory Pairing",
    y = "Reproduction of 500ms Duration",
    fill = NULL,
  ) +
  theme(
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    panel.background = element_blank(),
    axis.line = element_line(colour  = "black"),
    axis.title = element_text(size = 22, color = "black",
                              face = "bold"),
    axis.text = element_text(size = 22, color = "black"),
    axis.text.x = element_text(margin = margin(t = 10)),
    axis.text.y = element_text(size = 17),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.ticks.x = element_blank(),
    legend.position = c(0.50, 0.85),
    legend.background = element_rect(color = "black"),
    legend.text = element_text(size = 15),
    legend.margin = margin(5,5,5,5),
    legend.key = element_rect(color = NA, fill = NA)
  ) +
  guides(
    fill = guide_legend( 
      keywidth = 1.2,
      keyheight = 1.2,
      default.unit = "cm")
  )


ggsave("adaptaion-crossmodal.png", width = 10, height = 7, dpi = 300)


# difference_df -----------------------------------------------------------

difference_df

difference_df %>%
  ggplot(aes(x = difference_types,
             y = mean_difference_base_adapt,
             fill = difference_conditions,
             ymin = mean_difference_base_adapt - sd_difference_base_adapt,
             ymax = mean_difference_base_adapt + sd_difference_base_adapt)) +
  geom_col(width = .5,position = position_dodge(.6),
           color = "black", key_glyph = "polygon3") +
  geom_errorbar(width = .1 , position = position_dodge(.6)) +
  scale_fill_manual(values = c("#FED966","#95B8D9"), labels = c("Unimodal", "Crossmodal")) +
  scale_x_discrete(labels=c('AA', 'VV', 'AV', 'VA')) +
  scale_y_continuous(expand = expansion(0),
                     limits = c(-300, 300)) +
  labs(
    x = "Test-Sensory Pairing",
    y = "Reproduction of 500ms Duration",
    fill = NULL,
  ) +
  theme(
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    panel.background = element_blank(),
    axis.line = element_line(colour  = "black"),
    axis.title = element_text(size = 22, color = "black",
                              face = "bold"),
    axis.text = element_text(size = 22, color = "black"),
    axis.text.x = element_text(margin = margin(t = 10)),
    axis.text.y = element_text(size = 17),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.ticks.x = element_blank(),
    legend.position = c(0.75, 0.85),
    legend.background = element_rect(color = "black"),
    legend.text = element_text(size = 15),
    legend.margin = margin(5,5,5,5),
    legend.key = element_rect(color = NA, fill = NA)
  ) +
  guides(
    fill = guide_legend( 
      keywidth = 1.2,
      keyheight = 1.2,
      default.unit = "cm")
  )


ggsave("difference-unimodal-crossmodal.png", width = 10, height = 7, dpi = 300)









