alphas_table <- function(.data){
  
  # Define orders
  timing_order <- c("mom_1_12", "mom_1_36", "mom_1_60",
                    "mom_3_12", "mom_3_36", "mom_3_60",
                    "mom_6_12", "mom_6_36", "mom_6_60",
                    "mom_12_12", "mom_12_36", "mom_12_60", "var")
  
  strategy_order <- c(
    "naive",
    "ts_mom12", "ts_mom6", "ts_mom3", "ts_mom1",
    "cs_mom12", "cs_mom6", "cs_mom3", "cs_mom1",
    "ts_carry", "ss_carry",
    "dol_carry", "dol"
  )
  
  .data |> 
    dplyr::mutate(
      stars = dplyr::case_when(
        p.value < 0.01 ~ "***",
        p.value < 0.05 ~ "**",
        p.value < 0.1  ~ "*",
        TRUE           ~ ""
      ),
      alpha_label = sprintf("%.4f%s", estimate, stars)
    ) |> 
    dplyr::select(strategy, timing, alpha_label) |> 
    tidyr::pivot_wider(
      names_from = timing, 
      values_from = alpha_label
    ) |> 
    kableExtra::kbl(booktabs = TRUE, align = "l") |> 
    kableExtra::kable_styling(latex_options = c("striped", "scale_down", "hold_position")) |> 
    kableExtra::footnote(
      general = "Note: * p < 0.10, ** p < 0.05, *** p < 0.01",
      general_title = "",
      threeparttable = TRUE
    )
  
}
