data(unemployment)

unemployment <- unemployment %>% 
  mutate(val = map(nrow(.), ~ data_frame(x = c(1, 2, 3), y = c(2, 3, 5)))) %>% 
  mutate(val = map(val, list_parse))

hist(unemployment$value)
quantile(unemployment$value)

hcm <- hcmap("countries/us/us-all-all", data = unemployment,
             download_map_data = FALSE,
             name = "Unemployment", value = "value",
             joinBy = c("hc-key", "code"),
             borderColor = "transparent") %>%
  hc_colorAxis(dataClasses = color_classes(c(seq(0, 10, by = 2), 50))) %>% 
  hc_legend(layout = "vertical", align = "right",
            floating = TRUE, valueDecimals = 0, valueSuffix = "%") %>% 
  hc_tooltip(
    useHTML = TRUE,
    pointFormatter = tooltip_chart(
      accesor = "val",
      hc_opts = list(title = list(text = "point.name"))
    )
  )
hcm
