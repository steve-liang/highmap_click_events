data(gapminder, package = "gapminder")
glimpse(gapminder)

gp <- gapminder %>% 
  arrange(desc(year)) %>% 
  distinct(country, .keep_all = TRUE)

gppop <- gapminder %>% 
  select(country, x = year, y = pop) %>% 
  nest(-country) %>% 
  mutate(data = map(data, list_parse)) %>% 
  rename(ttdata = data)

gp2 <- gapminder %>% 
  group_by(country) %>% 
  do(ttdata = .$lifeExp)
gp2

gp2 <- gapminder %>% 
  nest(-country) %>% 
  mutate(data = map(data, mutate_mapping, hcaes(x = lifeExp, y = gdpPercap), drop = TRUE),
         data = map(data, list_parse)) %>% 
  rename(ttdata = data)
gp2

gptot <- left_join(gp, gppop)

hc <- hchart(gptot, "point", hcaes(lifeExp, gdpPercap, name = country, size = pop, group = continent)) %>% 
  hc_yAxis(type = "logarithmic")

hc

pfmc <-  tooltip_chart(accesor = "ttdata")

cat(pfmc)

hc %>% 
  hc_tooltip(useHTML = TRUE, pointFormatter = tooltip_chart(accesor = "ttdata", hc_opts = list(plotOptions = list(series = list(label = list(enabled = FALSE))))))

hc %>% 
  hc_tooltip(useHTML = TRUE, pointFormatter = tooltip_chart(
    accesor = "ttdata",
    hc_opts = list(chart = list(type = "column"))
  ))

hc %>% 
  hc_tooltip(
    useHTML = TRUE,
    # positioner = JS("function () { return { x: this.chart.plotLeft + 10, y: 10}; }"),
    pointFormatter = tooltip_chart(
      accesor = "ttdata",
      hc_opts = list(
        title = list(text = "point.country"),
        xAxis = list(title = list(text = "lifeExp")),
        yAxis = list(title = list(text = "gdpPercap")))
    ))


hc %>% 
  hc_tooltip(
    useHTML = TRUE,
    pointFormatter = tooltip_chart(
      accesor = "ttdata",
      hc_opts = list(
        legend = list(enabled = TRUE),
        series = list(list(color = "gray", name = "point.name"))
      )
    )
  )
