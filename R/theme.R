library(highcharter)

themer <- hc_theme(
  colors = c("#1B1C3A","#d7e1ee", "#bfcbdb", "#a4a2a8", "#df8879", "#c86558", "#b04238", "#991f17"),
  chart = list(
    backgroundColor = '#FFFFFF'
  ),
  loading = list (
    hideDuration = 1000,
    showDuration = 1000
  ),
  title = list(
    style = list(
      color = '#000000',
      fontFamily = "Roboto",
      fontWeight = "bold",
      fontSize= "22px"
    )
  ),
  subtitle = list(
    style = list(
      color = '#000000',
      fontFamily = "Roboto",
      fontSize = "19px"
    )
  ),
  legend = list(
    itemStyle = list(
      fontFamily = 'Open Sans',
      color = 'black',
      fontSize = '14px'
    ),
    itemHoverStyle = list(
      color = 'gray'
    ),
    plotOptions = list(
      dataLabels = list(
        style = list (
          fontSize = '16px',
          fontFamily = 'Open Sans',
          fontWeight = 'normal',
          textShadow = FALSE,
          textOutline = FALSE
        )
      )
    ),
    yAxis = list(
      dataLabel = list(
        fontSize = '19px'
      ),
      labels = list(
        fontSize = '16px'
      )
    ),
    hc_xAxis = list(
      fontSize = '16px',
      gridLineWidth = 1
    )
  )
)
