library(shiny)
library(tidyverse)

# Adatok betöltése

s <- read.csv2("EUSILCH20052023legszukebb.csv")
medianok <- read.csv2("medianok.csv")
HUempELOSZLAS <- read.csv2("HUempeloszlas.csv")

ui <- fluidPage(
  tags$h4('Income distribution of the Hungarian EU-SILC sample'),
  
  fluidRow(
    column(1, 
           selectInput(inputId = "ev",
                       label = "Year:",
                       choices = 2005:2023,
                       selected = 2021)
    ),
    column(11, 
           plotOutput("HX090Hist", width = "100%")
    )
  ),
  
  fluidRow(
    column(12, 
           uiOutput("szoveg")
    )
  ),
  
  tags$style(HTML("
    .selectize-input {
      margin-bottom: 5px;
      width: 80px; /* Állítsd be az ideális szélességet */
    }
  "))
)

server <- function(input, output){
  HX090_hist <- function(ev){
    kuszob <- medianok$AROP60tresh[medianok$y == ev]
    teteje <- medianok$also90[medianok$y == ev]
    
    hist <- hist(s$HX090[s$y == ev & s$HX090 < teteje[1] & s$HX090 > -500],
                 plot = FALSE,
                 breaks = seq(-500, round(teteje[1] / 100 + 1) * 100, 100)) 
    
    highlight_ranges <- list(
      "2017" = list(c(-500,0)),
      "2018" = list(c(3200, 3300)),
      "2019" = list(c(3500,3800)),
      "2020" = list(c(3800,4000)),
      "2021" = list(c(4000,4100)),                  
      "2022" = list(c(0,1)),
      "2023" = list(c(7800,7900),c(4400,4500))
    )
    
    colors <- rep("#999999", length(hist$mids))
    colors[hist$mids < kuszob[1]] <- "#984ea3"
    
    ranges <- highlight_ranges[[as.character(ev)]]
    if (!is.null(ranges)) {
      for (range in ranges) {
        colors[hist$mids >= range[1] & hist$mids <= range[2]] <- "#ca0020"
      }
    }
    
    plot(hist, col = colors, main=NULL,
         xlab = "Equivalised household income in Euros, in €100 intervals", 
         ylab="Number of cases")
    
    abline(v = kuszob[1], col = "#377eb8", lwd=2)
    text(x = kuszob[1], 
         y = max(hist$counts) * 0.85, 
         labels = paste("poverty threshold", round(kuszob[1]),"€"), 
         pos = 2, col = "#377eb8",cex=1.4)
    
    mu <- medianok$HX090median[medianok$y == ev]
    sigma <- sd(s$HX090[s$y == ev & s$HX090 < teteje & s$HX090 > -500])
    x_vals <- seq(0, teteje[1], length.out = 16)
    y_vals <- HUempELOSZLAS$y
    y_vals <- y_vals * quantile(hist$counts,probs=seq(0,1,0.05))["95%"]  / max(y_vals)  
    lines(x_vals, y_vals, col = "darkgreen", lwd = 2)
  }
  
  output$HX090Hist <- renderPlot({
    HX090_hist(ev = input$ev)
  })
  
  szovegek <- data.frame(ev= 2005:2023, szoveg=character(19),stringsAsFactors=F)
  szovegek$szoveg[szovegek$ev %in% 2005:2016] <- 
    "Between 2005 and 2016, the well-known pattern of the income distribution is observed, as indicated by the green line."
  szovegek$szoveg[szovegek$ev==2017] <- 
    "In 2017, 1% of the sample had a negative income, meaning they paid more taxes than they had income. Negative incomes always and everywhere occur, but the rate of around 1% is unprecedented in the history of Hungarian data. Apart from this, the distribution shows a regular pattern, in line with the well-known pattern of income distribution, as indicated by the green line."
  szovegek$szoveg[szovegek$ev==2018] <- 
    "In 2018, the poverty threshold is €3,254, with around 600 people's income clustered directly around this value (almost twice the next highest frequency income band). This column is also the most frequent value, which is unusual for the income distribution. The green line projects the classic shape of the income distribution onto the 2018 data, displaying the expected pattern based on the 2005-2016 period."
  szovegek$szoveg[szovegek$ev==2019] <- 
    "In 2019, the share of observations in the band just above the poverty threshold of €300 is much higher than in the years before 2016. This band between €3,600 and €3,700 (middle red column) is also the most frequent value, which is unusual for the income distribution. The green line projects the classic shape of the income distribution onto the 2019 data, showing the pattern expected from the 2005-2016 period."
  szovegek$szoveg[szovegek$ev==2020] <- 
    "In 2020, the number of observations around the poverty threshold (right below and above) is significant. These columns are also the most frequent values, which is unusual for the income distribution. The green line projects the classical shape of the income distribution onto the 2020 data, showing the expected pattern based on the 2005-2016 period."
  szovegek$szoveg[szovegek$ev==2021] <- 
    "In 2021, the number of observations above the poverty threshold is significant. The green line projects the classic shape of the income distribution to 2021, showing the expected pattern based on the 2005-2016 period."
  szovegek$szoveg[szovegek$ev==2022] <- 
    "In 2022, the number of observations below the poverty thershold is less significant than in the previous years. The green line shows the classic shape of the income distribution for 2022 data, representing the expected pattern based on the 2005-2016 period."
  szovegek$szoveg[szovegek$ev==2023] <- 
    "In 2023, the share of persons in the lowest income brackets is very high. In addition, clustering can be seen both slightly below and above the poverty threshold. The green line projects the classic shape of the income distribution on the 2023 data, showing the expected pattern based on the 2005-2016 period."
  
  output$szoveg <- renderUI({
    p(strong(szovegek$szoveg[szovegek$ev == input$ev]))
  })
}

shinyApp(ui = ui, server = server)

  