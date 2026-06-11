library(shiny)
library(tidyverse)

# Adatok betöltése

sGYER <- read.csv2("EUSILCH20052023legszukebbGYER.csv")
medianok <- read.csv2("medianok.csv")
HUempELOSZLASgyer <- read.csv2("HUempeloszlasGYER.csv")


ui <- fluidPage(
  tags$h4('Income distribution of persons in households with children in the Hungarian EU-SILC surveys'),
  
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
  HX090_hist_GYER <- function(ev){
    kuszob <- medianok$AROP60tresh[medianok$y == ev]
    teteje <- medianok$also90[medianok$y == ev]
    #Hisztogram elmentése, hogy lehessen szerkeszteni
    hist <- hist(sGYER$HX090[sGYER$y == ev & sGYER$HX090 < teteje[1] & sGYER$HX090 > -500],
                 plot = FALSE,
                 breaks = seq(-500,
                              round(teteje[1] / 100 + 1) * 100,
                              100))
    # Egyedi színezési tartományok év szerint
    highlight_ranges <- list(
      "2017" = list(c(-500,0)),
      "2018" = list(c(3200,3300)),
      "2019" = list(c(3500,3700)),
      "2020" = list(c(3800,3900)),
      "2021" = list(c(4000,4100)),                  
      "2022" = list(c(0,1)),
      "2023" = list(c(0,1)))
    # Alapértelmezett színezés: minden szürke
    colors <- rep("#999999", length(hist$mids))
    # Szegénységi küszöb alatti oszlopok kékre állítása
    colors[hist$mids < kuszob[1]] <- "#984ea3"
    # Kiemelt tartományok pirosra állítása
    ranges <- highlight_ranges[[as.character(ev)]]
    if (!is.null(ranges)) {
      for (range in ranges) {
        colors[hist$mids >= range[1] & hist$mids <= range[2]] <- "#ca0020"
      }
    }
    #Ábrázolás
    plot(hist, col = colors, main = NULL,
         xlab = "Equivalised disposable household income in Euros, in €100 intervals",
         ylab = "Number of cases")
    abline(v = kuszob[1], col = "#377eb8", lwd=2)
    text(x = kuszob[1], 
         y = max(hist$counts) * 0.925, 
         labels = paste("poverty threshold", round(kuszob[1]),"€"), 
         pos = 2, col = "#377eb8")
    # normálgörbe rárajzolása
    x_vals <- seq(0, teteje[1], length.out = 15) # számsorozat a vonal generálásához
    y_vals <- HUempELOSZLASgyer$y
    y_vals <- y_vals * quantile(hist$counts,probs=seq(0,1,0.05))["95%"]  / max(y_vals) # felszorzott ávltozat picit emelve
    
    
    lines(x_vals, y_vals, col = "darkgreen", lwd = 2)
    
  }
  
  
  
  output$HX090Hist <- renderPlot({
    HX090_hist_GYER(ev = input$ev)
  })
  
  szovegek <- data.frame(ev= 2005:2023, szoveg=character(19),stringsAsFactors=F)
  szovegek$szoveg[szovegek$ev %in% 2005:2016] <- 
    "Between 2005 and 2016, the well-known pattern of the income distribution is observed, as indicated by the green line."
  szovegek$szoveg[szovegek$ev==2017] <- 
    "In 2017, the share of observations in the income band below €2,400 has decreased dramatically, compared to previous years. At the same time, the share of observations just below the poverty threshold is smaller than in previous years. Apart from this, the distribution shows the classical shape of the income distribution."
  szovegek$szoveg[szovegek$ev==2018] <- 
    "In 2018, we see at least twice as many persons in households with children living directly in the €100 income band (red column), which includes the poverty threshold. Apart from this, the distribution shows the classical income distribution."
  szovegek$szoveg[szovegek$ev==2019] <- 
    "In 2019, the number of persons in the two income bands right above the poverty threshold is significantly higher than expected. At the same time, the income bands right below the threshold are emptier than expected. The green line projects the classic shape of the income distribution onto the 2019 data, showing the expected pattern based on the 2005-2016 period."
  szovegek$szoveg[szovegek$ev==2020] <- 
    "In 2020, there is a significant clustering of observations just below the poverty threshold, which cannot be explained by reasons related to income distribution in general, social trends or administrative measures. This column is also has the highest frequency, which is unusual for the income distribution. The green line shows the classical shape of the income distribution for 2020 data."
  szovegek$szoveg[szovegek$ev==2021] <- 
    "In 2021, there is a significant income clustering just above the poverty threshold that cannot be explained by reasons related to income distribution in general, social trends or administrative measures. This column has also the highest frequency, which is unusual for the income distribution. The green line shows the classical shape of the income distribution for the 2021 data. The line also shows that there are far fewer cases than expected in the income bands below the poverty line."
  szovegek$szoveg[szovegek$ev==2022] <- 
    "In 2022, prevalence is lower than expected just above the poverty threshold. The green line projects the classic shape of the income distribution onto the 2022 data, showing the expected pattern based on the 2005-2016 period."
  szovegek$szoveg[szovegek$ev==2023] <- 
    "In 2023, the majority of persons with an income below the poverty threshold is concentrated in bands below €1800. This is unprecedented when compared to previous years and contrasts with the classical pattern of income distribution, shown by the green line in the figure."
  
  output$szoveg <- renderUI({
    p(strong(szovegek$szoveg[szovegek$ev == input$ev]))
  })
}

shinyApp(ui = ui, server = server)
