library(shiny)
library(tidyverse)

# Adatok betöltése

sEGYFO <- read.csv2("EUSILCH20052023legszukebbEGYFO.csv")
medianok <- read.csv2("medianok.csv")
HUempELOSZLASegyfo <- read.csv2("HUempeloszlasEGYFO.csv")

ui <- fluidPage(
  tags$h4('Income distribution of single-person households'),
  
  fluidRow(
    column(1, 
           selectInput(inputId = "ev",
                       label = "Year:",
                       choices = 2005:2023,
                       selected = 2020)
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
  HX090_hist_minimalber_egyfos_teljeseloszlas <- function(ev){
    kuszob <- medianok$AROP60tresh[medianok$y == ev]
    minimalber <- medianok$minimalber[medianok$y == ev]
    teteje <- medianok$also90[medianok$y == ev]
    #Hisztogram elmentése, hogy lehessen szerkeszteni
    hist <- hist(sEGYFO$HX090[sEGYFO$y == ev & sEGYFO$HX090 < teteje[1] & sEGYFO$HX090 > -500],
                 plot = FALSE,
                 breaks = seq(-500,
                              round(teteje[1] / 100 + 1) * 100,
                              100))
    # Egyedi színezési tartományok év szerint
    highlight_ranges <- list(
      "2017" = list(c(-500,0)),
      "2018" = list(c(0, 1)),
      "2019" = list(c(0,1)),
      "2020" = list(c(3800,4000)),
      "2021" = list(c(0,1)),                  
      "2022" = list(c(0,1)),
      "2023" = list(c(4400,4500),c(5800,6000)))
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
         yla="Number of cases")
    abline(v = kuszob[1], col = "#377eb8", lwd=2)
    text(x = ifelse(minimalber[1]>-5000,minimalber[1],kuszob[1]),
         y = max(hist$counts) * 0.75, 
         labels = paste("poverty threshold ", round(kuszob[1]),"€"), 
         pos = 2, col = "#377eb8",cex=0.8)
    abline(v= minimalber[1],col="darkgreen",lty=2)
    # Minimálbér felirat csak akkor, ha a minimálbér nagyobb, mint -5000
    if (minimalber[1] > -5000) {
      text(x = minimalber[1],
           y = max(hist$counts) * 0.7,
           labels = paste("minimum wage", round(minimalber[1]), "€"),
           pos = 2, col = "darkgreen", cex = 0.8)
    }
    # normálgörbe rárajzolása
    mu <- median(sEGYFO$HX090[sEGYFO$y == ev & sEGYFO$HX090 < teteje[1] & sEGYFO$HX090 > -500],na.rm=T) # medián legyen a várható érték
    x_vals <- seq(0, teteje[1], length.out = 18) # számsorozat a vonal generálásához
    y_vals <- HUempELOSZLASegyfo$y #normális eloszlás sűrűségfüggvénye
    y_vals <- y_vals * quantile(hist$counts,probs=seq(0,1,0.05))["95%"]  / max(y_vals) # felszorzott ávltozat picit emelve
    lines(x_vals, y_vals, col = "darkgreen", lwd = 2)
  }
  
  
  output$HX090Hist <- renderPlot({
    HX090_hist_minimalber_egyfos_teljeseloszlas(ev = input$ev)
  })
  
  szovegek <- data.frame(ev= 2005:2023, szoveg=character(19),stringsAsFactors=F)
  szovegek$szoveg[szovegek$ev %in% 2005:2016] <- 
    "Between 2005 and 2016, the well-known pattern of the income distribution is observed, as indicated by the green line."
  szovegek$szoveg[szovegek$ev==2017] <- 
    "In 2017, there is a significant number of people with negative income, i.e. those who paid more tax than they had income. Negative incomes always and everywhere occur, but this high number is unprecedented in the history of Hungarian data.  Apart from this, the distribution shows a pattern typical of the income distribution in general."
  szovegek$szoveg[szovegek$ev %in% 2018:2019] <- 
    "In 2018 and 2019, the distributions show the classic shape of the income distribution, as in the 2005-2016 period."
  szovegek$szoveg[szovegek$ev==2020] <- 
    "In 2020, there is a significant income clustering just above the poverty threshold that cannot be explained by reasons related to income distribution in general, social trends or administrative measures. This column is also the most frequent value, which is unusual for the income distribution. The green line projects the classical shape of the income distribution onto the 2020 data, showing the expected pattern based on the 2005-2016 period. "
  szovegek$szoveg[szovegek$ev==2021] <- 
    "In 2021, there is a slight concentration of observations below the poverty threshold, while the band between €3300-3500 is virtually empty. The green line projects the classic shape of the income distribution onto the 2021 data, showing the expected pattern based on the 2005-2016 period."
  szovegek$szoveg[szovegek$ev==2022] <- 
    "In 2022, the income band of €400 below the minimum wage is virtually empty. The green line shows the classic shape of the income distribution in 2022, representing the expected pattern over the 2005-2016 period."
  szovegek$szoveg[szovegek$ev==2023] <- 
    "In 2023, there are two spikes in income data, one just above the poverty line at €4400 and another between €5800-6000. In addition, the bands below €2000 are virtually empty, as if there were no single-person households in Hungary with an income below €2000 net per year. While 4-8% of single-person households belong to this income band in the 2020-2022 period. The green line shows the classic pattern of income distribution for 2023 data, representing the expected pattern based on the 2005-2016 period."
  
  output$szoveg <- renderUI({
    p(strong(szovegek$szoveg[szovegek$ev == input$ev]))
  })
}

shinyApp(ui = ui, server = server)
