library(shiny)
library(tidyverse)

# Adatok beolvasása
s <- read.csv2("EUSILCH20052023bruttonettoMINDENKI.csv", stringsAsFactors = FALSE)
medianok <- read.csv2("medianok.csv", stringsAsFactors = FALSE)

s$y <- s$HB010
s$bruttoEGYFOGY <- s$HY010 / s$HX050
s$nettoEGYFOGY <- s$HY020 / s$HX050
s$bruttonetto <- 0
s$bruttonetto[s$HY010 < s$HY020] <- 1

# UI
ui <- fluidPage(
  titlePanel("Net and Gross Income Data in the Hungarian EU-SILC Sample"),
  sidebarLayout(
    sidebarPanel(width = 3,
                 selectInput("ev", "Year", choices = 2005:2023, selected = 2020),
                 radioButtons("csoport", "Household type", choices = c(
                   "full sample" = "teljes",
                   "single-person households" = "egyfos",
                   "two-person households < 65" = "ketfos65minusz",
                   "two-person households ≥ 65" = "ketfos65plusz",
                   "other households without children" = "masgyereknelkul",
                   "households with children" = "gyermekes"
                 ), selected = "egyfos"),
                 numericInput("felsobertek", "Upper limit (EUR)", value = NA, min = 2000, step = 1000)
    ),
    mainPanel(width = 9,
              plotOutput("bruttoNettoPlot"),
              verbatimTextOutput("statSzoveg"),
              uiOutput("infoSzoveg")
    )
  )
)


# SERVER
server <- function(input, output, session) {
  
  observeEvent(input$ev, {
    alapertelmezett <- medianok %>%
      filter(y == input$ev) %>%
      pull(HX090median) %>% round()
    
    if(length(alapertelmezett) == 1 && !is.na(alapertelmezett)) {
      updateNumericInput(session, "felsobertek", value = as.numeric(alapertelmezett))
    }
  }, ignoreNULL = FALSE)
  
  csoport_nevek <- c(
    "teljes" = "full sample ",
    "egyfos" = "single-person households",
    "ketfos65minusz" = "two-person households < 65",
    "ketfos65plusz" = "two-person households ≥ 65",
    "masgyereknelkul" = "other households without children",
    "gyermekes" = "households with children"
  )
  
  kuszob <- reactive({
    val <- medianok %>% filter(y == input$ev) %>% pull(AROP60tresh)
    if(length(val) == 1) val else NA
  })
  
  median <- reactive({
    val <- medianok %>% filter(y == input$ev) %>% pull(HX090median)
    if(length(val) == 1) val else NA
  })
  
  teteje <- reactive({
    val <- medianok %>% filter(y == input$ev) %>% pull(also90)
    if(length(val) == 1) val else NA
  })
  
  szurt_adat <- reactive({
    req(input$ev, input$felsobertek)
    adat <- s %>% filter(y == input$ev, bruttoEGYFOGY < input$felsobertek)
    adat <- switch(input$csoport,
                   "teljes" = adat,
                   "egyfos" = adat %>% filter(HX060 == 5),
                   "ketfos65minusz" = adat %>% filter(HX060 == 6),
                   "ketfos65plusz" = adat %>% filter(HX060 == 7),
                   "masgyereknelkul" = adat %>% filter(HX060 == 8),
                   "gyermekes" = adat %>% filter(HX060 %in% c(9,10,11,12,13)),
                   adat)
    adat
  })
  
  histogram_adat <- reactive({
    adat <- s %>% filter(y == input$ev)
    adat <- switch(input$csoport,
                   "teljes" = adat,
                   "egyfos" = adat %>% filter(HX060 == 5),
                   "ketfos65minusz" = adat %>% filter(HX060 == 6),
                   "ketfos65plusz" = adat %>% filter(HX060 == 7),
                   "masgyereknelkul" = adat %>% filter(HX060 == 8),
                   "gyermekes" = adat %>% filter(HX060 %in% c(9,10,11,12,13)),
                   adat)
    adat
  })
  
  output$bruttoNettoPlot <- renderPlot({
    adat <- szurt_adat()
    req(nrow(adat) > 0)
    sorrend <- order(adat$bruttoEGYFOGY)
    brutto <- adat$bruttoEGYFOGY[sorrend]
    netto <- adat$nettoEGYFOGY[sorrend]
    x_vals <- seq_along(brutto)
    w <- 0.7
    plot(x = x_vals, y = brutto, type = "n",
         xlab = "", ylab = "Equivalised income (EUR)",
         main = paste("Gross and net incomes", input$ev, csoport_nevek[input$csoport]),
         ylim = c(-100, max(brutto, na.rm = TRUE)),
         xaxs = "i", bty = "n", xaxt="n")
    rect(x_vals - w, 0, x_vals + w, brutto, col = "red", border = NA)
    rect(x_vals - w, 0, x_vals + w, netto,
         col = rgb(173, 216, 230, alpha = 200, maxColorValue = 255), border = NA)
    abline(h = kuszob(), col = "black", lty = 2, lwd = 1)
    also_hatar <- median() * 0.58
    felso_hatar_kuszob <- median() * 0.62
    kozel_kuszobhoz_netto <- netto >= also_hatar & netto <= felso_hatar_kuszob
    points(x = x_vals[kozel_kuszobhoz_netto], y = rep(-200, sum(kozel_kuszobhoz_netto)),
           pch = 1, col = "black", cex = 0.5)
    text(x=length(adat$bruttoEGYFOGY),y=kuszob()+150,labels="poverty threshold",pos=2,cex=1.2)
  })
  
  output$statSzoveg <- renderPrint({
    adat_szurt <- szurt_adat()
    adat_teljes_ev <- s %>% filter(y == input$ev)
    
    if(nrow(adat_szurt) == 0) {
      cat("Nincs megjeleníthető adat a megadott feltételekkel.")
      return()
    }
    
    esetek_szama <- nrow(adat_szurt)
    teljes_minta <- switch(input$csoport,
                           "teljes" = adat_teljes_ev,
                           "egyfos" = adat_teljes_ev %>% filter(HX060 == 5),
                           "ketfos65minusz" = adat_teljes_ev %>% filter(HX060 == 6),
                           "ketfos65plusz" = adat_teljes_ev %>% filter(HX060 == 7),
                           "masgyereknelkul" = adat_teljes_ev %>% filter(HX060 == 8),
                           "gyermekes" = adat_teljes_ev %>% filter(HX060 %in% c(9,10,11,12,13)),
                           adat_teljes_ev)
    
    egyfos_szam <- nrow(teljes_minta)
    brutto_kisebb_netto <- sum(teljes_minta$bruttoEGYFOGY < teljes_minta$nettoEGYFOGY, na.rm = TRUE)
    
    also_hatar <- median() * 0.58
    felso_hatar_kuszob <- median() * 0.62
    netto_teljes <- teljes_minta$nettoEGYFOGY
    szegenyseg_kuszob_esetek <- sum(netto_teljes >= also_hatar & netto_teljes <= felso_hatar_kuszob, na.rm = TRUE)
    
    p_brutto_netto <- round(100 * brutto_kisebb_netto / egyfos_szam, 0)
    p_szegenyseg <- round(100 * szegenyseg_kuszob_esetek / egyfos_szam, 0)
    p_abrazolt <- round(100 * esetek_szama / egyfos_szam, 0)
    
    cat("Number of individuals in the selected group:", egyfos_szam, "\n")
    cat("Number of cases where gross < net income in the selected group:", brutto_kisebb_netto, sprintf("(%s%%)", p_brutto_netto), "\n")
    cat("Number of cases near the poverty threshold in the selected group:", szegenyseg_kuszob_esetek, sprintf("(%s%%)", p_szegenyseg), "\n")
    cat("Number of visualized cases considering the upper limit of the line chart:", esetek_szama, sprintf("(%s%%)", p_abrazolt), "\n")
  })
  
  # Statikus HTML-szöveg, formázható stílussal
  output$infoSzoveg <- renderUI({
    HTML("<b>The above chart shows the gross and disposable (net) equivalised income per consumption unit for individuals in the selected group, based on the Hungarian sample of EU-SILC.</b>
<p> Each thin line represents one person, displayed in order of their gross income.

<ul>
  <li><span style='color:red'>The person’s gross income level is indicated by a red line</span>,</li>
  <li><span style='color:RoyalBlue'>the person’s disposable (net) income level is indicated by a blue line.</span></li>
</ul>

<p><b>Taking into account the rules of the Hungarian tax system, the condition that gross income should always be higher 
 than net income must be met; therefore, blue lines extending above the red lines clearly indicate data errors.</b></p>

<p>The horizontal dashed line indicates the poverty threshold for the given year.
<p>The small circles below the figure mark those individuals whose net income is close to the poverty threshold 
         (i.e., falling within the range defined by 58–62% of the median).</p>")
    
  })
}

shinyApp(ui = ui, server = server)
