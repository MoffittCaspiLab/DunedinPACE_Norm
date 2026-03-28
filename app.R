library(shiny)
library(plotly)
library(tidyverse)
library(paletteer)
library(glue)
library(shinythemes)
library(bslib)

# App login
library(sodium)
library(shinymanager)



# ------------------ Precompute base data / graphs -----------------------------
# Create color palette
# Find colors
#use_colors <- paletteer_c("ggthemes::Sunset-Sunrise Diverging", 8)

use_rgba   <- c('rgba(51,  96,140, 0.3)', 'rgba(139,103,161, 0.3)', 'rgba(210,111,146, 0.3)', 
                'rgba(241,156,112, 0.3)', 'rgba(243,159, 80, 0.3)', 'rgba(230,108, 70, 0.3)',  
                'rgba(209, 70, 68, 0.3)', 'rgba(184, 24, 64, 0.3)')

# Calculate values
ages <- rep(c(20, 30, 40, 50, 60, 70, 80, 90), each = 7)
pct  <- rep(c(3, 10, 25, 50, 75, 90, 97), times = 8)
zs   <- rep(c(qnorm(0.03), qnorm(0.10), qnorm(0.25), qnorm(0.50), qnorm(0.75), qnorm(0.90), qnorm(.97)), times = 8)

norms <- data.frame(ages, pct, zs) |> 
               mutate(people = ((ages - 45)*0.0024 + 1 + (zs*0.13)),
                      men    = ((ages - 45)*0.0034 + 1 + (zs*0.13)),
                      women  = ((ages - 45)*0.0017 + 1 + (zs*0.13))
                           )
norms_wider <- norms |> select(-zs) |> 
                    pivot_wider(names_from = pct, values_from = c(people, men, women)) |> 
                    mutate(pct00  = 0.60,
                           pct100 = 1.4)

#---------------- Build base Plotly objects once on start-up --------------------
people_plot <- plot_ly(norms_wider, x = ~ages, y = ~pct100, type = 'scatter', mode = 'lines',
                       showlegend = FALSE, line = list(color = 'transparent')) |> 
                    add_trace(y = ~people_97, type = 'scatter', mode = 'lines', 
                              fill = 'tonexty', fillcolor = use_rgba[8],
                              line = list(color = 'black', dash = 'dot'),
                              showlegend = FALSE, name = "97th percentile") |> 
                    add_trace(y = ~people_90, type = 'scatter', mode = 'lines', 
                              fill = 'tonexty', fillcolor = use_rgba[7],
                              line = list(color = 'black', dash = 'dot'),
                              showlegend = FALSE, name = "90th percentile") |> 
                    add_trace(y = ~people_75, type = 'scatter', mode = 'lines', 
                              fill = 'tonexty', fillcolor = use_rgba[6],
                              line = list(color = 'black', dash = 'dot'),
                              showlegend = FALSE, name = "75th percentile") |> 
                    add_trace(y = ~people_50, type = 'scatter', mode = 'lines', 
                              fill = 'tonexty', fillcolor = use_rgba[5],
                              line = list(color = 'black'),
                              showlegend = FALSE, name = "50th percentile") |> 
                    add_trace(y = ~people_25, type = 'scatter', mode = 'lines', 
                              fill = 'tonexty', fillcolor = use_rgba[4],
                              line = list(color = 'black', dash = 'dot'),
                              showlegend = FALSE, name = "25th percentile") |> 
                    add_trace(y = ~people_10, type = 'scatter', mode = 'lines', 
                              fill = 'tonexty', fillcolor = use_rgba[3],
                              line = list(color = 'black', dash = 'dot'),
                              showlegend = FALSE, name = "10th percentile") |> 
                    add_trace(y = ~people_3, type = 'scatter', mode = 'lines', 
                              fill = 'tonexty', fillcolor = use_rgba[2],
                              line = list(color = 'black', dash = 'dot'),
                              showlegend = FALSE, name = "3rd percentile") |> 
                    add_trace(y = ~pct00, type = 'scatter', mode = 'lines', 
                              fill = 'tonexty', fillcolor = use_rgba[1],
                              line = list(color = 'transparent'),
                              showlegend = FALSE) |> 
                    layout(title = "<b>Women and Men</b>",
                           yaxis = list(title = "<b>DunedinPACE aging score</b>",
                                        dtick = 0.1,
                                        tickformat = ".1f",
                                        gridcolor = 'darkgrey'),
                           xaxis = list(title = "<b>Chronological age (years)</b>",
                                        gridcolor = 'darkgrey'), 
                           # Add annotations for the reference lines
                           annotations = list(
                             list(x         = 1.01,            # a bit to the right of the plot
                                   xref      = "paper",
                                   y         = max(norms_wider$people_3, na.rm = TRUE),
                                   yref      = "y",
                                   text      = "<b>3rd percentile</b>",
                                   xanchor   = "left",
                                   showarrow = FALSE,
                                   font      = list(size = 12)),
                              list(x         = 1.01,            # a bit to the right of the plot
                                   xref      = "paper",
                                   y         = max(norms_wider$people_10, na.rm = TRUE),
                                   yref      = "y",
                                   text      = "<b>10th percentile</b>",
                                   xanchor   = "left",
                                   showarrow = FALSE,
                                   font      = list(size = 12)),
                              list(x         = 1.01,
                                   xref      = "paper",
                                   y         = max(norms_wider$people_25, na.rm = TRUE),
                                   yref      = "y",
                                   text      = "<b>25th percentile</b>",
                                   xanchor   = "left",
                                   showarrow = FALSE,
                                   font      = list(size = 12)),
                              list(x         = 1.01,
                                   xref      = "paper",
                                   y         = max(norms_wider$people_50, na.rm = TRUE),
                                   yref      = "y",
                                   text      = "<b>50th percentile</b>",
                                   xanchor   = "left",
                                   showarrow = FALSE,
                                   font      = list(size = 12)),
                              list(x         = 1.01,
                                   xref      = "paper",
                                   y         = max(norms_wider$people_75, na.rm = TRUE),
                                   yref      = "y",
                                   text      = "<b>75th percentile</b>",
                                   xanchor   = "left",
                                   showarrow = FALSE,
                                   font      = list(size = 12)),
                              list(x         = 1.01,
                                   xref      = "paper",
                                   y         = max(norms_wider$people_90, na.rm = TRUE),
                                   yref      = "y",
                                   text      = "<b>90th percentile</b>",
                                   xanchor   = "left",
                                   showarrow = FALSE,
                                   font      = list(size = 12)),
                              list(x         = 1.01,
                                   xref      = "paper",
                                   y         = max(norms_wider$people_97, na.rm = TRUE),
                                   yref      = "y",
                                   text      = "<b>97th percentile</b>",
                                   xanchor   = "left",
                                   showarrow = FALSE,
                                   font      = list(size = 12)
                                   )),
                    margin = list(r = 125)  # Add extra right margin for outside labels
               )
men_plot <- plot_ly(norms_wider, x = ~ages, y = ~pct100, type = 'scatter', mode = 'lines',
                       showlegend = FALSE, line = list(color = 'transparent')) |> 
                    add_trace(y = ~men_97, type = 'scatter', mode = 'lines', 
                              fill = 'tonexty', fillcolor = use_rgba[8],
                              line = list(color = 'black', dash = 'dot'),
                              showlegend = FALSE, name = "97th percentile") |> 
                    add_trace(y = ~men_90, type = 'scatter', mode = 'lines', 
                              fill = 'tonexty', fillcolor = use_rgba[7],
                              line = list(color = 'black', dash = 'dot'),
                              showlegend = FALSE, name = "90th percentile") |> 
                    add_trace(y = ~men_75, type = 'scatter', mode = 'lines', 
                              fill = 'tonexty', fillcolor = use_rgba[6],
                              line = list(color = 'black', dash = 'dot'),
                              showlegend = FALSE, name = "75th percentile") |> 
                    add_trace(y = ~men_50, type = 'scatter', mode = 'lines', 
                              fill = 'tonexty', fillcolor = use_rgba[5],
                              line = list(color = 'black'),
                              showlegend = FALSE, name = "50th percentile") |> 
                    add_trace(y = ~men_25, type = 'scatter', mode = 'lines', 
                              fill = 'tonexty', fillcolor = use_rgba[4],
                              line = list(color = 'black', dash = 'dot'),
                              showlegend = FALSE, name = "25th percentile") |> 
                    add_trace(y = ~men_10, type = 'scatter', mode = 'lines', 
                              fill = 'tonexty', fillcolor = use_rgba[3],
                              line = list(color = 'black', dash = 'dot'),
                              showlegend = FALSE, name = "10th percentile") |> 
                    add_trace(y = ~men_3, type = 'scatter', mode = 'lines', 
                              fill = 'tonexty', fillcolor = use_rgba[2],
                              line = list(color = 'black', dash = 'dot'),
                              showlegend = FALSE, name = "3rd percentile") |> 
                    add_trace(y = ~pct00, type = 'scatter', mode = 'lines', 
                              fill = 'tonexty', fillcolor = use_rgba[1],
                              line = list(color = 'transparent'),
                              showlegend = FALSE) |> 
                    layout(title = "<b>Men</b>",
                           yaxis = list(title = "<b>DunedinPACE aging score</b>",
                                        dtick = 0.1,
                                        tickformat = ".1f",
                                        gridcolor = 'darkgrey'),
                           xaxis = list(title = "<b>Chronological age (years)</b>",
                                        gridcolor = 'darkgrey'), 
                           # Add annotations for the reference lines
                           annotations = list(
                              list(x         = 1.01,            # a bit to the right of the plot
                                   xref      = "paper",
                                   y         = max(norms_wider$men_3, na.rm = TRUE),
                                   yref      = "y",
                                   text      = "<b>3rd percentile</b>",
                                   xanchor   = "left",
                                   showarrow = FALSE,
                                   font      = list(size = 12)),
                           list(x         = 1.01,            # a bit to the right of the plot
                                   xref      = "paper",
                                   y         = max(norms_wider$men_10, na.rm = TRUE),
                                   yref      = "y",
                                   text      = "<b>10th percentile</b>",
                                   xanchor   = "left",
                                   showarrow = FALSE,
                                   font      = list(size = 12)),
                              list(x         = 1.01,
                                   xref      = "paper",
                                   y         = max(norms_wider$men_25, na.rm = TRUE),
                                   yref      = "y",
                                   text      = "<b>25th percentile</b>",
                                   xanchor   = "left",
                                   showarrow = FALSE,
                                   font      = list(size = 12)),
                              list(x         = 1.01,
                                   xref      = "paper",
                                   y         = max(norms_wider$men_50, na.rm = TRUE),
                                   yref      = "y",
                                   text      = "<b>50th percentile</b>",
                                   xanchor   = "left",
                                   showarrow = FALSE,
                                   font      = list(size = 12)),
                              list(x         = 1.01,
                                   xref      = "paper",
                                   y         = max(norms_wider$men_75, na.rm = TRUE),
                                   yref      = "y",
                                   text      = "<b>75th percentile</b>",
                                   xanchor   = "left",
                                   showarrow = FALSE,
                                   font      = list(size = 12)),
                              list(x         = 1.01,
                                   xref      = "paper",
                                   y         = max(norms_wider$men_90, na.rm = TRUE),
                                   yref      = "y",
                                   text      = "<b>90th percentile</b>",
                                   xanchor   = "left",
                                   showarrow = FALSE,
                                   font      = list(size = 12)),
                             list(x         = 1.01,
                                   xref      = "paper",
                                   y         = max(norms_wider$men_97, na.rm = TRUE),
                                   yref      = "y",
                                   text      = "<b>97th percentile</b>",
                                   xanchor   = "left",
                                   showarrow = FALSE,
                                   font      = list(size = 12)
                                   )),
                    margin = list(r = 125)  # Add extra right margin for outside labels
               )
women_plot <- plot_ly(norms_wider, x = ~ages, y = ~pct100, type = 'scatter', mode = 'lines',
                      showlegend = FALSE, line = list(color = 'transparent')) |> 
                    add_trace(y = ~women_97, type = 'scatter', mode = 'lines', 
                              fill = 'tonexty', fillcolor = use_rgba[8],
                              line = list(color = 'black', dash = 'dot'),
                              showlegend = FALSE, name = "97th percentile") |> 
                    add_trace(y = ~women_90, type = 'scatter', mode = 'lines', 
                              fill = 'tonexty', fillcolor = use_rgba[7],
                              line = list(color = 'black', dash = 'dot'),
                              showlegend = FALSE, name = "90th percentile") |> 
                    add_trace(y = ~women_75, type = 'scatter', mode = 'lines', 
                              fill = 'tonexty', fillcolor = use_rgba[6],
                              line = list(color = 'black', dash = 'dot'),
                              showlegend = FALSE, name = "75th percentile") |> 
                    add_trace(y = ~women_50, type = 'scatter', mode = 'lines', 
                              fill = 'tonexty', fillcolor = use_rgba[5],
                              line = list(color = 'black'),
                              showlegend = FALSE, name = "50th percentile") |> 
                    add_trace(y = ~women_25, type = 'scatter', mode = 'lines', 
                              fill = 'tonexty', fillcolor = use_rgba[4],
                              line = list(color = 'black', dash = 'dot'),
                              showlegend = FALSE, name = "25th percentile") |> 
                    add_trace(y = ~women_10, type = 'scatter', mode = 'lines', 
                              fill = 'tonexty', fillcolor = use_rgba[3],
                              line = list(color = 'black', dash = 'dot'),
                              showlegend = FALSE, name = "10th percentile") |> 
                    add_trace(y = ~women_3, type = 'scatter', mode = 'lines', 
                              fill = 'tonexty', fillcolor = use_rgba[2],
                              line = list(color = 'black', dash = 'dot'),
                              showlegend = FALSE, name = "3rd percentile") |> 
                    add_trace(y = ~pct00, type = 'scatter', mode = 'lines', 
                              fill = 'tonexty', fillcolor = use_rgba[1],
                              line = list(color = 'transparent'),
                              showlegend = FALSE) |> 
                    layout(title = "<b>Women</b>",
                           yaxis = list(title = "<b>DunedinPACE aging score</b>",
                                        dtick = 0.1,
                                        tickformat = ".1f",
                                        gridcolor = 'darkgrey'),
                           xaxis = list(title = "<b>Chronological age (years)</b>",
                                        gridcolor = 'darkgrey'), 
                           # Add annotations for the reference lines
                           annotations = list(
                              list(x         = 1.01,            # a bit to the right of the plot
                                   xref      = "paper",
                                   y         = max(norms_wider$women_3, na.rm = TRUE),
                                   yref      = "y",
                                   text      = "<b>3rd percentile</b>",
                                   xanchor   = "left",
                                   showarrow = FALSE,
                                   font      = list(size = 12)),
                              list(x         = 1.01,            # a bit to the right of the plot
                                   xref      = "paper",
                                   y         = max(norms_wider$women_10, na.rm = TRUE),
                                   yref      = "y",
                                   text      = "<b>10th percentile</b>",
                                   xanchor   = "left",
                                   showarrow = FALSE,
                                   font      = list(size = 12)),
                              list(x         = 1.01,
                                   xref      = "paper",
                                   y         = max(norms_wider$women_25, na.rm = TRUE),
                                   yref      = "y",
                                   text      = "<b>25th percentile</b>",
                                   xanchor   = "left",
                                   showarrow = FALSE,
                                   font      = list(size = 12)),
                              list(x         = 1.01,
                                   xref      = "paper",
                                   y         = max(norms_wider$women_50, na.rm = TRUE),
                                   yref      = "y",
                                   text      = "<b>50th percentile</b>",
                                   xanchor   = "left",
                                   showarrow = FALSE,
                                   font      = list(size = 12)),
                              list(x         = 1.01,
                                   xref      = "paper",
                                   y         = max(norms_wider$women_75, na.rm = TRUE),
                                   yref      = "y",
                                   text      = "<b>75th percentile</b>",
                                   xanchor   = "left",
                                   showarrow = FALSE,
                                   font      = list(size = 12)),
                              list(x         = 1.01,
                                   xref      = "paper",
                                   y         = max(norms_wider$women_90, na.rm = TRUE),
                                   yref      = "y",
                                   text      = "<b>90th percentile</b>",
                                   xanchor   = "left",
                                   showarrow = FALSE,
                                   font      = list(size = 12)),
                              list(x         = 1.01,
                                   xref      = "paper",
                                   y         = max(norms_wider$women_97, na.rm = TRUE),
                                   yref      = "y",
                                   text      = "<b>97th percentile</b>",
                                   xanchor   = "left",
                                   showarrow = FALSE,
                                   font      = list(size = 12)
                                   )),
                    margin = list(r = 125)  # Add extra right margin for outside labels
               )



# ------------------ Make our app a function -----------------------------
DunedinPACE_app <- function(...){
     # ----------- SUBMODULES-------------
     source("R/app_login.R")

     # ----------- SOURCE SECURE UI BEHIND LOGON-------------
     # UI Definition
     ui_defined <- fluidPage(
          theme = shinytheme("sandstone"),
          titlePanel("DunedinPACE Normed Scores"),
          
          sidebarLayout(
                sidebarPanel(
                    selectInput("sex_choice", "Select norming group:",
                                  choices = c("---Choose One---", "Women", "Men", "Women and Men")),
                    numericInput("age",   "Enter your age (years):",       value = 45, min = 18, max = 99),
                    numericInput("score", "Enter your DunedinPACE score:", value = 1.00, step = 0.01),
                    p("This calculator was designed to norm DunedinPACE values derived from blood samples."),
                    actionButton("go",    "Calculate Percentile")
                ),
                
                mainPanel(
                          plotlyOutput("norm_plot", height = "500px"),
                          br(),
                          div(
                          style = "font-size: 18px; line-height: 1.4;",
                          textOutput("percentile_text")
                          )
                )
           )
      )
     # secure logon wrapper
     ui <- shinymanager::secure_app(ui_defined, enable_admin = FALSE, timeout=15*60)

     # ----------- APP SERVER-------------
     server <- function(input, output, session) {

          # -------------  Christoph Add: secure components --------------
          auth <- shinymanager::secure_server(
            check_credentials = make_checker(credentials)
            )
            
          # -------------  Calculated State Flag --------------
          calculated <- reactiveVal(FALSE)
          
          # -------------- Arm the state on button click ---------
          observeEvent(input$go, {
              req(input$sex_choice != "---Choose One---")
              calculated(TRUE)
          })
          
          # ------------- Reset whenever inputs change ------------------
          observeEvent(input$sex_choice, {
              calculated(FALSE)
              }, 
              ignoreInit = TRUE)
        
          # -------------  Compute percentile -----------------
          calc_percentile <- function(sex, age, score) {
              
              if (sex == "Women and Men") {
                    adj <- 0.0024
              }
              else if (sex == "Men") {
                    adj <- 0.0034
              }
              else if (sex == "Women") {
                    adj <- 0.0017
              }
              else{
                    return(NA_real_)
              }
              
              normpace <- (45 - age) * adj + score
              normpct  <- pnorm((normpace-1)/0.13)*100
              
              return(normpct)
          }
        
          # ---------- Base Plot only -----------------------------------------------
          base_plot <- reactive({
              req(input$sex_choice)
              
              switch(input$sex_choice,
                      "Women" = women_plot,
                      "Men"   = men_plot,
                      "Women and Men" = people_plot,
                      NULL # for "---Choose One---"
                      )
          })
          
          
          # ----------  When button is pressed, calculate outputs -------------------
          calc_event <- reactive({
              req(calculated())
              req(input$sex_choice != "---Choose One---")
                        
              list(
                    age   = input$age,
                    score = input$score,
                    pct   = calc_percentile(input$sex_choice, input$age, input$score)
              )
          })
        
        
          # ----------  Plot output with annotations --------------------------------
          output$norm_plot <- renderPlotly({
              p <- base_plot()
              req(p)
              
              if (!calculated()){
                    return(p)    
              }
              
              res <- calc_event()
        
              p |> layout(annotations = list(
                            list(x = res$age, 
                                  y = res$score,
                                  text = ~paste0(
                                            '<b></br>Your PACE Score: ', round(res$score, 2),
                                            '</br>Percentile for Age: ', round(res$pct, 1), "%</b>"),
                                  bgcolor = "white",
                                  font = list(color = "black",
                                              size = 14),
                                  showarrow  = TRUE,
                                  arrowcolor = "black",
                                  arrowsize  = 1.25,
                                  arrowhead = 7
                            )
                        )
                    )
              })
        
          # ----------  Text output with annotations --------------------------------
          output$percentile_text <- renderText({
              if (!calculated()) {
                    return("")
                    }
                  
              res <- calc_event()
              sex <- tolower(input$sex_choice)
              
              pct1 <- 100 - res$pct
              lcl  <- calc_percentile(input$sex_choice, res$age, res$score - 0.05)
              ucl  <- calc_percentile(input$sex_choice, res$age, res$score + 0.05)
        
              ord_pct <- ifelse(round(res$pct) %in% c(11, 12, 13), "th",
                          ifelse(round(res$pct) %% 10 == 1, "st",
                          ifelse(round(res$pct) %% 10 == 2, "nd",
                          ifelse(round(res$pct) %% 10 == 3, "rd", "th"))))
              ord_lcl <- ifelse(round(lcl) %in% c(11, 12, 13), "th",
                          ifelse(round(lcl) %% 10 == 1, "st",
                          ifelse(round(lcl) %% 10 == 2, "nd",
                          ifelse(round(lcl) %% 10 == 3, "rd", "th"))))
              ord_ucl <- ifelse(round(ucl) %in% c(11, 12, 13), "th",
                          ifelse(round(ucl) %% 10 == 1, "st",
                          ifelse(round(ucl) %% 10 == 2, "nd",
                          ifelse(round(ucl) %% 10 == 3, "rd", "th"))))
              
              glue(
                    "Compared to other {sex} who are {res$age} years old, ",
                    "your DunedinPACE aging score of {round(res$score, 2)} ",
                    "is in the {round(res$pct)}{ord_pct} percentile. ",
                    "This suggests that {round(res$pct)}% of {sex} your age ",
                    "have slower aging scores and {round(pct1)}% have faster aging scores. ",
                    "Given the reliability of the DunedinPACE measure, ",
                    "we can be 95% confident that your aging score lies between ",
                    "{round(res$score - 0.05, 2)} and {round(res$score + 0.05, 2)}, ",
                    "which would correspond to the {round(lcl)}{ord_lcl} and {round(ucl)}{ord_ucl} percentiles ",
                    "for {sex} your age. ",
                    "There are methods that might be able to slow your aging, ",
                    "including XXX and YYY. See [publication] to learn more."
                    )
          })
            
        }
     
     # ----------- RUN SHINY APP-------------
     shinyApp(ui, server)
}


DunedinPACE_app()
