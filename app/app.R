
library(shiny)
library(ggplot2)
library(plotly)
library(bslib)
library(DT)
library(dplyr)
library(tidyr)
library(MASS)



# At the very top of app.R before ui and server
ivory_coast_summary                        <- readRDS("ivory_coast_summary.rds")
ivory_coast_timeline                       <- readRDS("ivory_coast_timeline.rds")
cuumulative_ivorycoast_xgplot              <- readRDS("cuumulative_ivorycoast_xgplot.rds")
manager_comparison_shotmap                 <- readRDS("manager_comparison_shotmap.rds")
ivorycoast_managers_shotsconceded_xgagainst <- readRDS("ivorycoast_managers_shotsconceded_xgagainst.rds")
shotquality_vs_quantity                    <- readRDS("shotquality_vs_quantity.rds")
passing_heatmap                            <- readRDS("passing_heatmap.rds")
defensive_actions_manager_comparison       <- readRDS("defensive_actions_manager_comparison.rds")
dribble_manager_comparison_plot            <- readRDS("dribble_manager_comparison_plot.rds")
final_shotmap                              <- readRDS("final_shotmap.rds")
combined_goals_and_assists                 <- readRDS("combined_goals_and_assists.rds")
top5_dribble_heatmap                       <- readRDS("top5_dribble_heatmap.rds")
defensive_stats                            <- readRDS("defensive_stats.rds")
top5_passers_heatmap                       <- readRDS("top5_passers_heatmap.rds")
attacking_metrics                          <- readRDS("attacking_metrics.rds")
deserved_winner_tournament                 <- readRDS("deserved_winner_tournament.rds")

ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "darkly"),
  style = "background:#12131a;",
  
  tags$script(HTML("
    Shiny.addCustomMessageHandler('setPage', function(p) {
      Shiny.setInputValue('page', p, {priority: 'event'});
    });
  ")),
  
  # ── Home ──────────────────────────────────────────────────────────────────
  conditionalPanel(
    condition = "input.page == 'home' || input.page == undefined",
    h1("AFCON 2023 — how did Ivory Coast win AFCON 2023 despite sacking their manager during the tournament, and does the data back them up?",
       style = "color:white; text-align:center; padding:30px;"),
    fluidRow(
      column(3, actionButton("btn_journey",  "🏆 Tournament Journey",        width = "100%",
                             style = "background:#12421a; color:white; height:150px; font-size:16px; border:none; border-radius:10px;")),
      column(3, actionButton("btn_managers", "👤 Before & After Manager Sacking", width = "100%",
                             style = "background:#5a2000; color:white; height:150px; font-size:16px; border:none; border-radius:10px;")),
      column(3, actionButton("btn_players",  "⭐ Key Players",               width = "100%",
                             style = "background:#1a1a4e; color:white; height:150px; font-size:16px; border:none; border-radius:10px;")),
      column(3, actionButton("btn_deserved", "📊 Was It Deserved?",          width = "100%",
                             style = "background:#4a1a00; color:white; height:150px; font-size:16px; border:none; border-radius:10px;"))
    )
  ),
  
  # ── Tournament Journey ────────────────────────────────────────────────────
  conditionalPanel(
    condition = "input.page == 'journey'",
    actionButton("back1", "← Back", style = "margin:10px; background:#333; color:white; border:none;"),
    h2("Tournament Journey", style = "color:white; text-align:center;"),
    navset_pill(
      nav_panel("Overall Stats",
                div(style = "background:white; padding:10px; border-radius:8px; margin:10px;",
                    tableOutput("ivory_coast_summary"))),
      nav_panel("Cumulative xG",
                plotlyOutput("cuumulative_ivorycoast_xgplot")),
      nav_panel("Match Results",
                div(style = "background:white; padding:10px; border-radius:8px; margin:10px;",
                    tableOutput("ivory_coast_timeline")))
    )
  ),
  
  # ── Before & After Manager ────────────────────────────────────────────────
  conditionalPanel(
    condition = "input.page == 'managers'",
    actionButton("back2", "← Back", style = "margin:10px; background:#333; color:white; border:none;"),
    h2("Before & After Manager Sacking", style = "color:white; text-align:center;"),
    navset_pill(
      nav_panel("Shot Map",
                plotlyOutput("manager_shotmap")),
      nav_panel("Shots Conceded & xG Against",
                DTOutput("shots_conceded_table")),
      nav_panel("Shot Quality vs Quantity",
                DTOutput("shot_quality_table")),
      nav_panel("Passing Heatmap",
                plotOutput("manager_pass_heatmap")),
      nav_panel("Defensive Stats",
                DTOutput("manager_defensive_table")),
      nav_panel("Dribble Areas",
                plotOutput("manager_dribble_heatmap"))
    )
  ),
  
  # ── Key Players ───────────────────────────────────────────────────────────
  conditionalPanel(
    condition = "input.page == 'players'",
    actionButton("back3", "← Back", style = "margin:10px; background:#333; color:white; border:none;"),
    h2("Key Players", style = "color:white; text-align:center;"),
    navset_pill(
      nav_panel("Shot Map",
                plotOutput("top5_shotmap", height = "600px")),
      nav_panel("Goals & Assists",
                DTOutput("goals_assists_table")),
      nav_panel("Top 5 Dribblers",
                plotOutput("top5_dribble_heatmap")),
      nav_panel("Defensive Stats",
                DTOutput("player_defensive_table")),
      nav_panel("Top 5 Passers",
                plotOutput("top5_pass_heatmap")),
      nav_panel("Attacking Stats",
                DTOutput("attacking_table"))
    )
  ),
  
  # ── Was It Deserved ───────────────────────────────────────────────────────
  conditionalPanel(
    condition = "input.page == 'deserved'",
    actionButton("back4", "← Back", style = "margin:10px; background:#333; color:white; border:none;"),
    h2("Was It Deserved?", style = "color:white; text-align:center;"),
    DTOutput("deserved_table")
  )
)

server <- function(input, output, session) {
  
  # ── Navigation ─────────────────────────────────────────────────────────────
  page <- reactiveVal("home")
  
  observeEvent(input$btn_journey, page("journey"))
  observeEvent(input$btn_managers, page("managers"))
  observeEvent(input$btn_players, page("players"))
  observeEvent(input$btn_deserved, page("deserved"))
  observeEvent(input$back1, page("home"))
  observeEvent(input$back2, page("home"))
  observeEvent(input$back3, page("home"))
  observeEvent(input$back4, page("home"))
  
  observe({
    session$sendCustomMessage("setPage", page())
  })
  
  # ── Tournament Journey ─────────────────────────────────────────────────────
  output$ivory_coast_summary <- renderTable({ ivory_coast_summary })
  output$cuumulative_ivorycoast_xgplot <- renderPlotly({ ggplotly(cuumulative_ivorycoast_xgplot, tooltip = "text") })
  output$ivory_coast_timeline <- renderTable({ ivory_coast_timeline })
  
  # ── Manager Comparison ─────────────────────────────────────────────────────
  output$manager_shotmap <- renderPlotly({ ggplotly(manager_comparison_shotmap, tooltip = "text") })
  output$shots_conceded_table <- renderDT({
    ivorycoast_managers_shotsconceded_xgagainst
  })
  output$shot_quality_table <- renderDT({ shotquality_vs_quantity },
                                        options = list(pageLength = 10,
                                                       order = list(list(1, "desc"))))
  output$manager_pass_heatmap <- renderPlot({ passing_heatmap })
  output$manager_defensive_table <- renderDT({ defensive_actions_manager_comparison },
                                             options = list(pageLength = 10,
                                                            order = list(list(1, "desc"))))
  output$manager_dribble_heatmap <- renderPlot({ dribble_manager_comparison_plot })
  
  # ── Key Players ────────────────────────────────────────────────────────────
  output$top5_shotmap <- renderPlot({ final_shotmap })
  output$goals_assists_table <- renderDT({ combined_goals_and_assists },
                                         options = list(pageLength = 10,
                                                        order = list(list(1, "desc"))))
  output$top5_dribble_heatmap   <- renderPlot({ top5_dribble_heatmap })
  output$player_defensive_table <- renderDT({ defensive_stats },
                                            options = list(pageLength = 10,
                                                           order = list(list(1, "desc"))))
  output$top5_pass_heatmap <- renderPlot({ top5_passers_heatmap })
  output$attacking_table <- renderDT({ attacking_metrics },
                                     options = list(pageLength = 10,
                                                    order = list(list(1, "desc"))))
  
  # ── Was It Deserved ────────────────────────────────────────────────────────
  output$deserved_table <- renderDT({ deserved_winner_tournament },
                                    options = list(pageLength = 25,
                                                   order = list(list(
                                                     ncol(deserved_winner_tournament) - 1,
                                                     "desc"))))
}

shinyApp(ui, server)
  