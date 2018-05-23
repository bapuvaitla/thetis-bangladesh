  library(shiny)
  library(shinyjs)
  library(tidyverse)
  library(ggplot2)
  library(DT)
  library(shinythemes)

  eco_scen_long_shiny <- read.csv("eco_scen_long_shiny.csv")
  market_scen_shiny <- read.csv("market_scen_shiny.csv")
  fish_nutrition_intake_shiny <- read.csv("fish_nutrient_intake_shiny.csv")
  
    
  fluidPage(
    # For table toggle button
    useShinyjs(),
    theme = shinytheme("cosmo"),
    titlePanel("THETIS modeling"),
    h1("Bangladesh"),
    sidebarLayout(
      sidebarPanel(
        radioButtons("model", "Model", 
                     choices = c("Ecological", "Market", "Nutritional")),
        # Ecological model       
        conditionalPanel("input.model == 'Ecological'",
                  # Years of ecological model extend to 2100, others to 2050
                         sliderInput("yearView", "Years to view",
                                     min = 1950, max = 2100, 
                                     value = c(1950, 2100),
                                     dragRange = TRUE, sep=""),
                         checkboxGroupInput("climate", "RCP scenario",
                                      selected = "RCP 2.6",
                                      choices = c("RCP 2.6", "RCP 8.5")),
                         checkboxGroupInput("sector", "Sector",
                                      selected = "All sectors",
                                      choices = c("All sectors", 
                                                  "Subsistence", 
                                                  "Artisanal", 
                                                  "Industrial")),
                         radioButtons("agg", "Aggregation",
                                      selected = "Totals",
                                      choices = c("Totals", 
                                                  "By fish group"))
        ),
        conditionalPanel("input.agg == 'By fish group'",
                         checkboxGroupInput("fish_group", "Fish Groups",
                                      selected = "All fish groups",
                                      choices =  c(
      "All fish groups",  "Acetes shrimps", "Banana prawn", "Barramundi", 
      "Bartail flathead", "Black pomfret", "Bombay duck",
      "Chinese silver pomfret", "Commercial shrimps and prawns", 
      "Cutlassfishes", "Drums, croakers", "Fourfinger threadfin", 
      "Garment anchovies", "Giant tiger prawn","Green tiger prawn", 
      "Grunters", "Herrings, sardines, menhadens", "Hilsa shad", 
      "Indian mackerel", "Indian white prawn", "Indo-Pacific king mackerel", 
      "Indo-Pacific prawns", "Indo-Pacific swamp crab", "Jacks, pompanos", 
      "Jarbua terapon", "Johns snapper", "Kuruma prawn", 
      "Mullets, grey mullets", "Narrowbarred Spanish mackerel", "Scats", 
      "Sea catfishes, coblers", "Sepia cuttlefishes", "Silver biddies", 
      "Silver pomfret", "Silver sillago", "Slipmouths, ponyfishes", 
      "Smelt-whitings", "Speckled shrimp", "Stargazers", "Threadfins",  
      "Thryssas", "Toli shad", "Torpedo scad", "Upeneid goatfishes", 
      "Whitefin wolf-herring", "Yellowfin seabream", "Others"))
                         ),
      # Market model
        conditionalPanel("input.model == 'Market'",
                         sliderInput("yearView_market", "Years to view",
                                     min = 2010, max = 2050, 
                                     value = c(2010, 2050),
                                     dragRange = T, sep = ""
                                     ),
                         radioButtons("stages", "Model type",
                                      selected = "Three-stage demand",
                                      choices = c("Three-stage demand",
                                                  "Fish demand only")),
                         checkboxGroupInput("climate_market", "RCP scenario",
                                            selected = "RCP 2.6",
                                            choices = c("RCP 2.6", "RCP 8.5")),
                         checkboxGroupInput("region_market", "Region",
                                      selected = "Total",
                                      choices = c("Total", "Rural", "Urban")),
                         checkboxGroupInput("aqua_prod", "Aquaculture trend",
                                            selected = "Low productivity trend",
                                            choices = 
                                              c("Low productivity trend",
                                                "High productivity trend")),
                         radioButtons("income", 
                                            "Per capita income growth",
                                            selected = "Low per capita growth",
                                            choices = 
                                              c("Low per capita growth",
                                                "High per capita growth")),
                         radioButtons("inland", "Inland capture trend",
                                            selected = "Same as present",
                                            choices = 
                                              c("Same as present",
                                                "Decreased from present")),
                         radioButtons("aqua_elas", 
                                            "Aquaculture demand elasticity",
                                            selected = "Same as present",
                                            choices = c("Same as present",
                                                        "Less elastic demand")),
                         radioButtons("non_fish_price", 
                                            "Price of non-fish foods",
                                            selected = "Same as present",
                                            choices = c("Same as present",
                                                        "Increased price")),
                         radioButtons("agg_market", "Aggregation",
                                      selected = "Totals",
                                      choices = c("Totals", 
                                                  "By fish group"))
        ),
          conditionalPanel("input.agg_market == 'By fish group'",
                           checkboxGroupInput("fish_group_market", 
                                              "Fish Groups",
                                              selected = "All fish groups",
                                              choices =  c("All fish groups",
                          "Big shrimp", "Climbing perch", "Dried fish",
                          "Exotic and other carps", "High value marine",
                          "Hilsha", "Indian major carps", "Low value marine",
                          "Pangas and big catfish", "Small indigenous species",
                          "Small shrimp", "Tilapia and barbs", 
                          "Other freshwater fish", 
                          "Other live fish and snakehead")
                           )
          ),
      # Nutritional model
      conditionalPanel("input.model == 'Nutritional'",
                       radioButtons("analysis", "Analysis type",
                                    selected = "Fish-derived nutrient intake",
                                    choices = c(
                                      "Fish-derived nutrient intake",
                                      "Nutrient deficiency prevalence",
                                      "Total deficient population"
                                    )),
                       sliderInput("yearView_nut", "Years to view",
                                   min = 2010, max = 2050, 
                                   value = c(2010, 2050),
                                   dragRange = T, sep = ""
                       ),
                       radioButtons("nutrient_list", 
                                    "Fish-derived nutrient intake",
                                    selected = "Protein",
                                    choices = c(
                                      "Calories", "Carbohydrates",
                                      "Protein", "Fat", 
                                      "Monounsaturated fatty acids",
                                      "Polyunsaturated fatty acids",
                                      "Saturated fatty acids",
                                      "Dietary fiber", "Sodium",
                                      "Calcium", "Copper", "Iron", "Magnesium", 
                                      "Phosphorus", "Potassium", "Zinc",
                                      "Folate", "Niacin", "Riboflavin", 
                                      "Thiamin", "Vitamin A", "Vitamin B6",
                                      "Vitamin C"                                          
                                                )
                                    ),
                       radioButtons("stages_nut", "Model type",
                                    selected = "Three-stage demand",
                                    choices = c("Three-stage demand", 
                                                "Fish demand only")),
                       checkboxGroupInput("climate_nut", "RCP scenario",
                                          selected = "RCP 2.6",
                                          choices = c("RCP 2.6", "RCP 8.5")),
                       checkboxGroupInput("region_nut", "Region",
                                          selected = "Total",
                                          choices = c("Total", "Rural", "Urban")),
                       checkboxGroupInput("aqua_prod_nut", "Aquaculture trend",
                                          selected = "Low productivity trend",
                                          choices = 
                                            c("Low productivity trend",
                                              "High productivity trend")),
                       radioButtons("income_nut", 
                                    "Per capita income growth",
                                    selected = "Low per capita growth",
                                    choices = 
                                      c("Low per capita growth",
                                        "High per capita growth")),
                       radioButtons("inland_nut", "Inland capture trend",
                                    selected = "Same as present",
                                    choices = 
                                      c("Same as present",
                                        "Decreased from present")),
                       radioButtons("aqua_elas_nut", 
                                    "Aquaculture demand elasticity",
                                    selected = "Same as present",
                                    choices = c("Same as present",
                                                "Less elastic demand")),
                       radioButtons("non_fish_price_nut", 
                                    "Price of non-fish foods",
                                    selected = "Same as present",
                                    choices = c("Same as present",
                                                "Increased price"))
      )),
    mainPanel(
      conditionalPanel(
        # Ecological model, only appears for total fish harvest
        "input.model == 'Ecological' && input.agg == 'Totals'",
        plotOutput("eco_totals_plot"),
        br(),
        br(),
        actionButton("toggle", "Hide/show table"),
        br(),
        br(),
        DT::dataTableOutput("eco_totals_table"),
        # Download button
        downloadButton("downloadData", "Download")
      ),
      conditionalPanel(
        # Ecological model, only appears for individual fish groups
        "input.model == 'Ecological' && input.agg == 'By fish group'",
        plotOutput("eco_group_plot"),
        br(),
        br(),
        actionButton("toggle2", "Hide/show table"),
                br(),
        br(),
        DT::dataTableOutput("eco_group_table"),
        # Download button
        downloadButton("downloadData2", "Download")
      ),
      conditionalPanel(
        # Market model, only appears for market totals results
        "input.model == 'Market' && input.agg_market == 'Totals'",
        plotOutput("market_totals_plot"),
        br(),
        br(),
        actionButton("toggle3", "Hide/show table"),
        br(),
        br(),
        DT::dataTableOutput("market_totals_table"),
        # Download button
        downloadButton("downloadData3", "Download")
      ),
      conditionalPanel(
        # Market model, only appears for market fish group results
        "input.model == 'Market' && input.agg_market == 'By fish group'",
        plotOutput("market_group_plot"),
        br(),
        br(),
        actionButton("toggle4", "Hide/show table"),
        br(),
        br(),
        DT::dataTableOutput("market_group_table"),
        # Download button
        downloadButton("downloadData4", "Download")
      ),
      conditionalPanel(
        # Nutritional model
        "input.model == 'Nutritional' && input.analysis == 
        'Fish-derived nutrient intake'",
        plotOutput("fd_nutrient_intake_plot"),
        br(),
        br(),
        actionButton("toggle5", "Hide/show table"),
        br(),
        br(),
        DT::dataTableOutput("fd_nutrient_intake_table"),
        # Download button
        downloadButton("downloadData5", "Download")
      )
    )
  )
  )
 