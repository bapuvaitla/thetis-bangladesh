  function (input, output) {
    
    totals_filtered <- reactive({
      yearView <- seq(input$yearView[1], input$yearView[2])
      eco_scen_long_shiny <- read.csv("eco_scen_long_shiny.csv")
      eco_scen_long_shiny %>%
        filter(year %in% yearView &
               rcp %in% input$climate &
               sector %in% input$sector & 
               agg == input$agg )
    })
    
    groups_filtered <- reactive({
      yearView <- seq(input$yearView[1], input$yearView[2])
      eco_scen_long_shiny <- read.csv("eco_scen_long_shiny.csv")
      eco_scen_long_shiny %>%
        filter(year %in% yearView &
               rcp %in% input$climate &
               sector %in% input$sector &
               fish_group %in% input$fish_group
        )
    })
    
    market_totals_filtered <- reactive({
      yearView_market <- seq(input$yearView_market[1], input$yearView_market[2])
      market_scen_shiny <- read.csv("market_scen_shiny.csv")
      market_scen_shiny %>%
        filter(year %in% yearView_market &
               stages == input$stages &
               climate %in% input$climate_market &
               region %in% input$region_market &
               aqua_prod %in% input$aqua_prod &
               income %in% input$income &
               inland %in% input$inland &
               aqua_elas %in% input$aqua_elas &
               non_fish_price %in% input$non_fish_price &
               agg == input$agg_market
                 )
    })
    
    market_groups_filtered <- reactive({
      yearView_market <- seq(input$yearView_market[1], input$yearView_market[2])
      market_scen_shiny <- read.csv("market_scen_shiny.csv")
      market_scen_shiny %>%
        filter(year %in% yearView_market &
                 stages == input$stages &
                 climate %in% input$climate_market &
                 region %in% input$region_market &
                 aqua_prod %in% input$aqua_prod &
                 income %in% input$income &
                 inland %in% input$inland &
                 aqua_elas %in% input$aqua_elas &
                 non_fish_price %in% input$non_fish_price &
                 fish_group %in% input$fish_group_market
                )
    })
    
    fd_nutrient_intake_filtered <- reactive({
      yearView_nutrition <- seq(input$yearView_nut[1], input$yearView_nut[2])
      nutrition_scen_shiny <- read.csv("fish_nutrient_intake_shiny.csv")
      nutrition_scen_shiny %>%
        filter(year_nut %in% yearView_nutrition &
                 region_nut %in% input$region_nut &
                 nutrient == input$nutrient_list &
                 stages_nut == input$stages_nut &
                 climate_nut %in% input$climate_nut &
                 aqua_prod_nut %in% input$aqua_prod_nut &
                 income_nut == input$income_nut &
                 inland_nut == input$inland_nut &
                 aqua_elas_nut == input$aqua_elas_nut &
                 non_fish_price_nut == input$non_fish_price_nut
                  )
    })
    
    output$eco_totals_plot <- renderPlot({
      ggplot(totals_filtered()) +
        geom_line(aes(x = year, 
                      y = value, 
                      color = sector,
                      size = rcp)
                  ) + 
        scale_x_continuous(breaks = seq(1950, 2100, by=10), name = "Year") +
        scale_y_continuous(name = "Catch (MT)") +
        scale_size_discrete(range = c(0.4, 0.8))
      
    })    
    
    output$eco_totals_table <- DT::renderDataTable({
      totals_filtered()
      }, colnames = c("Fish group", "RCP", "Sector",
                      "Year", "Harvest (MT)", "Aggregation"))
    
    observeEvent(input$toggle, {
      toggle("eco_totals_table")
    })
    
    # Data to download with download button
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("TableDownload", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(totals_filtered(), file, row.names = FALSE)
      }
    )
    
    
    output$eco_group_plot <- renderPlot({
      ggplot(groups_filtered()) + 
        geom_line(aes(x = year,
                      y = value,
                      color = fish_group,
                      linetype = sector,
                      size = rcp)) +
        scale_x_continuous(breaks = seq(1950, 2100, by=10), name = "Year") +
        scale_y_continuous(name = "Catch (MT)") +
        scale_size_discrete(range = c(0.4, 0.8))
    })
    
    output$eco_group_table <- DT::renderDataTable({
      groups_filtered()
      }, colnames = c("Fish group", "RCP", "Sector",
                      "Year", "Harvest (MT)", "Aggregation"))
    
    observeEvent(input$toggle2, {
      toggle("eco_group_table")
    })
    
    # Data to download with download button
    output$downloadData2 <- downloadHandler(
      filename = function() {
        paste("TableDownload", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(groups_filtered(), file, row.names = FALSE)
      }
    )
    
    output$market_totals_plot <- renderPlot({ 
      ggplot(market_totals_filtered()) +
        geom_line(aes(x = year, 
                      y = consumption_pc,
                      color = region,
                      size = climate,
                      linetype = aqua_prod
                      )
        ) + 
        scale_x_continuous(breaks = seq(2010, 2050, by=5), name = "Year") +
        scale_y_continuous(name = "Consumption (kg/person/year)") +
        scale_size_discrete(range = c(0.4, 0.8))
    })    
    
    output$market_totals_table <- DT::renderDataTable({
      market_totals_filtered()
    }, colnames = c("Scenario name", "Region", "Fish group",
                    "Model type", "Aquaculture productivity", 
                    "Income growth", "Inland capture fisheries",
                    "Aquaculture demand elasticity",
                    "Non-fish food prices",
                    "RCP", "Year", "Consumption (kg/pc/yr)", "Aggregation"
                    ))
    
    observeEvent(input$toggle3, {
      toggle("market_totals_table")
    })
    
    # Data to download with download button
    output$downloadData3 <- downloadHandler(
      filename = function() {
        paste("TableDownload", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(market_totals_filtered(), file, row.names = FALSE)
      }
    )
    
    output$market_group_plot <- renderPlot({
      ggplot(market_groups_filtered()) + 
        geom_line(aes(x = year,
                      y = consumption_pc,
                      color = fish_group,
                      linetype = region,
                      size = climate)) +
        scale_x_continuous(breaks = seq(2010, 2050, by=5), name = "Year") +
        scale_y_continuous(name = "Consumption (kg/person/year)") +
        scale_size_discrete(range = c(0.4, 0.8))
    })
    
    output$market_group_table <- DT::renderDataTable({
      market_groups_filtered()
    }, colnames = c("Scenario name", "Region", "Fish group",
                    "Model type", "Aquaculture productivity", 
                    "Income growth", "Inland capture fisheries",
                    "Aquaculture demand elasticity",
                    "Non-fish food prices",
                    "RCP", "Year", "Consumption (kg/pc/yr)", "Aggregation"))
    
    observeEvent(input$toggle4, {
      toggle("market_group_table")
    })
    
    # Data to download with download button
    output$downloadData4 <- downloadHandler(
      filename = function() {
        paste("TableDownload", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(market_groups_filtered(), file, row.names = FALSE)
      }
    )
    
    output$fd_nutrient_intake_plot <- renderPlot({
      ggplot(fd_nutrient_intake_filtered()) +
        geom_line(aes(x = year_nut,
                      y = consumption_pc_nut,
                      color = region_nut,
                      linetype = aqua_prod_nut,
                      size = climate_nut)) +
        scale_x_continuous(breaks=seq(2010,2050, by=5), name = "Year") +
        scale_y_continuous(name = "Consumption (unit/person/day)") +
        scale_size_discrete(range = c(0.4, 0.8))
    })
    
    output$fd_nutrient_intake_table <- DT::renderDataTable({
      fd_nutrient_intake_filtered()
    })
    
    observeEvent(input$toggle5, {
      toggle("fd_nutrient_intake_table")
    })
    
    # Data to download with download button
    output$downloadData5 <- downloadHandler(
      filename = function() {
        paste("TableDownload", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(fd_nutrient_intake_filtered(), file, row.names = FALSE)
      }
    )
    
  }
  