  library(plyr)
  library(tidyverse)
  library(reshape2)
  
  # For block-diagonal matrix manipulation
  library(magic) 

  # Disable scientific notation
  options(scipen=99)    

  #### PRELIMINARIES-----------------------------------------------------------

  # Set root directory
  setwd("/Users/bapu/Documents/work/commissions/harvard/fisheries/bangladesh/") 
  
  # Function to extract all sheets in xlsx as list of tibbles 
  read_excel_allsheets <- function(filename) {
    sheets <- readxl::excel_sheets(filename)
    x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
    names(x) <- sheets
    x 
  }  
  
  # Ecological external tables----
  
    ## Read in ecological output tables (raw, no totals)
  eco_scen_raw <- read.csv("./ecology/bangladesh-eco.csv", stringsAsFactors = T)
  
      ### Calculate totals
        #### By RCP
  eco_scen_totals_rcp <- subset(eco_scen_raw, 
                                select = -c(fish_group, sector)) %>%
                          group_by(rcp) %>%
                          summarise_all(funs(sum), na.rm = TRUE)
  eco_scen_totals_rcp$sector <- "All sectors"
  eco_scen_totals_rcp$fish_group <- "All fish groups"
  
        #### By sector
  eco_scen_totals_sector <- subset(eco_scen_raw,
                                   select = -fish_group) %>%
                              group_by(sector, rcp) %>%
                              summarise_all(funs(sum), na.rm = TRUE)
  eco_scen_totals_sector$fish_group <- "All fish groups"
  
        #### By fish_group
  eco_scen_totals_fish_group <- subset(eco_scen_raw,
                                   select = -sector) %>%
                                  group_by(fish_group, rcp) %>%
                                  summarise_all(funs(sum), na.rm = TRUE)
  eco_scen_totals_fish_group$sector <- "All sectors"
  
      ### Row bind data tables
  eco_scen <- bind_rows(
                    eco_scen_raw, eco_scen_totals_rcp, 
                    eco_scen_totals_sector, eco_scen_totals_fish_group)

      ### Make ecological tables long
  eco_scen_long <- melt(eco_scen, id.vars=c("fish_group", "rcp", "sector"))
      
      ### Remove "X"s from year variable
  eco_scen_long$variable <- gsub("X", "", eco_scen_long$variable)
      
      ### Correct classes of variables (and rename where needed)
  eco_scen_long$fish_group <- as.factor(eco_scen_long$fish_group)
  eco_scen_long$sector <- as.factor(eco_scen_long$sector)
  names(eco_scen_long)[names(eco_scen_long) == "variable"] <- "year"
  eco_scen_long$year <- as.numeric(eco_scen_long$year)
      
      ### Add fish group aggregation variable, make factor
  eco_scen_long$agg <- ifelse(eco_scen_long$fish_group=="All fish groups",
                          "Totals",
                          "By fish group")
  eco_scen_long$agg <- as.factor(eco_scen_long$agg)
  
      ### Save in R Shiny folder
  write.csv(eco_scen_long, "./nutrition/analysis/thetis-bangladesh/visualization/eco_scen_long_shiny.csv", 
            row.names = F)
  
  # Market external tables----
  
    ## Read in market output tables; xlsx must have only data, no metadata
  market_scen <- read_excel_allsheets("./market/bangladesh-market.xlsx")
    
      ### Add scenario names & fields to prep for long file
  market_scen_name <- Map(cbind, 
                          market_scen, 
                          scenario = names(market_scen))
  
      ### Bind all list elements into a single data frame
  market_scen_unified <- rbind.fill(market_scen_name)
  
      ### Read in scenario descriptions 
  scen_desc <- read.csv("./nutrition/aux/scenarios.csv") 
  
      ### Match to scenario descriptions file
  market_scen_unified <- merge(market_scen_unified, 
                                       scen_desc, 
                                       by.x="scenario")
  
      ### Convert character variables to factors
  market_scen_unified$region <- as.factor(market_scen_unified$region)
  market_scen_unified$fish_group <- as.factor(market_scen_unified$fish_group)
  
      ### Make long
  market_scen_long <- melt(market_scen_unified, id.vars = 
                             c("scenario", "region", "fish_group",
                               "stages", "aqua_prod", "income", 
                               "inland", "aqua_elas", "non_fish_price",
                               "climate"))
  
      ### Rename and strip "Y"s from year variable, convert to numeric
  names(market_scen_long)[names(market_scen_long) == "variable"] <- "year"
  market_scen_long$year <- gsub("Y", "", market_scen_long$year)
  market_scen_long$year <- as.numeric(market_scen_long$year)
  
      ### Rename harvest variable
  names(market_scen_long)[names(market_scen_long) == "value"] <- 
    "consumption_pc"
  
      ### Proper case fields 
        #### Function to capitalize first letter
  capFirst <- function(s) {
    paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "")
  }
        #### Capitalize first letter
  market_scen_long$region <- capFirst(market_scen_long$region)
      
      ### Rename fish group names
  market_scen_long$fish_group <- gsub("imc","Indian major carps", 
                                  market_scen_long$fish_group, fixed = TRUE)
  market_scen_long$fish_group <- gsub("oc","Exotic and other carps", 
                                      market_scen_long$fish_group, fixed = TRUE)
  market_scen_long$fish_group <- gsub("tilb","Tilapia and barbs", 
                                      market_scen_long$fish_group, fixed = TRUE)
  market_scen_long$fish_group <- gsub("koi","Climbing perch", 
                                      market_scen_long$fish_group, fixed = TRUE)
  market_scen_long$fish_group <- gsub("pcat","Pangas and big catfish", 
                                      market_scen_long$fish_group, fixed = TRUE)
  market_scen_long$fish_group <- gsub("ofwf","Other freshwater fish", 
                                      market_scen_long$fish_group, fixed = TRUE)
  market_scen_long$fish_group <- gsub("lfsh","Other live fish and snakehead", 
                                      market_scen_long$fish_group, fixed = TRUE)
  market_scen_long$fish_group <- gsub("sis","Small indigenous species", 
                                      market_scen_long$fish_group, fixed = TRUE)
  market_scen_long$fish_group <- gsub("hilsha","Hilsha", 
                                      market_scen_long$fish_group, fixed = TRUE)
  market_scen_long$fish_group <- gsub("sshrimp","Small shrimp", 
                                      market_scen_long$fish_group, fixed = TRUE)
  market_scen_long$fish_group <- gsub("bshrimp","Big shrimp", 
                                      market_scen_long$fish_group, fixed = TRUE)
  market_scen_long$fish_group <- gsub("hvm","High value marine", 
                                      market_scen_long$fish_group, fixed = TRUE)
  market_scen_long$fish_group <- gsub("lvm","Low value marine", 
                                      market_scen_long$fish_group, fixed = TRUE)
  market_scen_long$fish_group <- gsub("dried","Dried fish", 
                                      market_scen_long$fish_group, fixed = TRUE)
  
      ### Generate totals columns
  market_scen_long_totals <- market_scen_long %>%
    group_by(scenario, region, stages, aqua_prod, 
             income, inland, aqua_elas, non_fish_price, climate, year) %>%
    summarise_at("consumption_pc", sum, na.rm = TRUE)
  
      ### Create all fish group column
  market_scen_long_totals <- cbind(
    market_scen_long_totals[, c("scenario", "region")],
                                   fish_group = "All fish groups",
                                   market_scen_long_totals[, -c(1:2)])
  
      ### Bind totals to long dataset
  market_scen_long <- rbind(market_scen_long, market_scen_long_totals)
  
      ### Create aggregation column
  market_scen_long$agg <- ifelse(market_scen_long$fish_group == 
                                   "All fish groups",
                                 "Totals",
                                 "By fish group")
  
      ### Change variable classes
  market_scen_long$region <- as.factor(market_scen_long$region)
  market_scen_long$fish_group <- as.factor(market_scen_long$fish_group)
  market_scen_long$agg <- as.factor(market_scen_long$agg)

      ### Export to Shiny folder
  write.csv(market_scen_long, "./nutrition/analysis/thetis-bangladesh/visualization/market_scen_shiny.csv",
            row.names = FALSE)
  
    ## Read in fish group conversion weights
  fish_conv_weights <- read.csv("./nutrition/aux/fish-group-conv-BGD.csv") 

  #### RESHAPE GENuS DATA------------------------------------------------------

  ### Total population food consumption tables, kg/year/person----

  # Download per capita food consumed, 2010
  food_all_raw <- read.csv(
    url("https://dataverse.harvard.edu/api/access/datafile/2770316"))
  
  # Extract only selected country row 
  food_all_kg_yr <- food_all_raw[food_all_raw$X.1 %in% "Bangladesh", ]

  # Remove all foods not in selected country diet (==0)
  food_all_kg_yr <- food_all_kg_yr[,!apply(food_all_kg_yr==0,2,all)] 

  # Remove blank columns
  food_all_kg_yr <- food_all_kg_yr[, colSums(food_all_kg_yr!="")!=0] 

  # Drop country name, ID columns
  food_all_kg_yr <- subset(food_all_kg_yr, select=-c(X, X.1)) 
  
  # Add "total" label column
  food_all_kg_yr <- cbind(agesex="total", food_all_kg_yr)
  
  # Add units column
  food_all_kg_yr <- cbind(group=food_all_kg_yr[,1],
                          units="kg/yr",
                          food_all_kg_yr[,-1])
  
  # Change factors to numeric
  food_all_kg_yr[,-c(1:2)] <- sapply(food_all_kg_yr[,-c(1:2)], function(x) {
    x <- as.numeric(levels(x))[x]
    return(x)
    }
  )
  
  # Convert kg/year to g/day; change units
  food_all <- food_all_kg_yr
  food_all[,-c(1:2)] <- food_all_kg_yr[, -c(1:2)]*1000/365
  food_all$units <- "g/day"
  
  
  ### Age-sex group food consumption tables----

  # Create list of age-sex food consumption data frames
  food_agesex_kg_yr_list <- lapply(
    Sys.glob("./nutrition/aux/genus/foodAgeSex/EdibleFood*.csv"), 
    read.csv) 

  # Remove high and low columns  
  food_agesex_kg_yr_list <- lapply(food_agesex_kg_yr_list, function(x) {
    x <- x[,-grep(".1|.2", colnames(x))]
    return(x)
    }
  ) 
  
  # Extract only selected country rows
  food_agesex_kg_yr_list <- lapply(food_agesex_kg_yr_list, function(x) {
    x <- x[x$X %in% "BGD",]
    return(x)
    }
  ) 
  
  # Remove all foods not in selected country diet (==0)
  food_agesex_kg_yr_list <- lapply(food_agesex_kg_yr_list, function(x) {
    x <- x[, !apply(x==0,2,all)]
    return(x)
    }
  ) 
  
  # Remove blank columns
  food_agesex_kg_yr_list <- lapply(food_agesex_kg_yr_list, function(x) {
    x <- x[, colSums(x!="")!=0]
    return(x)
    }
  ) 
  
  # Get table labels
  agesex_labels <- as.matrix(
    Sys.glob("./nutrition/aux/genus/foodAgeSex/EdibleFood*.csv")) 
  
  # Simplify agesex names
  agesex_labels <- gsub(
    "./nutrition/aux/genus/foodAgeSex/EdibleFood_2011_|.csv",
    "", agesex_labels) 

  # Concatenate age-sex groups into single data frame
  food_agesex_kg_yr <- do.call(rbind, food_agesex_kg_yr_list)
  
  # Add agesex labels
  food_agesex_kg_yr <- cbind(group=agesex_labels, food_agesex_kg_yr)
  
  # Remove country ID
  food_agesex_kg_yr <- subset(food_agesex_kg_yr, select=-X)
  
  # Change factors to numeric
  food_agesex_kg_yr[,-1] <- sapply(food_agesex_kg_yr[,-1], function(x) {
    x <- as.numeric(levels(x))[x]
    return(x)
    }
  )
  
  # Add units column
  food_agesex_kg_yr <- cbind(group=food_agesex_kg_yr[,1], 
                             units="g/day",
                             food_agesex_kg_yr[,-1])
  
  # Change kg/year to g/day; change unit labels
  food_agesex <- food_agesex_kg_yr
  food_agesex[,-c(1:2)] <- sapply(food_agesex_kg_yr[, -c(1:2)], function(x) {
    x <- x*1000/365
    return(x)
    }
  )
  food_agesex$units <- "g/day"
  
  # Bind total and agesex food consumption tables
  food_intake <- merge(food_all, food_agesex, all=T) 

  # Keep only foods found in both total and agesex tables
  food_intake <- food_intake[, intersect(names(food_all), names(food_agesex))] 
  
  # ID seafood/fish and all other foods to segregate data
  food_fish <- names(food_intake) %in% 
    c("Freshwater.Fish", "Demersal.Fish", "Pelagic.Fish", 
      "Marine.Fish..Other","Crustaceans")
  
  # Food dataset without seafood/fish
  food_intake_nofish <- food_intake[!food_fish] 
  
  # Food dataset with only seafood/fish
  food_intake_fish <- cbind(group=food_intake[, 1], food_intake[food_fish])
    
  ### Nutrients by food tables----
  
  # Create list of food nutrient content data frames
  food_nutrients_list <- lapply(
    Sys.glob("./nutrition/aux/genus/foodNut/NutrientsByFood*.csv")
    , read.csv)
  
  # Remove high and low columns
  food_nutrients_list <- lapply(food_nutrients_list, function(x) {
    x <- x[,-grep(".1|.2", colnames(x))]
    return(x)
    }
  )
  
  # Extract only selected country rows
  food_nutrients_list <- lapply(food_nutrients_list, function(x) {
    x <- x[x$X %in% "BGD",]
    return(x)
    }
  ) 
  
  # Nutrient_labels
  nutrient_labels <- c("calories", "protein", "fat", "carbohydrates", 
                       "vitamin C", "vitamin A", "folate", "calcium", 
                       "iron", "zinc", "potassium", "dietary fiber",
                       "copper", "sodium", "phosphorus", "thiamin",
                       "riboflavin", "niacin", "vitamin B6", "magnesium",
                       "saturated fatty acids", "monounsaturated fatty acids",
                       "polyunsaturated fatty acids")
  
  # Add units for each nutrient
  nutrient_units <- c("calories (kcal/person/day)", 
                      "protein (g/person/day)", 
                      "fat (g/person/day)", 
                      "carbohydrates (g/person/day)", 
                      "vitamin C (mg/person/day)", 
                      "vitamin A (mcg/person/day)", 
                      "folate (mcg/person/day)", 
                      "calcium (mg/person/day)", 
                      "iron (mg/person/day)", 
                      "zinc (mg/person/day)", 
                      "potassium (mg/person/day)", 
                      "dietary fiber (g/person/day)", 
                      "copper (mg/person/day)", 
                      "sodium (mg/person/day)", 
                      "phosphorus (mg/person/day)",
                      "thiamin (mg/person/day)", 
                      "riboflavin (mg/person/day)", 
                      "niacin (mg/person/day)", 
                      "vitamin B6 (mg/person/day)",
                      "magnesium (mg/person/day)", 
                      "saturated fatty acids (g/person/day)", 
                      "monounsaturated fatty acids (g/person/day)", 
                      "polyunsaturated fatty acids (g/person/day)")

  # Concatenate nutrient tables into single data frame
  food_nutrients <- do.call(rbind, food_nutrients_list) 
  
  # Remove extra columns
  food_nutrients <- food_nutrients[,-c(1:2)]
  
  # Add nutrient labels
  food_nutrients <- cbind(nutrient=nutrient_labels, units=nutrient_units, 
                          food_nutrients)
  
  # Convert factors to numeric
  food_nutrients[, -c(1:2)] <- sapply(food_nutrients[, -c(1:2)], function(x) {
    x <- as.numeric(levels(x))[x]
    return(x)
    }
  )
  
  # Bind overall consumption table to nutrients by food table
  food_all_nutrients <- full_join(food_all, food_nutrients)
  
  # Keep only columns in both datasets
  food_all_nutrients <- food_all_nutrients[, intersect(names(food_all), 
                                                       names(food_nutrients))]
  
  # Food/nutrient type label
  food_nutrient_type <- c("all food", nutrient_labels)


  # Add food/nutrient type column
  food_all_nutrients <- cbind(type=food_nutrient_type,
                              food_all_nutrients) 

  # Generate nutrient content (unit/g food consumed for each nutrient)
  nutrient_content <- sweep(
    as.matrix(food_all_nutrients[-1, 3:105]), 
    MARGIN=2, 
    STATS=as.matrix(food_all_nutrients[1,3:105]), 
    FUN= "/") 

  # Units per gram of food labels
  nutrient_g_food_labels <- c("kcal/g food", "g/g food", "g/g food", 
                              "g/g food","mg/g food", "mcg/g food", 
                              "mcg/g food", "mg/g food", "mg/g food", 
                              "mg/g food", "mg/g food", "g/g food", 
                              "mg/g food", "mg/g food", "mg/g food",
                              "mg/g food", "mg/g food", "mg/g food",
                              "mg/g food", "mg/g food", "g/g food", 
                              "g/g food", "g/g food")
  
  # Add unit labels
  nutrient_content <- as.data.frame(cbind(nutrient=nutrient_labels, 
                                          unit=nutrient_g_food_labels, 
                                          nutrient_content))
  
  # Change all factors to numeric
  nutrient_content[, -c(1:2)] <- sapply(nutrient_content[,-c(1:2)], function(x) {
    x <- as.numeric(levels(x))[x]
    return(x)   
    }
  )
  
  # Nutrient content of only non-fish foods
  nutrient_content_nofish <- cbind(nutrient_content[,1:2], 
                                   nutrient_content[, intersect(
                                     names(food_intake_nofish), 
                                     names(nutrient_content))])
  
  # Nutrient content of only fish foods
  nutrient_content_fish <- cbind(nutrient_content[,1:2], 
                                   nutrient_content[, intersect(
                                     names(food_intake_fish), 
                                     names(nutrient_content))])

  ### Total nutrient intake table----
  
  # Download total nutrient intake files----
  nutrient_all_raw <- read.csv(
    url("https://dataverse.harvard.edu/api/access/datafile/2776417")) 
  
  # Extract only the Bangladesh row
  nutrient_all <- nutrient_all_raw[nutrient_all_raw$X.1 %in% "Bangladesh", ]  
  
  # Remove all foods that aren't in Bangladesh diet (==0)
  nutrient_all <- nutrient_all[,!apply(nutrient_all==0,2,all)] 
  
  # Remove blank columns
  nutrient_all <- nutrient_all[, colSums(nutrient_all!="")!=0]
  
  # Drop country name column 
  nutrient_all <- subset(nutrient_all, select=-c(X, X.1))
  
  # Add "all" label
  nutrient_all <- cbind(group="all", nutrient_all) 
  
  # Convert factors to numeric
  nutrient_all[,-1] <- sapply(nutrient_all[,-1], function(x) {
    x <- as.numeric(levels(x))[x]
    return(x)
    }
  )
  
  ### Age-sex nutrient intake tables----

  # Get agesex nutrient intake files into list of data frames
  nutrient_agesex_list <- lapply(
    Sys.glob("./nutrition/aux/genus/nutAgeSex/NutrientTotal*.csv"), 
    read.csv)
  
  # Remove high and low columns
  nutrient_agesex <- lapply(nutrient_agesex_list, function(x) {
    x <- x[,-grep(".1|.2", colnames(x))]
    return(x)
    }
  ) 
  
  # Extract only Bangladesh rows
  nutrient_agesex <- lapply(nutrient_agesex, function(x) {
  x <- x[x$X %in% "BGD",]
  return(x)
    }
  ) 
  
  # Remove all foods that aren't in Bangladesh diet (==0)
  nutrient_agesex <- lapply(nutrient_agesex, function(x) {
  x <- x[, !apply(x==0,2,all)]
  return(x)
    }
  )
  
  # Remove blank columns
  nutrient_agesex <- lapply(nutrient_agesex, function(x) {
  x <- x[, colSums(x!="")!=0]
  return(x)
    }
  ) 

  # Concatenate age-sex groups into single data frame
  nutrient_agesex <- do.call(rbind, nutrient_agesex)
  
  # Add age-sex labels
  nutrient_agesex <- cbind(group=agesex_labels, nutrient_agesex)
  
  # Remove country ID column
  nutrient_agesex <- subset(nutrient_agesex, select=-X)
  
  # Convert factors to numeric
  nutrient_agesex[,-1] <- sapply(nutrient_agesex[,-1], function(x) {
    x <- as.numeric(levels(x))[x]
    return(x)
    }
  )

  # Bind total and age-sex nutrient intake tables
  nutrient_intake <- merge(nutrient_all, nutrient_agesex, all=T)

  # Keep only nutrients found in both total and age-sex tables
  nutrient_intake <- nutrient_intake[, intersect(names(nutrient_all), 
                                                 names(nutrient_agesex))] 
                                 
  ## Calculate and remove fish-sourced nutrient intake
  
  # Multiply age-sex fish consumption and fish nutrient content matrices
  nutrient_intake_fish <- as.data.frame(as.matrix(
    food_intake_fish[,c(2:6)]) %*% as.matrix(t(nutrient_content_fish[,c(3:7)])))
  
  # Add age-sex groups
  nutrient_intake_fish <- cbind(group=c("all", agesex_labels), 
                                       nutrient_intake_fish)
  
  # Add nutrient labels
  names(nutrient_intake_fish) <- c("group", nutrient_labels)
  
  # Subtract total nutrient intake from fish intake
  nutrient_intake_nofish <- cbind(
    group=nutrient_intake[,1], 
    nutrient_intake[,-1]-nutrient_intake_fish[,-1]) 
  
  #### TRANSFORM MARKET MODEL OUTPUTS TO NUTRIENT AGE-SEX OUTPUTS--------------
  
  ### Market model fish categories to GENuS fish categories----
  
  # ***MANUALLY IMPORT MARKET MODEL OUTPUT FILE INTO SUBDIRECTORY BELOW
  
  # Create new fish group matrix based on GENuS categories
  genus_scen_kg_yr <- lapply(market_scen, 
                       function(x) 
                         cbind(fish_conv_weights[,1, drop=F], 
                               as.matrix(fish_conv_weights[,2:43]) 
                               %*% as.matrix(x[,3:43])))
  
  # Convert kg/year to g/day
  genus_scen <- lapply(genus_scen_kg_yr, function(x) {
    x[,-1] <- x[,-1]*1000/365
    return(x)
    }
  )
 
  # Extract only total rows (for later use in age-sex tables)
  genus_scen_all <- lapply(genus_scen, function(x) {
    x <- x[grep("total*", x$fish_group),]
    return(x)
      }
    )
  
  ### Convert fish to nutrients----
  
  # Expand fish nutrient content matrix for multiplication
  nutrient_content_fish_matrix <- cbind(
    nutrient_content_fish, 
    nutrient_content_fish[, 3:7],
    nutrient_content_fish[, 3:7]
  )
  
  nutrient_content_fish_matrix <- rbind(nutrient_content_fish_matrix,
                                        nutrient_content_fish_matrix,
                                        nutrient_content_fish_matrix)
   
  # Reset row numbers
  rownames(nutrient_content_fish_matrix) <- NULL

  # Zero out rows for matrix multiplication (of 3x3 matrix)
  nutrient_content_fish_matrix[1:23, c(8:17)] <- 0 # Quadrants 2, 3 
  nutrient_content_fish_matrix[24:46, c(3:7, 13:17)] <- 0 # Quadrants 4, 6
  nutrient_content_fish_matrix[47:69, c(3:12)] <- 0 # Quadrants 7, 8
  
  # New fish group matrix
  fish_nutrition_scen_all <- lapply(genus_scen, function (x) {
    x <- as.data.frame((as.matrix(nutrient_content_fish_matrix[, 3:17]) %*% 
            as.matrix(x[, 2:42])))
    return(x)
    }
  )
  
  # Add labels
  fish_nutrition_scen_all <- lapply(fish_nutrition_scen_all, function (x) {
                          x <- 
                            cbind(region=c(rep("urban", 23), 
                                            rep("rural", 23), 
                                            rep("total", 23)),
                                   nutrient=rep(nutrient_labels,3),
                                   units=nutrient_units, 
                                   x)
    }
  )
  
  ### Calculate age-sex allocation factors----
  
  # Allocation factors for all agesex groups & foods
  allocation_factors <- sweep(as.matrix(food_intake[-1,-c(1,2)]), 
              MARGIN=2, 
              STATS=as.matrix(food_intake[1,-c(1,2)]), 
              FUN= "/")
  
  # Convert to data frame
  allocation_factors <- as.data.frame(allocation_factors)
  
  # Add age-sex labels
  allocation_factors <- cbind(group=agesex_labels, allocation_factors)
  
  # Allocation factors for fish, all age-sex groups
  allocation_factors_fish <- sweep(as.matrix(
    food_intake_fish[-1, 2:6]),
    MARGIN=2, 
    STATS=as.matrix(food_intake_fish[1,2:6]), 
    FUN= "/")
  
  # Convert to data frame
  allocation_factors_fish <- as.data.frame(allocation_factors_fish)

  # Add age-sex labels
  allocation_factors_fish <- cbind(
    group=agesex_labels, allocation_factors_fish)
  
  # Create allocation factors block-diagonal matrix for multiplication
  allocation_factors_fish_BD <- as.data.frame(cbind(
    matrix(agesex_labels, nrow=160), 
    adiag(as.matrix(allocation_factors_fish[,2:6]), 
    as.matrix(allocation_factors_fish[,2:6]),
    as.matrix(allocation_factors_fish[,2:6]),
    as.matrix(allocation_factors_fish[,2:6]),
    as.matrix(allocation_factors_fish[,2:6])))) 
  
  # Convert factors to numeric
  allocation_factors_fish_BD[, -1] <- sapply(allocation_factors_fish_BD[, -1], 
                                             function(x) {
                                               x <- as.numeric(levels(x))[x]
                                               return(x)
                                               }
                                             )

  # Rename first column
  colnames(allocation_factors_fish_BD)[1] <-  "group"

  # Create GENuS scenario total fish consumption matrix for multiplication
  genus_scen_all_matrix <- lapply(genus_scen_all, function(x) {
    x <-  as.data.frame(matrix(t(cbind(x[,-1], 
                       matrix(0,5,205))), 
               ncol=41, byrow=T)[1:25, ])
    }
  )

  ### Calculate age-sex nutrient intake----

  # Generate age-sex fish group matrix
  genus_scen_agesex <- lapply(genus_scen_all_matrix, function(x) 
                                as.matrix(allocation_factors_fish_BD[,2:26]) 
                                %*% as.matrix(x))
  
  # Convert to data frame
  genus_scen_agesex <- lapply(genus_scen_agesex, function(x) {
    x <- as.data.frame(x)
    return(x)
    }
  )
  
  # Create fish category labels
  fish_labels <- c(rep("total fresh", 32), rep("total demersal", 32), 
                   rep("total pelagic", 32), rep("total oth_mar", 32), 
                   rep("total crust", 32)) 
  
  # Add age-sex and fish labels
  genus_scen_agesex <- lapply(genus_scen_agesex, function(x) {
    x <- cbind(agesex=rep(agesex_labels,5), fish_category=fish_labels, x)
    return(x)
    }
  )

  # Relabel with years
  genus_scen_agesex <- lapply(genus_scen_agesex, function(x) {
    colnames(x)[3:43] <- colnames(genus_scen_all[[1]][,-1])
    return(x)
    }
    )
  
  # Order by age-sex group
  genus_scen_agesex <- lapply(genus_scen_agesex, function(x) 
    x[with(x, order(agesex)), ]
    )
  
  # Reset row numbers
  genus_scen_agesex <- lapply(genus_scen_agesex, function(x) {
    rownames(x) <- NULL
    return(x)
    }    
  )
  
  # Create long fish nutrient content block-diagonal matrix for multiplication
  nutrient_content_fish_matrix_long <- as.data.frame(
    cbind(
      matrix(nutrient_labels, nrow=736), 
      do.call(adiag, 
              replicate(32,
                        as.matrix(nutrient_content_fish[,3:7]),
                        simplify=F)))
    ) 
  
  # Convert factors to numeric
  nutrient_content_fish_matrix_long[, -1] <- sapply(
    nutrient_content_fish_matrix_long[, -1], function(x) {
      x <- as.numeric(levels(x))[x]
      return(x)
      }
    )
  
  # Create long age-sex labels list
  agesex_labels_long <- unlist(lapply(agesex_labels, function(x) 
    rep(x,23))
    )

  # Create fish nutrient intake data frames, age-sex groups
  fish_nutrition_scen_agesex <- lapply(genus_scen_agesex, 
                                function(x)
                                  as.data.frame(as.matrix(
                                    nutrient_content_fish_matrix_long[,2:161]) 
                                    %*% as.matrix(x[,3:43]))) 
  
  # Reset row numbers
  fish_nutrition_scen_agesex <- lapply(
    fish_nutrition_scen_agesex, function (x) {
    rownames(x) <- NULL
    return (x)
    }
  )

  # Add age-sex and nutrient labels
  fish_nutrition_scen_agesex <- lapply(fish_nutrition_scen_agesex, 
                                function(x) 
                                  cbind(group=agesex_labels_long, 
                                        nutrient=matrix(nutrient_labels, 
                                                        nrow=736), 
                                        x)) 
  
  
  #### NUTRIENT MODEL OUTPUTS--------------------------------------------------
  
  ### Total population fish nutrient scenarios----
  
  # Proper case ID fields
  fish_nutrition_scen_all_pcase <- 
    lapply(fish_nutrition_scen_all, function (x) {
      x[, c(1:3)] <- sapply(x[, c(1:3)], capFirst)
      return(x)
    })
  
  # Reshape to long
  fish_nutrition_scen_all_long <- lapply(fish_nutrition_scen_all_pcase,
                                         function(x) melt(x, 
                                                          id.vars=c("region",
                                                                    "nutrient",
                                                                    "units")))
  
  # Strip "Ys" from year variable
  fish_nutrition_scen_all_long <- lapply(
    fish_nutrition_scen_all_long, function(x) { 
      x$variable <- gsub("Y","", x$variable)
      return(x)
    }  
  )
  
  # Correctly classify year variable
  fish_nutrition_scen_all_long <- lapply(
    fish_nutrition_scen_all_long, function(x) {
      x$variable <- as.numeric(x$variable)
      return(x)
    }
  )
  
  # Add scenario names & fields
  fish_nutrition_scen_all_long_plus <- Map(cbind, 
                                           fish_nutrition_scen_all_long,
                                           scenario = 
                                             names(fish_nutrition_scen_all_long)
  )
  
  # Bind all data frames in list
  fish_nutrition_scen_long <- rbind.fill(
    fish_nutrition_scen_all_long_plus)
  
  # Match to scenario descriptions file
  fish_nutrition_scen_long <- merge(fish_nutrition_scen_long, 
                                       scen_desc, 
                                       by.x="scenario")
  
  # Change variable and value column names
  names(fish_nutrition_scen_long)[
    names(fish_nutrition_scen_long) == "variable"
  ] <- "year"
  
  names(fish_nutrition_scen_long)[
    names(fish_nutrition_scen_long) == "value"
  ] <- "consumption_pc"
  
  # Suffix columns with '_nut' for Shiny use (except nutrient column)
  colnames(fish_nutrition_scen_long)[-3] <- paste(
        (colnames(fish_nutrition_scen_long)[-3]), 
        "nut", sep = "_")
  
  # Add aggregation column
  fish_nutrition_scen_long$agg <- ifelse(fish_nutrition_scen_long$region_nut == 
                                   "Total",
                                 "Totals",
                                 "By region")
 
  # Export to Shiny folder
  write.csv(fish_nutrition_scen_long, 
            "./nutrition/analysis/thetis-bangladesh/visualization/fish_nutrient_intake_shiny.csv", row.names = FALSE)
                                        

  
