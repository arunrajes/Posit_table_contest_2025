library(dplyr)
library(gt)
library(gtExtras)
library(tibble)

# Create the coffee data with flag image paths
coffee_tbl <- tibble(
  Coffee = c("Espresso", "Latte", "Cold Brew", "Turkish", "Flat White", "Affogato", "Dalgona"),
  Origin = c("Italy", "Italy", "USA", "Turkey", "Australia", "Italy", "South Korea"),
  Flag = c("Images/italy.png", "Images/italy.png", "Images/usa.png", "Images/turkey.png", 
           "Images/australia.png", "Images/italy.png", "Images/south_korea.png"),
  Method = c("Espresso Machine", "Espresso + Milk", "Cold Brew", "Ibrik Pot", "Espresso + Milk", "Espresso + Ice Cream", "Whipped Instant Coffee"),
  Caffeine_mg = c(64, 150, 200, 130, 110, 75, 90),
  Flavor = c("Bold, Bitter", "Creamy, Smooth", "Smooth, Refreshing", "Spiced, Strong", "Velvety, Mild", "Sweet, Dessert", "Frothy, Sweet"),
  Popularity = list(
    c(70, 75, 80, 85, 90),
    c(80, 82, 85, 89, 90),
    c(50, 55, 65, 70, 75),
    c(40, 45, 55, 60, 62),
    c(60, 70, 72, 74, 75),
    c(30, 35, 40, 42, 45),
    c(20, 80, 90, 85, 60)
  ),
  Price_USD = c(2, 4, 5, 3, 4, 6, 3),
  FunFact = c(
    "Espresso is the base for most coffee drinks",
    "Latte is milk-heavy and smooth",
    "Cold Brew originated from Japan’s Kyoto method",
    "Turkish coffee is UNESCO-listed cultural heritage",
    "Flat White popularized in Australia/New Zealand",
    "Affogato is eaten as dessert, not just a drink",
    "Dalgona went viral during COVID-19 lockdowns"
  )
)

# Create the gt table
coffee_tbl %>%
  gt() %>%
  
  # Add title and subtitle
  tab_header(
    title = md("**☕ Coffee Data Explorer**"),
    subtitle = "A comprehensive guide to different coffee types and their characteristics"
  ) %>%
  
  # Format column names
  cols_label(
    Coffee = "Coffee Type",
    Origin = "Origin Country",
    Flag = "Flag",
    Method = "Brewing Method", 
    Caffeine_mg = "Caffeine (mg)",
    Flavor = "Flavor Profile",
    Popularity = "Popularity Trend",
    Price_USD = "Price (USD)",
    FunFact = "Extra Info"
  ) %>%
  
  # Add flag images using local_image()
  text_transform(
    locations = cells_body(columns = Flag),
    fn = function(x) {
      local_image(filename = x, height = 20)
    }
  ) %>%
  
  # Format caffeine values to be bold
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = Caffeine_mg)
  ) %>%
  
  # Format price column
  fmt_currency(
    columns = Price_USD,
    currency = "USD"
  ) %>%
  
  # Add sparklines for popularity (requires gtExtras package)
  gt_plt_sparkline(
    column = Popularity,
    same_limit = FALSE
  ) %>%
  
  # Style the header
  tab_style(
    style = list(
      cell_fill(color = "#34495e"),
      cell_text(color = "white", weight = "bold")
    ),
    locations = cells_column_labels()
  ) %>%
  
  # Add alternating row colors
  opt_row_striping() %>%
  
  # Style the title
  tab_style(
    style = list(
      cell_text(size = px(24), weight = "bold", color = "#2c3e50")
    ),
    locations = cells_title(groups = "title")
  ) %>%
  
  # Style the subtitle
  tab_style(
    style = list(
      cell_text(size = px(14), color = "#7f8c8d", style = "italic")
    ),
    locations = cells_title(groups = "subtitle")
  ) %>%
  
  # Add conditional formatting for high caffeine values
  tab_style(
    style = list(
      cell_text(color = "#e74c3c", weight = "bold")
    ),
    locations = cells_body(
      columns = Caffeine_mg,
      rows = Caffeine_mg > 150
    )
  ) %>%
  
  # Add conditional formatting for medium caffeine values
  tab_style(
    style = list(
      cell_text(color = "#f39c12", weight = "bold")
    ),
    locations = cells_body(
      columns = Caffeine_mg,
      rows = Caffeine_mg > 100 & Caffeine_mg <= 150
    )
  ) %>%
  
  # Add conditional formatting for low caffeine values
  tab_style(
    style = list(
      cell_text(color = "#27ae60", weight = "bold")
    ),
    locations = cells_body(
      columns = Caffeine_mg,
      rows = Caffeine_mg <= 100
    )
  ) %>%
  
  # Style origin and flag columns
  tab_style(
    style = cell_text(size = px(14)),
    locations = cells_body(columns = c(Origin, Flag))
  ) %>%
  
  # Add borders and general styling
  opt_table_lines(extent = "default") %>%
  opt_table_outline(style = "solid", width = px(2), color = "#34495e") %>%
  
  # Add source note
  tab_source_note(
    source_note = md("*Data includes caffeine content, popularity trends, and pricing information*")
  ) %>%
  
  # Add footnotes for caffeine levels
  tab_footnote(
    footnote = "High caffeine content (>150mg)",
    locations = cells_body(columns = Caffeine_mg, rows = Caffeine_mg > 150)
  ) %>%
  
  # Adjust table width and alignment
  tab_options(
    table.width = pct(100),
    table.font.size = px(14),
    data_row.padding = px(8),
    column_labels.padding = px(12)
  )  %>%
# Tooltips on Brewing Method & Flavor
  gt_tooltip(
    columns = Method,
    text = "How the coffee is brewed"
  ) %>%
    gt_tooltip(
      columns = Flavor,
      text = "Taste notes / flavor profile"
    ) %>%
    
    # Expandable row (FunFact column hidden until expanded)
    tab_options(row_group.as_column = TRUE)
