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
  FunFacts = c(
    "☕ History: Invented in 1884 by Angelo Moriondo in Turin, Italy. The word 'espresso' means 'pressed out' in Italian, referring to the brewing method where hot water is forced through finely ground coffee beans.",
    "🥛 Cultural Note: Originally called 'caffè latte' in Italy, it became popular in Seattle coffee culture during the 1980s. In Italy, lattes are typically only consumed at breakfast time.",
    "❄️ Origin Story: Cold brew was first developed in Japan in the 1600s using the 'Kyoto method.' It gained massive popularity in the US during the 2010s as a smoother, less acidic alternative to iced coffee.",
    "🇹🇷 UNESCO Heritage: Turkish coffee is recognized by UNESCO as an Intangible Cultural Heritage. It's traditionally prepared in a special pot called 'cezve' and served with Turkish delight.",
    "🇦🇺 Down Under Creation: Developed in Australia and New Zealand in the 1980s. It's similar to a cappuccino but with less foam and a stronger coffee flavor. Often served with latte art.",
    "🍨 Dessert Innovation: Created in Italy, 'affogato' means 'drowned' in Italian. This dessert-drink hybrid combines the bitter espresso with sweet gelato for a perfect temperature and flavor contrast.",
    "📱 Viral Sensation: Named after Korean actor Jung Il-woo's character in a drama, it became a global phenomenon during COVID-19 lockdowns through social media platforms like TikTok and Instagram."
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
  
  # Format column names with tooltips
  cols_label(
    Coffee = with_tooltip("Coffee Type", "Nature of Coffee"),
    Origin = with_tooltip("Origin Country","Origin nation for Coffee "),
    Flag = with_tooltip("Flag","Country flag"),
    Method = with_tooltip("Brewing Method", "How the coffee is prepared and brewed"), 
    Caffeine_mg = with_tooltip("Caffeine (mg)", "Caffeine content in milligrams per serving"),
    Flavor = with_tooltip("Flavor Profile", "Primary taste characteristics and flavor notes"),
    Popularity = with_tooltip("Popularity Trend", "Popularity over time (sparkline chart)"),
    Price_USD = with_tooltip("Price (USD)", "Average price per serving in US dollars"),
    FunFacts = with_tooltip("📚 Historical & Cultural Facts", "Interesting history, cultural significance, and trivia")
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
  
  # Make Fun Facts column wider
  # cols_width(
  #   FunFacts ~ px(350),
  #   Coffee ~ px(100),
  #   Origin ~ px(120),
  #   Flag ~ px(60),
  #   Method ~ px(140),
  #   Caffeine_mg ~ px(100),
  #   Flavor ~ px(130),
  #   Popularity ~ px(120),
  #   Price_USD ~ px(80)
  # ) %>%
  # 
  # Style Fun Facts column with smaller text and italic
  tab_style(
    style = list(
      cell_text(size = px(12), style = "italic", color = "#7f8c8d")
    ),
    locations = cells_body(columns = FunFacts)
  ) %>%
  
  # Add borders and general styling
  opt_table_lines(extent = "default") %>%
  opt_table_outline(style = "solid", width = px(2), color = "#34495e") %>%
  
  # Add source note
  tab_source_note(
    source_note = md("*Data includes caffeine content, popularity trends, pricing information, and fun facts*")
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
  )