library(dplyr)
library(gt)
library(gtExtras)
library(tibble)

# Create the coffee data with enhanced iconography and visual elements
coffee_tbl <- tibble(
  Coffee = c("☕ Espresso", "🥛 Latte", "🍨 Affogato", "🎨 Flat White", "❄️ Cold Brew", "🏺 Turkish", "🧊 Dalgona"),
  Origin = c("Italy", "Italy", "Italy", "Australia", "USA", "Turkey", "South Korea"),
  Region = c("Europe", "Europe", "Europe", "Oceania", "North America", "Asia", "Asia"),
  Flag = c("Images/italy.png", "Images/italy.png", "Images/italy.png", "Images/australia.png", 
           "Images/usa.png", "Images/turkey.png", "Images/south_korea.png"),
  Method = c("Espresso Machine", "Espresso + Milk", "Espresso + Ice Cream", "Espresso + Milk", 
             "Cold Brew", "Ibrik Pot", "Whipped Instant Coffee"),
  Caffeine_mg = c(64, 150, 75, 110, 200, 130, 90),
  Flavor = c("🌋 Bold, Bitter", "🥛 Creamy, Smooth", "🍫 Sweet, Dessert", "🌿 Velvety, Mild", 
             "🌊 Smooth, Refreshing", "🌶️ Spiced, Strong", "☁️ Frothy, Sweet"),
  Popularity = list(
    c(70, 75, 80, 85, 90),
    c(80, 82, 85, 89, 90),
    c(30, 35, 40, 42, 45),
    c(60, 70, 72, 74, 75),
    c(50, 55, 65, 70, 75),
    c(40, 45, 55, 60, 62),
    c(20, 80, 90, 85, 60)
  ),
  PopularityRating = c(4.5, 4.5, 2.3, 3.7, 3.8, 3.1, 4.2),
  Price_USD = c(2, 4, 6, 4, 5, 3, 3),
  FunFacts = c(
    "☕ History: Invented in 1884 by Angelo Moriondo in Turin, Italy. The word 'espresso' means 'pressed out' in Italian, referring to the brewing method where hot water is forced through finely ground coffee beans.",
    "🥛 Cultural Note: Originally called 'caffè latte' in Italy, it became popular in Seattle coffee culture during the 1980s. In Italy, lattes are typically only consumed at breakfast time.",
    "🍨 Dessert Innovation: Created in Italy, 'affogato' means 'drowned' in Italian. This dessert-drink hybrid combines the bitter espresso with sweet gelato for a perfect temperature and flavor contrast.",
    "🇦🇺 Down Under Creation: Developed in Australia and New Zealand in the 1980s. It's similar to a cappuccino but with less foam and a stronger coffee flavor. Often served with latte art.",
    "❄️ Origin Story: Cold brew was first developed in Japan in the 1600s using the 'Kyoto method.' It gained massive popularity in the US during the 2010s as a smoother, less acidic alternative to iced coffee.",
    "🇹🇷 UNESCO Heritage: Turkish coffee is recognized by UNESCO as an Intangible Cultural Heritage. It's traditionally prepared in a special pot called 'cezve' and served with Turkish delight.",
    "📱 Viral Sensation: Named after Korean actor Jung Il-woo's character in a drama, it became a global phenomenon during COVID-19 lockdowns through social media platforms like TikTok and Instagram."
  )
)

# Create the gt table with grouping and dark theme
coffee_tbl %>%
  # Group by Region first
  group_by(Region) %>%
  gt() %>%
  fmt_markdown(columns = Coffee) %>%
  
  # Add title and subtitle with coffee theme
  tab_header(
    title = md("**☕ Global Coffee Culture Explorer ☕**"),
    subtitle = md("*From Italian espresso bars to Korean coffee trends - discover the world in every cup*")
  ) %>%
  
  # Create column groups
  tab_spanner(
    label = md("**🌍 Origin & Identity**"),
    columns = c(Origin, Flag)
  ) %>%
  tab_spanner(
    label = md("**⚗️ Composition**"),
    columns = c(Method, Caffeine_mg, Flavor)
  ) %>%
  tab_spanner(
    label = md("**📊 Market & Popularity**"),
    columns = c(Popularity, PopularityRating, Price_USD)
  ) %>%
  tab_spanner(
    label = md("**📚 Cultural Heritage**"),
    columns = c(FunFacts)
  ) %>%
  
  # Format column names with tooltips
  cols_label(
    Origin = "Country",
    Flag = "Flag",
    Method = with_tooltip("Brewing Method", "How the coffee is prepared and brewed"), 
    Caffeine_mg = with_tooltip("Caffeine (mg)", "Caffeine content with visual bar chart"),
    Flavor = with_tooltip("Flavor Profile", "Taste characteristics with emoji categories"),
    Popularity = with_tooltip("Trend", "Popularity over time (sparkline)"),
    PopularityRating = with_tooltip("⭐ Rating", "Overall popularity rating out of 5 stars"),
    Price_USD = with_tooltip("Price (USD)", "Average price with visual comparison bars"),
    FunFacts = with_tooltip("Historical & Cultural Facts", "Rich history, cultural significance, and fascinating trivia")
  ) %>%
  
  # Add flag images using local_image()
  text_transform(
    locations = cells_body(columns = Flag),
    fn = function(x) {
      local_image(filename = x, height = 25)
    }
  ) %>%
  
  # Add mini bar charts for caffeine content
  gt_plt_bar_pct(
    column = Caffeine_mg,
    scaled = TRUE,
    #fill = "#D2691E",
    #background = "#4E342E",
    width = 40
  ) %>%
  
  # Add mini bar charts for price
  gt_plt_bar_pct(
    column = Price_USD,
    scaled = TRUE, 
    fill = "#228B22",
    background = "#2F4F2F",
    width = 35
  ) %>%
  
  # Convert popularity rating to star display
  text_transform(
    locations = cells_body(columns = PopularityRating),
    fn = function(x) {
      rating <- as.numeric(x)
      # Ensure rating is between 0 and 5
      rating <- pmax(0, pmin(5, rating))
      
      full_stars <- floor(rating)
      half_star <- ifelse((rating - full_stars) >= 0.5, 1, 0)
      empty_stars <- 5 - full_stars - half_star
      
      # Ensure all values are non-negative
      full_stars <- max(0, full_stars)
      half_star <- max(0, half_star)
      empty_stars <- max(0, empty_stars)
      
      stars_display <- paste0(
        if(full_stars > 0) paste(rep("⭐", full_stars), collapse = "") else "",
        if(half_star > 0) "⚡" else "",
        if(empty_stars > 0) paste(rep("⚫", empty_stars), collapse = "") else ""
      )
      
      paste0(stars_display, " (", sprintf("%.1f", rating), ")")
    }
  ) %>%
  
  # Add sparklines for popularity
  gt_plt_sparkline(
    column = Popularity,
    same_limit = FALSE,
    palette = c("#8B4513", "#D2691E", "#F4A460", "#DEB887", "#F5DEB3")
  ) %>%
  
  # DARK THEME STYLING
  # Style the main title with coffee gradient
  tab_style(
    style = list(
      cell_fill(color = "transparent"),
      cell_text(
        size = px(28), 
        weight = "bold", 
        color = "#D2691E",
        font = "Georgia"
      )
    ),
    locations = cells_title(groups = "title")
  ) %>%
  
  # Style the subtitle
  tab_style(
    style = list(
      cell_fill(color = "transparent"),
      cell_text(
        size = px(16), 
        color = "#F5DEB3", 
        style = "italic"
      )
    ),
    locations = cells_title(groups = "subtitle")
  ) %>%
  
  # Style column spanners (group headers) with coffee gradient
  tab_style(
    style = list(
      cell_fill(color = "#3E2723"),
      cell_text(
        color = "#D2691E", 
        weight = "bold", 
        size = px(14),
        transform = "uppercase"
      ),
      cell_borders(
        sides = c("top", "bottom"), 
        color = "#8B4513", 
        weight = px(2)
      )
    ),
    locations = cells_column_spanners()
  ) %>%
  
  # Style column headers with dark coffee theme
  tab_style(
    style = list(
      cell_fill(color = "#4E342E"),
      cell_text(
        color = "#F5DEB3", 
        weight = "bold", 
        size = px(12)
      ),
      cell_borders(
        sides = "all", 
        color = "#6D4C41", 
        weight = px(1)
      )
    ),
    locations = cells_column_labels()
  ) %>%
  
  # Style row group headers (regions) with gradient effect
  tab_style(
    style = list(
      cell_fill(color = "#2E1A0E"),
      cell_text(
        color = "#DEB887", 
        weight = "bold", 
        size = px(16),
        transform = "uppercase"
      ),
      cell_borders(
        sides = c("top", "bottom"), 
        color = "#8B4513", 
        weight = px(3)
      )
    ),
    locations = cells_row_groups()
  ) %>%
  
  # Style coffee names (stub) with warm colors
  tab_style(
    style = list(
      cell_fill(color = "#3E2723"),
      cell_text(
        color = "#D2691E", 
        weight = "bold", 
        size = px(14)
      ),
      cell_borders(
        sides = "right", 
        color = "#8B4513", 
        weight = px(2)
      )
    ),
    locations = cells_stub()
  ) %>%
  
  # Style data cells with dark background
  tab_style(
    style = list(
      cell_fill(color = "#2C1810"),
      cell_text(color = "#F5DEB3", size = px(12))
    ),
    locations = cells_body()
  ) %>%
  
  # HEATMAP COLORING FOR CAFFEINE LEVELS
  # High caffeine (>150mg) - Intense red heatmap
  # tab_style(
  #   style = list(
  #     cell_text(color = "#FFFFFF", weight = "bold", size = px(13)),
  #     cell_fill(color = "#8B0000")  # Dark red
  #   ),
  #   locations = cells_body(
  #     columns = Caffeine_mg,
  #     rows = Caffeine_mg > 150
  #   )
  # ) %>%
  # 
  # # Medium-high caffeine (100-150mg) - Orange heatmap
  # tab_style(
  #   style = list(
  #     cell_text(color = "#FFFFFF", weight = "bold", size = px(13)),
  #     cell_fill(color = "#FF4500")  # Orange red
  #   ),
  #   locations = cells_body(
  #     columns = Caffeine_mg,
  #     rows = Caffeine_mg > 100 & Caffeine_mg <= 150
  #   )
  # ) %>%
  # 
  # # Medium caffeine (75-100mg) - Yellow heatmap
  # tab_style(
  #   style = list(
  #     cell_text(color = "#000000", weight = "bold", size = px(13)),
  #     cell_fill(color = "#FFD700")  # Gold
  #   ),
  #   locations = cells_body(
  #     columns = Caffeine_mg,
  #     rows = Caffeine_mg >= 75 & Caffeine_mg <= 100
  #   )
  # ) %>%
  # 
  # # Low caffeine (<75mg) - Green heatmap
  # tab_style(
  #   style = list(
  #     cell_text(color = "#FFFFFF", weight = "bold", size = px(13)),
  #     cell_fill(color = "#228B22")  # Forest green
  #   ),
  #   locations = cells_body(
  #     columns = Caffeine_mg,
  #     rows = Caffeine_mg < 75
  #   )
  # ) %>%
  
  # HEATMAP COLORING FOR PRICE LEVELS
  # Expensive (>$5) - Deep purple
  tab_style(
    style = list(
      cell_text(color = "#FFFFFF", weight = "bold"),
      cell_fill(color = "#4B0082")  # Indigo
    ),
    locations = cells_body(
      columns = Price_USD,
      rows = Price_USD > 5
    )
  ) %>%
  
  # Medium price ($3-$5) - Blue
  tab_style(
    style = list(
      cell_text(color = "#FFFFFF", weight = "bold"),
      cell_fill(color = "#4169E1")  # Royal blue
    ),
    locations = cells_body(
      columns = Price_USD,
      rows = Price_USD >= 3 & Price_USD <= 5
    )
  ) %>%
  
  # Budget price (<$3) - Teal
  tab_style(
    style = list(
      cell_text(color = "#FFFFFF", weight = "bold"),
      cell_fill(color = "#008B8B")  # Dark cyan
    ),
    locations = cells_body(
      columns = Price_USD,
      rows = Price_USD < 3
    )
  ) %>%
  
  # Enhanced Fun Facts styling with coffee-themed background
  tab_style(
    style = list(
      cell_text(size = px(11), color = "#F5DEB3"),
      cell_fill(color = "#1A0E0A"),
      cell_borders(
        sides = "left", 
        color = "#D2691E", 
        weight = px(4)
      )
    ),
    locations = cells_body(columns = FunFacts)
  ) %>%
  
  # Column widths optimization with new visual elements
  cols_width(
    Origin ~ px(100),
    Flag ~ px(70),
    Method ~ px(150),
    Caffeine_mg ~ px(120),
    Flavor ~ px(160),
    Popularity ~ px(120),
    PopularityRating ~ px(140),
    Price_USD ~ px(110),
    FunFacts ~ px(380)
  ) %>%
  
  # Overall table options with dark theme
  tab_options(
    table.background.color = "#1A0E0A",
    table.border.top.style = "solid",
    table.border.top.width = px(4),
    table.border.top.color = "#8B4513",
    table.border.bottom.style = "solid",
    table.border.bottom.width = px(4),
    table.border.bottom.color = "#8B4513",
    table.width = pct(100),
    table.font.size = px(12),
    data_row.padding = px(10),
    column_labels.padding = px(12),
    row_group.padding = px(8),
    table.font.color = "#F5DEB3",
    heading.background.color = "transparent",
    column_labels.background.color = "#4E342E",
    row_group.background.color = "#2E1A0E",
    stub.background.color = "#3E2723"
  ) %>%
  
  # Add source note with coffee theme
  tab_source_note(
    source_note = md("*☕ Comprehensive coffee data including origins, brewing methods, market trends, and rich cultural heritage*")
  ) %>%
  
  # Add footnotes with enhanced information
  tab_footnote(
    footnote = "UNESCO Intangible Cultural Heritage status",
    locations = cells_body(columns = FunFacts, rows = grepl("Turkish", Coffee))
  ) %>%
  
  tab_footnote(
    footnote = "Viral social media phenomenon during COVID-19",
    locations = cells_body(columns = FunFacts, rows = grepl("Dalgona", Coffee))
  ) %>%
  
  tab_footnote(
    footnote = "Heatmap: 🟢 Low caffeine (<75mg), 🟡 Medium (75-100mg), 🟠 High (100-150mg), 🔴 Very High (>150mg)",
    locations = cells_column_labels(columns = Caffeine_mg)
  ) %>%
  
  tab_footnote(
    footnote = "⭐ Full star, ⚡ Half star, ⚫ Empty star - Rating out of 5.0",
    locations = cells_column_labels(columns = PopularityRating)
  ) %>%
  
  # Style source note
  tab_style(
    style = list(
      cell_text(color = "#DEB887", size = px(10), style = "italic")
    ),
    locations = cells_source_notes()
  ) %>%
  
  # Style footnotes
  tab_style(
    style = list(
      cell_text(color = "#D2691E", size = px(9))
    ),
    locations = cells_footnotes()
  )