library(dplyr)
library(gt)
library(gtExtras)
library(tibble)
library(purrr)

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


# Enhanced dataset with helper columns
coffee_tbl2 <- coffee_tbl %>%
  mutate(
    # Collapsible FunFacts (using <details>)
    Details = map(FunFacts, ~gt::html(paste0(
      "<details><summary style='cursor:pointer;color:#8B4513;font-weight:bold;'>Read more</summary>",
      "<div style='padding:8px;margin-top:5px;'>", .x, "</div>",
      "</details>"
    ))),
    
    # Brewing method icons
    MethodIcon = case_when(
      grepl("Machine", Method) ~ "☕",
      grepl("Milk", Method) ~ "🥛",
      grepl("Ice Cream", Method) ~ "🍨",
      grepl("Cold Brew", Method) ~ "🧊",
      grepl("Pot", Method) ~ "🏺",
      grepl("Whipped", Method) ~ "☁️",
      TRUE ~ "☕"
    ),
    MethodWithIcon = paste0(MethodIcon, " ", Method),
    
    # Caffeine % relative to max
    Caffeine_pct = round(Caffeine_mg / max(Caffeine_mg) * 100, 1),
    
    # Inline star ratings with ★ ☆
    RatingHTML = purrr::map_chr(PopularityRating, function(rating) {
      full_stars <- floor(rating)
      half_star <- ifelse((rating - full_stars) >= 0.5, 1, 0)
      empty_stars <- 5 - full_stars - half_star
      paste0(
        "<span style='color:#FFD700;'>",
        strrep("★", full_stars),
        ifelse(half_star == 1, "⯨", ""),
        "</span>",
        "<span style='color:#DDD;'>",
        strrep("☆", empty_stars),
        "</span>",
        " <span style='color:#666;font-size:0.9em;'>(", sprintf("%.1f", rating), ")</span>"
      )
    }),
    
    # Calculate value score for heatmap (lower price + higher rating = better value)
    ValueScore = PopularityRating / Price_USD
  )

# Create the gt table
coffee_tbl2 %>%
  group_by(Region) %>%
  gt() %>%
  fmt_markdown(columns = Coffee) %>%
  
  
  # Title + subtitle
  tab_header(
    title = md("**☕ Global Coffee Culture Explorer ☕**"),
    subtitle = md("*From Italian espresso bars to Korean coffee trends - discover the world in every cup*")
  ) %>%
  
  # Column groups
  tab_spanner(
    label = md("**🌍 Origin & Identity**"),
    columns = c(Origin, Flag)
  ) %>%
  tab_spanner(
    label = md("**⚗️ Composition**"),
    columns = c(MethodWithIcon, Caffeine_mg, Flavor)
  ) %>%
  tab_spanner(
    label = md("**📊 Market & Popularity**"),
    columns = c(Popularity, RatingHTML, Price_USD)
  ) %>%
  tab_spanner(
    label = md("**📚 Cultural Heritage**"),
    columns = c(Details)
  ) %>%
  
  # Column labels
  cols_label(
    Origin = "Country",
    Flag = "Flag",
    MethodWithIcon = "Brewing Method", 
    Caffeine_mg = "Caffeine (mg)",
    Flavor = "Flavor Profile",
    Popularity = "Trend",
    RatingHTML = "⭐ Rating",
    Price_USD = "Price (USD)",
    Details = "Historical & Cultural Facts"
  ) %>%
  
  # Reorder columns to move Details to the end
  cols_move_to_end(columns = Details) %>%
  
  # Hide original columns we've transformed
  cols_hide(columns = c(Method, MethodIcon, FunFacts, Region, PopularityRating, Caffeine_pct, ValueScore)) %>%
  
  # Format flag images
  fmt_image(
    columns = Flag,
    height = 30,
    width = 30
  ) %>%
  
  # Render HTML columns
  fmt_markdown(columns = c(RatingHTML, Details)) %>%
  
  # Sparklines for popularity trends with WARM BROWN PALETTE
  gt_plt_sparkline(
    column = Popularity,
    same_limit = TRUE,
    palette = c("#3E2723", "#5D4037", "#6D4C41", "#8D6E63", "#A1887F")
  ) %>%
  
  # Caffeine bar chart with LATTE TO ESPRESSO GRADIENT
  gt_plt_bar_pct(
    column = Caffeine_mg,
    scaled = FALSE,
    fill = "#3E2723",  # Dark espresso brown
    background = "#D7CCC8",  # Light latte cream
    width = 50,
    labels = TRUE
  ) %>%
  
  # Styling
  tab_options(
    table.font.size = px(13),
    heading.align = "center",
    heading.title.font.size = px(24),
    heading.subtitle.font.size = px(14),
    column_labels.font.weight = "bold",
    column_labels.background.color = "#EFEBE9",
    table.border.top.color = "#3E2723",
    table.border.top.width = px(3),
    row_group.background.color = "#D7CCC8",
    row_group.font.weight = "bold",
    row_group.font.size = px(14)
  ) %>%

# Display the table
  fmt_currency(
    columns = Price_USD,
    currency = "USD",
    decimals = 0  
  )  %>%
  cols_width(
    Coffee ~ px(140),
    Origin ~ px(100),
    Flag ~ px(70),
    MethodWithIcon ~ px(180),
    Caffeine_mg ~ px(120),
    Flavor ~ px(180),
    Popularity ~ px(160),
    RatingHTML ~ px(140),
    Price_USD ~ px(100),
    Details ~ px(180)
  ) %>%
  cols_align(
    align = "center",
    columns = c(Caffeine_mg, Price_USD, RatingHTML)
  )


