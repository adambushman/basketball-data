players_data <- 
  hoopR::load_mbb_player_box()

names(players_data)

my_data <- readxl::read_xlsx(
  "C:/Users/Adam Bushman/Dropbox (HFC)/Jabber Jazz Podcast/Draft/2023 Board.xlsx", 
  skip = 3
)

art_data <- 
  my_data %>%
  mutate(
    name = case_when(
      Name == "GG Jackson" ~ "Gregory Jackson II", 
      Name == "Andre Jackson" ~ "Andre Jackson Jr.", 
      TRUE ~ Name
    )
  ) %>%
  filter(
    name %in% c(
      "Jarace Walker", "Cam Whitmore", "Ausar Thompson", "Anthony Black", "Taylor Hendricks", 
      "Cason Wallace", "Jalen Hood-Schifino", "Kobe Bufkin", "Keyonte George", "Bilal Coulibaly", 
      "Dariq Whitehead", "Gregory Jackson II", "Maxwell Lewis", "Colby Jones", "Andre Jackson Jr."
    )
  ) %>%
  left_join(
    players_data %>%
      select(name = athlete_display_name, logo = team_logo, headshot = athlete_headshot_href, color = team_color) %>%
      distinct(), 
    by = c("name" = "name")
  ) %>%
  select(
    name = Name, 
    club = Club, 
    age = Age,
    height = Height, 
    weight = Weight, 
    logo, 
    headshot, 
    color, 
    pick = Pick, 
    tier = Tier
  )


art_data <-
  art_data %>%
  mutate(
    age = paste0(age, " yrs"), 
    logo = case_when(
      club == "Metropolitans92" ~ "https://upload.wikimedia.org/wikipedia/en/2/2a/Metropolitans_92_logo.png", 
      club == "G League Ignite" ~ "https://upload.wikimedia.org/wikipedia/en/thumb/8/88/NBA_G_League_Ignite_logo_%282022%29.svg/1200px-NBA_G_League_Ignite_logo_%282022%29.svg.png",
      club == "Overtime Elite" ~ "https://smartabase.com/wp-content/uploads/2022/10/OTE_Site-logo.png", 
      club == "Barcelona (ACB)" ~ "https://www.proballers.com/api/getTeamLogo?id=148&width=300", 
      club == "NZ Breakers" ~ "https://cdn.shopify.com/s/files/1/0247/7893/2276/collections/new-zealand-breakers_1200x1200.png?v=1567640960", 
      club == "Mega Basket" ~ "https://upload.wikimedia.org/wikipedia/en/d/da/Mega-logo-2020.png", 
      TRUE ~ logo
    ), 
    headshot = case_when(
      name == "Ausar Thompson" ~ "https://images.overtime.tv/ote-games/23a42d64-4196-4306-8681-dc2a7a0442a6/f3aeb0f8-4d84-4b94-91ff-8517bf3a82e8.webp", 
      name == "Bilal Coulibaly" ~ "https://res.cloudinary.com/djcqmdgda/image/upload/nba/prospects/headshots/106638.png", 
      TRUE ~ headshot
    ),
    color = case_when(
      club == "Metropolitans92" ~ "#151b49", 
      club == "G League Ignite" ~ "#ffffff",
      club == "Overtime Elite" ~ "#ffffff", 
      club == "Barcelona (ACB)" ~ "#c00724", 
      club == "NZ Breakers" ~ "#ffffff", 
      club == "Mega Basket" ~ "#ea1473", 
      club == "Alabama" ~ "#ffffff", 
      club == "Indiana" ~ "#ffffff", 
      club == "Duke" ~ "#ffffff", 
      club == "Baylor" ~ "#f9bb06", 
      club == "Iowa State" ~ "#f9bb06", 
      club == "UCLA" ~ "#fabb2a", 
      club == "Kansas State" ~ "#ffffff", 
      club == "Tennessee" ~ "#ffffff", 
      club == "Texas" ~ "#ffffff", 
      club == "Syracuse" ~ "#0c1c43", 
      TRUE ~ paste0("#", color)
    )
  )



camcorder::gg_record(
  dir = "C:/Users/Adam Bushman/Pictures/prospect-art", 
  device = "png", 
  width = 1920, 
  height = 1080, 
  units = "px", 
  dpi = 300
)

contrast_text_color <- function(hex_code) {
  # Remove the "#" symbol if present
  hex_code <- gsub("#", "", hex_code)
  
  # Convert hexadecimal to decimal
  r <- strtoi(substr(hex_code, 1, 2), base = 16)
  g <- strtoi(substr(hex_code, 3, 4), base = 16)
  b <- strtoi(substr(hex_code, 5, 6), base = 16)
  
  # Calculate relative luminance using the sRGB color space formula
  luminance <- (0.2126 * r + 0.7152 * g + 0.0722 * b) / 255
  
  # Check contrast against black (luminance: 0)
  contrast_black <- (luminance + 0.05) / 0.05
  
  # Check contrast against white (luminance: 1)
  contrast_white <- (1 + 0.05) / (luminance + 0.05)
  
  # Determine the best contrasting text color
  if (contrast_black > contrast_white) {
    text_color <- "#000000"  # Black
  } else {
    text_color <- "#FFFFFF"  # White
  }
  
  return(text_color)
}

generate_art <- function(obj) {
  ggplot() +
    geom_polygon(
      aes(x, y), 
      data.frame(
        x = c(0, 1440, 1556, 0), 
        y = c(0, 0, 200, 200)
      ), 
      fill = pluck(obj, "color")
    ) +
    annotate(
      nflplotR::GeomFromPath,
      x = 200,
      y = 150,
      path = pluck(obj, "headshot"), 
      width = 0.23
    ) +
    annotate(
      nflplotR::GeomFromPath,
      x = 75,
      y = 50,
      path = pluck(obj, "logo"), 
      width = 0.06
    ) +
    annotate(
      "text", x = 400, y = 130, size = 7, hjust = 0,
      color = contrast_text_color(pluck(obj, "color")), 
      label = pluck(obj, "name")
    ) +
    annotate(
      "text", x = 400, y = 50, size = 3, hjust = 0,
      color = contrast_text_color(pluck(obj, "color")), 
      label = paste(
        pluck(obj, "club"), pluck(obj, "age"), pluck(obj, "height"), 
        pluck(obj, "weight"), 
        sep = " | "
      )
    ) +
    annotate(
      "text", x = 1325, y = 100, size = 8, hjust = 0.5,
      color = contrast_text_color(pluck(obj, "color")), 
      label = paste0("#", pluck(obj, "pick"))
    ) +
    scale_x_continuous(limits = c(0, 1920), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 1080), expand = c(0, 0)) +
    theme_void()
}

data <- purrr::transpose(art_data)

data %>%
  purrr::map(generate_art)
