## code to prepare `ichimoku_themes` dataset goes here

ichimoku_themes <- as.matrix(data.frame(
  default = c("pink", "snow4", "plum", "firebrick", "forestgreen", "lightblue",
                "darkblue", "darkblue", "darkblue", "white", "midnightblue", "darkblue"),
  dark = c("lightpink", "snow4", "plum", "mediumvioletred", "turquoise", "#b58900",
             "#eee8d5", "#fdf6e3", "#fdf6e3", "#002b36", "#eee8d5", "#fdf6e3"),
  solarized = c("#d33682", "#eee8d5", "#6c71c4", "#cb4b16", "#859900", "#268bd2",
                  "#002b36", "#002b36", "#002b36", "#fdf6e3", "#073642", "#002b36"),
  classic = c("cornsilk", "snow4", "plum", "dodgerblue", "coral4", "lightblue",
              "darkcyan", "orangered3", "black", "darkcyan", "orangered3", "black"),
  mono = c("gray92", "gray88", "gray85", "gray45", "gray12", "gray75",
              "gray10", "gray10", "gray10", "white", "gray20", "gray10")
))

usethis::use_data(ichimoku_themes, internal = TRUE, overwrite = TRUE)
