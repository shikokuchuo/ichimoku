## code to prepare internal sysdata

ichimoku_themes <- cbind(
  original = c("pink", "snow4", "plum", "firebrick", "forestgreen", "lightblue",
               "darkblue", "darkblue", "darkblue", "white", "midnightblue", "darkblue"),
  dark = c("lightpink", "snow4", "plum", "mediumvioletred", "turquoise", "#b58900",
           "#eee8d5", "#fdf6e3", "#fdf6e3", "#002b36", "#eee8d5", "#fdf6e3"),
  solarized = c("#d33682", "#eee8d5", "#6c71c4", "#cb4b16", "#859900", "#268bd2",
                "#002b36", "#002b36", "#002b36", "#fdf6e3", "#073642", "#002b36"),
  mono = c("gray85", "gray83", "gray81", "gray45", "gray12", "gray72",
           "gray10", "gray10", "gray10", "white", "gray20", "gray10")
)

ichimoku_user_agent <- "r-ichimoku/0.3.51"

usethis::use_data(ichimoku_themes, ichimoku_user_agent, internal = TRUE, overwrite = TRUE)
