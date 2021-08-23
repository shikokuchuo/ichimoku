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

x_user_agent <- "r-ichimoku/1.1.0"
x_archive_sha256 <- NA

usethis::use_data(ichimoku_themes, x_user_agent, x_archive_sha256,
                  internal = TRUE, overwrite = TRUE)
