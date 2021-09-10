## code to prepare internal sysdata

ichimoku_themes <- cbind(
  original = c("#ffc0cb", "#8b8989", "#dda0dd", "#b22222", "#228b22", "#add8e6",
               "#00008b", "#00008b", "#00008b", "#ffffff", "#191970", "#00008b"),
  dark = c("#ffb6c1", "#8b8989", "#dda0dd", "#c71585", "#40e0d0", "#b58900",
           "#eee8d5", "#fdf6e3", "#fdf6e3", "#002b36", "#eee8d5", "#fdf6e3"),
  solarized = c("#d33682", "#eee8d5", "#6c71c4", "#cb4b16", "#859900", "#268bd2",
                "#002b36", "#002b36", "#002b36", "#fdf6e3", "#073642", "#002b36"),
  mono = c("#d9d9d9", "#d4d4d4", "#d1d1d1", "#737373", "#1f1f1f", "#b8b8b8",
           "#1a1a1a", "#1a1a1a", "#1a1a1a", "#ffffff", "#333333", "#1a1a1a")
)

x_user_agent <- "r-ichimoku/1.1.5"

usethis::use_data(ichimoku_themes, x_user_agent, internal = TRUE, overwrite = TRUE)
