# Load all function files from R/ directory
invisible(lapply(list.files("R", pattern = "\\.R$", full.names = TRUE), source))
