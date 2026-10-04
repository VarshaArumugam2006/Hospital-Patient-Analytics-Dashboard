# ==============================================================================
# Cloud & Container Runner for Hospital Patient Analytics Dashboard
# ==============================================================================
# Reads PORT and HOST from environment (Render, Railway, Fly.io, or Local Docker)

port <- as.numeric(Sys.getenv("PORT", "10000"))
host <- Sys.getenv("HOST", "0.0.0.0")

cat("=================================================================\n")
cat("  Hospital Patient Analytics Platform - Cloud Runner\n")
cat(sprintf("  Binding to: %s:%d\n", host, port))
cat("=================================================================\n\n")

shiny::runApp(
  appDir = ".",
  host = host,
  port = port,
  launch.browser = FALSE
)
