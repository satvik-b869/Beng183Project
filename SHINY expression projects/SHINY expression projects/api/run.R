library(plumber)

# Plumb the API from app.R
pr <- plumber::plumb("app.R")
pr <- pr_set_debug(pr, TRUE)   # <--- ADD THIS

# Run on port 8000
pr$run(host = "0.0.0.0", port = 8000)
 