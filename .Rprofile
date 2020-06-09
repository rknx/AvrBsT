# Set variables for project
ROOT = "/mnt/d/OneDrive - University of Florida/Lab/Prj5 - AvrBst/Rproj"
FONT_PATH = "/mnt/c/Users/rknx/AppData/Local/Microsoft/Windows/Fonts/"

# Set working directory
setwd(ROOT)

# Load universal project functions
source(paste0(ROOT, "/0 - prerequisites.R"))

# Load help in browser
options(help_type = "html")

# Set maxprint
options(max.print = 250)

# Prevent strings as factors
options(stringsAsFactors = FALSE)

# Execute this when starting a session
.First = function() {
    if (interactive()) { # Not for Rscript
        cat("Successfully loaded .Rprofile\n")
        cat(paste0("##------ [", getwd(), "] ------##\n"))
        utils::timestamp()
        cat(paste0("\n"))
    }
}

# Execute this when ending a session
.Last = function() {
    if (interactive()) { # Not for Rscript
        histFile = Sys.getenv("R_HISTFILE")
        if (histFile == "") histFile = paste0(ROOT, "/.Rhistory.R")
        utils::savehistory(histFile)
    }
}
