library(gridExtra)
library(grid)

# Wrap text at specified width
wrap_text <- function(text, width = 20) {
  sapply(text, function(x) paste(strwrap(x, width = width), collapse = "\n"))
}

# Teaching things that Protzko says
phrases <- c(
  "Beautiful graphs", "Because you can study anything",
  "Don’t talk about researchers", "Don’t quote—unless to make fun of", 
  "Clear the slides", "Wouldn’t it be cute if… (Don't do it)",
  "As if you were explaining it to someone's Grandma",
  "I got a line, what’s the slope?", "Are you paying for that ink?",
  "Use Zotero", "Commit to reading, or commit to not reading",
  "Lick-ert scale", "Everything is a secret regression", "Shout your paper",
  "Use/find your voice", "Don't use modern references", 
  "No font smaller than 30 pt on slides", "Clinical PhD is the hardest to get into", 
  "Be a good friend to your future self", "No one will care as much about your research as you, and I do", 
  "Keep me working for you", "Random.org", "Suck it up (Intention to treat)",
  "Best time to email a researcher is 11am on Saturday", 
  "Write in English prose and speak plainly; no jargon", 
  "You can't edit a blank page", "Cheese a study (Break it)"
)

# Function to generate one bingo board
generate_bingo_board <- function(phrases) {
  selected <- sample(phrases, 24)
  selected <- wrap_text(selected, width = 18)
  board <- matrix(selected, nrow = 5, ncol = 5, byrow = TRUE)
  board[3, 3] <- "FREE"  
  colnames(board) <- c("B", "I", "N", "G", "O")
  return(board)
}

# Format today's date as DDMMMYYYY (e.g., 02MAY2025) 
day   <- format(Sys.Date(), "%d")
month <- toupper(format(Sys.Date(), "%b"))
year  <- format(Sys.Date(), "%Y")
today <- paste0(day, month, year)

# Generate and export 20 bingo cards
for (i in 1:20) {
  board <- generate_bingo_board(phrases)
  
  #save as PDF in landscape
  pdf(file = paste0("protzko_bingo_card_", i, "_", today, ".pdf"), width = 11, height = 8.5)
  
  grid.newpage()
  grid.text(paste("Protzko Bingo Card #", i),
            y = unit(0.95, "npc"),
            gp = gpar(fontsize = 18, fontface = "bold"))
  
  # Draw table with wrapped text, bigger font, bold borders, no fill
  grid.table(board, rows = NULL, theme = ttheme_default(
    core = list(
      fg_params = list(fontsize = 15, fontface = "plain", just = "center", x = 0.5),
      bg_params = list(fill = NA, col = "black", lwd = 1.5),
      padding = unit(c(4, 4), "mm")
    ),
    colhead = list(
      fg_params = list(fontsize = 14, fontface = "bold"),
      bg_params = list(fill = NA, col = "black", lwd = 1.5)
    )
  ))
  
  dev.off()
}

