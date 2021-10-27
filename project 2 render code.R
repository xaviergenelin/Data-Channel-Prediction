# render code for project 2

# Create a list of the different data channels
channelList <- list("Bus", "Lifestyle", "Entertainment", "Socmed", "Tech", "World")

# loop through the different data channels to create the reports for individual data channels
for(channel in channelList){
  rmarkdown::render(
    "project2.Rmd",
    output_file = paste0("Reports/", channel),
    output_format = "github_document" (html_preview = FALSE),
    params = list(
      channel = tolower(channel)
    )
  )

}

  
## render code for the main project
# rmarkdown::render(
#   "project2.Rmd",
#   output_format = "github_document" (html_preview = FALSE),
#   output_file = "README.md",
#   encoding = "UTF-8",
#   params = list(channel = "world")
# )