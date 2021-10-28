# render code for project 2

# Create a list of the different data channels
channelList <- list("Bus", "Lifestyle", "Entertainment", "Socmed", "Tech", "World")

# loop through the different data channels to create the reports for individual data channels
for(channel in channelList){
  rmarkdown::render(
    "project2.Rmd",
    output_file = paste0("Reports/", channel),
    params = list(
      channel = channel
    )
  )
}
