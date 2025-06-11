install.packages("httr")
install.packages("jsonlite")  # Optional, for parsing JSON

clientId<-"96fb2a07-31e9-49b4-bf12-cc2c8c0f561f_d0012fbe-02d8-4bad-bda0-bb4bbff9a480"
ClientSecret<- "cfg3JRMhNtv9mG1ZmoW0a0kMSfE2jzyw6dsBzcPRiKw="


library(httr)
library("jsonlite")

diagnosis <- "severe depressive episode"

# Base API endpoint
base_url <- "https://clinicaltables.nlm.nih.gov/api/icd10cm/v3/search"


# Send GET request
response <- GET(url = base_url,
                query = list(sf = "code,name", 
                             terms = diagnosis))
# Parse the content
result <- content(response, as = "text", encoding = "UTF-8")
data <- fromJSON(result)
data
# Extract relevant info
codes <- data[[2]]
descriptions <- data[[3]]

# Combine into a data frame
icd10_results <- data.frame(code = codes, description = descriptions, stringsAsFactors = FALSE)

# View results
print(icd10_results)



##################################################################################
### try german

# Define the base URL and parameters
base_url <- "https://id.who.int/icd/release/10/2025"
endpoint <- "/search"
query <- "Diabetes"
language <- "de"  # German language


# Construct the full URL
url <- paste0(base_url, endpoint, "?q=", query, "&lang=", language)

# Make the GET request
response <- GET(url, add_headers(Authorization = paste("Bearer", ClientSecret)))

# Check if the request was successful
if (status_code(response) == 200) {
  # Parse the JSON response
  content_data <- content(response, as = "text", encoding = "UTF-8")
  parsed_data <- fromJSON(content_data)
  
  # Extract relevant information
  if (length(parsed_data$destinationEntities) > 0) {
    icd_results <- data.frame(
      code = sapply(parsed_data$destinationEntities, function(x) x$code),
      description = sapply(parsed_data$destinationEntities, function(x) x$title$`@value`),
      stringsAsFactors = FALSE
    )
    print(icd_results)
  } else {
    print("No results found.")
  }
} else {
  print(paste("Error:", status_code(response)))
}
# does not work




