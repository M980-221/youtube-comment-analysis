library(tuber)
library(httr)
library(jsonlite)

# Load your YouTube API key
api_key <- yt_get_key()

# Arabic YouTube video:
# حسين الجسمي - بشرة خير
video_id <- "QUBvVTNRp4Q"

# Request comments from YouTube
response <- GET(
  "https://www.googleapis.com/youtube/v3/commentThreads",
  query = list(
    part = "snippet",
    videoId = video_id,
    maxResults = 100,
    order = "relevance",
    textFormat = "plainText",
    key = api_key
  )
)

# Stop and show an error if the request failed
stop_for_status(response)

# Convert the response into an R list
result <- content(
  response,
  as = "parsed",
  encoding = "UTF-8",
  simplifyVector = FALSE
)

# Check whether comments were returned
if (length(result$items) == 0) {
  
  message("The API returned no comments for this video.")
  comments <- data.frame()
  
} else {
  
  # Create a data frame containing the comments
  comments <- do.call(
    rbind,
    lapply(result$items, function(item) {
      
      comment_data <- item$snippet$topLevelComment$snippet
      
      data.frame(
        author = comment_data$authorDisplayName,
        comment = comment_data$textOriginal,
        likes = comment_data$likeCount,
        published_at = comment_data$publishedAt,
        reply_count = item$snippet$totalReplyCount,
        stringsAsFactors = FALSE
      )
    })
  )
}

# Print comments in the Console
print(comments)

# Open comments as a table
if (nrow(comments) > 0) {
  View(comments)
}
write.csv(
  comments,
  "data/comments.csv",
  row.names = FALSE
)
cancel
library(tuber)

yt_set_key("AIzaSyBK9bZ4wFZ6BhYYx1gyB8NL5PLYVBWz5CA")
