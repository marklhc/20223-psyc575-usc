library(checkdown)
right_ans <- paste("Correct", emo::ji("+1"))
wrong_ans <- "That is not correct. Rewatch the video if needed"

# Embedding youtube videos
add_youtube <- function(src) {
    yt_url <- paste0("https://www.youtube.com/embed/", src)
    htmltools::div(
        style = "position: relative; padding-bottom: 56.25%; height: 0; overflow: hidden;",
        htmltools::tags$iframe(
            src = yt_url,
            style = "position: absolute; top: 0; left: 0; width: 100%; height: 100%; border:0;",
            title = "YouTube Video",
            allowfullscreen = ""
        )
    )
}

# Use callout box for checkdown questions
practice_question <- function(qtitle, ctitle = "Check your learning", ...) {
    cat("::: {.callout-note icon=false}\n")
    cat("\n## ", ctitle, "\n")
    cat("\n", qtitle, "\n")
    check_question(right = right_ans, wrong = wrong_ans, ...)
    cat("\n:::\n")
}