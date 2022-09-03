cat(getwd())
# Convert Rmd slides to HTML and PDF
for (f in dir(here::here("slides"), pattern = "^[0-1].*.Rmd", full.names = TRUE)) {
  html_file <- xfun::with_ext(f, "html")
  pdf_file <- xfun::with_ext(f, "pdf")
  if (!file.exists(html_file) || file_test("-ot", html_file, f)) {
    xfun::Rscript_call(rmarkdown::render, args = list(input = f))
  }
  if (!file.exists(pdf_file) || file_test("-ot", pdf_file, f)) {
    pagedown::chrome_print(html_file)
  }
}
