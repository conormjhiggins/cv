# render_cv.R
# Script to render the resume to HTML and PDF

# Render resume to HTML
rmarkdown::render(
  "resume.Rmd",
  params = list(pdf_mode = FALSE),
  output_file = "resume.html"
)

# Render resume to PDF
tmp <- fs::file_temp(ext = ".html")
rmarkdown::render(
  "resume.Rmd",
  params = list(pdf_mode = TRUE),
  output_file = tmp
)
pagedown::chrome_print(input = tmp, output = "conor_resume.pdf")
