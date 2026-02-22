# CV Repository

This is a data-driven CV/resume built with R, pagedown, and the `datadrivencv`-inspired architecture.

## File Structure

- `resume.Rmd` — Main resume document (rendered to HTML/PDF)
- `CV_printing_functions.R` — CV printer object and helper functions
- `render_cv.R` — Script to render resume to HTML and PDF
- `dd_cv.css` — Main CV styles (fonts, colours, layout)
- `dd_resume.css` — Resume-specific layout overrides
- `entries.csv` — Work experience and education entries
- `contact_info.csv` — Contact details for the sidebar
- `text_blocks.csv` — Free-text blocks (intro paragraph etc.)
- `language_skills.csv` — Skill levels for skill bars (currently unused; skills are hardcoded in sidebar)

## How to Update the CV

Edit the CSV files in the repo root:

- **Add/change jobs or education** → edit `entries.csv`
- **Change contact details** → edit `contact_info.csv`
- **Change the intro paragraph** → edit `text_blocks.csv`

## How to Render

### Render HTML (preview)
```r
rmarkdown::render("resume.Rmd", params = list(pdf_mode = FALSE), output_file = "resume.html")
```

### Render PDF
```r
source("render_cv.R")
```

Or from the terminal:
```bash
Rscript render_cv.R
```

## Colour Scheme

- Sidebar background: `#96BF76` (green)
- Decorator/timeline: `#688B58` (dark green)
- Circle accent: `#499167` (mid green)

## Fonts

- Body: Montserrat
- Headers: Playfair Display
