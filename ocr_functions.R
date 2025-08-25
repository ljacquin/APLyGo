# function to clean text from ocr
clean_text_ <- function(text_) {
  # split into lines
  lines <- unlist(strsplit(text_, "\n"))

  # clean whitespace
  lines_clean <- trimws(lines)
  lines_clean <- gsub("\\s+", " ", lines_clean)

  # remove lines that look like nonsense
  is_nonsense <- function(line) {
    # remove if line is empty
    if (nchar(line) == 0) {
      return(TRUE)
    }

    # remove if mostly symbols (less than 40% letters/numbers)
    chars <- unlist(strsplit(line, ""))
    alnum_ratio <- sum(grepl("[A-Za-z0-9]", chars)) / length(chars)
    if (alnum_ratio < 0.4) {
      return(TRUE)
    }

    # remove if too short and not meaningful
    if (nchar(line) < 3) {
      return(TRUE)
    }

    # remove specific junk patterns
    junk_patterns <- c("^~~\\)", "^QUE £", "^an$", "^\\|$")
    if (any(sapply(junk_patterns, function(pat) grepl(pat, line, ignore.case = TRUE)))) {
      return(TRUE)
    }

    return(FALSE)
  }

  lines_filtered <- lines_clean[!vapply(lines_clean, is_nonsense, logical(1))]

  # remove exact duplicates
  lines_unique <- unique(lines_filtered)

  # remove near-duplicates (OCR variations)
  similarity_threshold <- 0.95
  keep_flags <- rep(TRUE, length(lines_unique))

  for (i in seq_along(lines_unique)) {
    if (keep_flags[i]) {
      sim_scores <- stringsim(lines_unique[i], lines_unique, method = "cosine")
      near_duplicates <- which(sim_scores >= similarity_threshold)
      near_duplicates <- near_duplicates[near_duplicates > i]
      keep_flags[near_duplicates] <- FALSE
    }
  }

  # keep only non-duplicates
  final_lines <- lines_unique[keep_flags]

  # recombine
  final_text <- paste(final_lines, collapse = "\n")

  return(final_text)
}

# function which rotates image automatically
rotate_image_auto <- function(angles_ = c(0, 90, 180, 270),
                              image_, ocr_engine) {
  scores <- sapply(angles_, function(a) {
    rotated <- image_rotate(image_, a)
    txt <- ocr(rotated, engine = ocr_engine)
    nchar(gsub("\\s+", "", txt))
  })
  best_angle <- angles_[which.max(scores)]
  return(image_rotate(image_, best_angle))
}

# main function to extract text
extract_text_img_from_file <- function(file_path, rot_img_auto_ = T) {
  file_ext <- tolower(file_ext(file_path))
  ocr_engine <- tesseract("fra+eng") # french + english

  if (file_ext %in% c("jpg", "jpeg", "png", "tiff", "bmp")) {
    message("image file detected, applying ocr...")
    image <- image_read(file_path)
    if (rot_img_auto_) {
      image <- rotate_image_auto(
        image_ = image,
        ocr_engine = ocr_engine
      )
    }
    if (length(image) > 1) {
      extracted_text <- sapply(
        image, function(img) ocr(img, engine = ocr_engine)
      )
      extracted_text <- paste(extracted_text, collapse = "\n\n--- page ---\n\n")
    } else {
      extracted_text <- ocr(image, engine = ocr_engine)
    }
    return(
      list(
        "text_" = clean_text_(extracted_text),
        "img_" = image
      )
    )
  } else if (file_ext == "pdf") {
    message("pdf file detected")
    pdf_text_content <- pdf_text(file_path)
    extracted_text <- paste(pdf_text_content, collapse = "\n")

    if (nchar(gsub("\\s+", "", extracted_text)) > 20) {
      message("text detected and extracted from pdf")
      return(
        list(
          "text_" = clean_text_(extracted_text),
          "img_" = NULL
        )
      )
    } else {
      message("pdf with no detectable text, conversion to image before ocr...")
      images <- image_read_pdf(file_path, density = 300)
      ocr_text <- sapply(images, function(img) ocr(img, engine = ocr_engine))
      extracted_text <- paste(ocr_text, collapse = "\n\n--- next page ---\n\n")
      return(
        list(
          "text_" = clean_text_(extracted_text),
          "img_" = image
        )
      )
    }
  } else {
    stop("file extension non supported")
  }
}
