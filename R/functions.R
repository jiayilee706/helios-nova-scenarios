#### Helper functions used by the figure and table scripts


# ===== P-value formatting ===========================================================================================

# Tables: 0.0001 or below as "<0.0001", otherwise 2 significant figures
fmt_pval <- function(p, append_p = FALSE) {

  out <- ifelse(is.na(p), NA_character_,
                ifelse(p < 0.0001, "<0.0001", {x <- signif(p, 2)
                s <- formatC(x, format = "f", digits = 4)
                sub("\\.?0+$", "", s)}))

  if (append_p) out <- ifelse(is.na(out), NA_character_,
                              ifelse(p < 0.0001, "P < 0.0001", paste0("P = ", out)))

  out
}

# Figures using P-values with scientific notation (ggtext markdown): P = 0.24, P = 0.004, P = 3.2 x 10^-8
fmt_p_sci <- function(p) {
  case_when(
    p < 1e-300 ~ "&lt; 1 × 10<sup>−300</sup>",
    p < 0.001  ~ {
      sci  <- formatC(p, format = "e", digits = 1)
      mant <- sub("e.*", "", sci)
      expo <- as.integer(sub(".*e", "", sci))
      paste0("= ", mant, " × 10<sup>−", abs(expo), "</sup>")
    },
    p < 0.01   ~ paste("=", sprintf("%.3f", p)),
    TRUE       ~ paste("=", sprintf("%.2f", p))
  )
}

# beta (lcl, ucl) with true minus signs
fmt_betaci <- function(beta, lcl, ucl, digits = 3) {
  f <- function(x) sub("^-", "−", formatC(x, format = "f", digits = digits))
  paste0(f(beta), " (", f(lcl), ", ", f(ucl), ")")
}


# ===== Formatted Excel tables =======================================================================================

# Writes a data frame to a formatted sheet (creates the file or adds/replaces the sheet)
#   title           : table title written above the table
#   header          : character matrix of header rows (NULL = use column names)
#   header_merges   : list of c(row1, col1, row2, col2), relative to header block / table columns
#   flag_cols       : helper columns used for formatting (bold_col etc.) that are NOT written out
#   merge_col       : flag column, rows with 1 are merged across all table columns
#   halign_right_col: flag column, rows with 1 are right aligned
#   cell_fills      : matrix (nrow(df) x written columns) of hex colours, NA = no fill
#   num_fmt         : named vector, column name = Excel number format (e.g. c(r2 = "0.0"))
#   footnotes       : character vector, one footnote per row under the table
#   footnote_fills  : hex colour per footnote (NA = none), used for colour legends
#   footnote_merge  : merge each long footnote across table width and wrap text
#   col_widths      : "auto" or numeric vector of widths for each written column
#   replace_sheet   : delete and recreate the sheet if it already exists (avoids leftover styles/merges)

write_sheets <- function(df, file, sheet_name = NULL, startRow = 1, startCol = 1,
                         font_name = "Times New Roman", font_size = 8,
                         halign_left_cols = 1, shade_col = NULL,
                         italic_col = NULL, bold_col = NULL,
                         bottomline_col = NULL,
                         title = NULL, header = NULL, header_merges = NULL,
                         flag_cols = NULL, merge_col = NULL, halign_right_col = NULL,
                         cell_fills = NULL, num_fmt = NULL,
                         footnotes = NULL, footnote_fills = NULL, footnote_merge = FALSE,
                         col_widths = "auto", replace_sheet = FALSE) {

  if (!is.data.frame(df)) stop("Input must be a data frame")
  if (is.null(sheet_name)) sheet_name <- deparse(substitute(df))
  for (fc in c(shade_col, italic_col, bold_col, bottomline_col, merge_col, halign_right_col)) {
    if (!fc %in% names(df)) stop(paste0('"', fc, '" not found in dataframe'))
  }

  loaded <- file.exists(file)
  wb <- if (loaded) openxlsx::loadWorkbook(file) else openxlsx::createWorkbook()
  if (replace_sheet && sheet_name %in% names(wb)) openxlsx::removeWorksheet(wb, sheet_name)
  if (!(sheet_name %in% names(wb))) openxlsx::addWorksheet(wb, sheet_name, gridLines = FALSE)

  # Data to write (helper/flag columns dropped)
  out_df <- as.data.frame(df[, setdiff(names(df), flag_cols), drop = FALSE])
  if (is.null(header)) header <- matrix(names(out_df), nrow = 1)
  if (ncol(header) != ncol(out_df)) stop("header must have the same number of columns as the written data")

  ## POSITION HELPERS
  title_row  <- startRow
  hdr_start  <- startRow + !is.null(title)
  hdr_rows   <- hdr_start:(hdr_start + nrow(header) - 1)
  hdr_end    <- max(hdr_rows)
  body_rows  <- (hdr_end + 1):(hdr_end + nrow(out_df))
  final_row  <- hdr_end + nrow(out_df)
  data_cols  <- startCol:(startCol + ncol(out_df) - 1)
  left_cols_absolute <- startCol + halign_left_cols - 1

  ## WRITE TITLE, HEADER, DATA
  if (!is.null(title)) openxlsx::writeData(wb, sheet_name, title, startRow = title_row, startCol = startCol)
  openxlsx::writeData(wb, sheet_name, as.data.frame(header), startRow = hdr_start, startCol = startCol, colNames = FALSE)
  openxlsx::writeData(wb, sheet_name, out_df, startRow = hdr_end + 1, startCol = startCol, colNames = FALSE, rowNames = FALSE)

  ## STYLES
  cs <- openxlsx::createStyle
  body_style <- cs(fontName = font_name, fontSize = font_size, valign = "center", halign = "center")
  header_style <- cs(fontName = font_name, fontSize = font_size, textDecoration = "bold", valign = "center", halign = "center", wrapText = TRUE)
  left_align_style <- cs(halign = "left")
  right_align_style <- cs(halign = "right")
  shade_style <- cs(fgFill = "#F2F2F2")
  italic_style <- cs(textDecoration = "italic")
  bold_style <- cs(textDecoration = "bold")
  topline_style <- cs(border = "top")
  bottomline_style <- cs(border = "bottom")
  add_style <- function(style, rows, cols, grid = TRUE)
    openxlsx::addStyle(wb, sheet_name, style, rows = rows, cols = cols, gridExpand = grid, stack = TRUE)

  ## APPLY BASE STYLES
  if (!is.null(title))
    add_style(cs(fontName = font_name, fontSize = font_size + 1, textDecoration = "bold", halign = "left"), title_row, startCol)
  if (nrow(out_df) > 0) add_style(body_style, body_rows, data_cols)
  add_style(header_style, hdr_rows, data_cols)
  add_style(topline_style, hdr_start, data_cols)
  add_style(bottomline_style, hdr_end, data_cols)
  add_style(bottomline_style, final_row, data_cols)

  ## HEADER MERGES
  for (m in header_merges) {
    openxlsx::mergeCells(wb, sheet_name, rows = hdr_start + (m[1]:m[3]) - 1, cols = startCol + (m[2]:m[4]) - 1)
  }

  ## LEFT ALIGN OVERRIDE
  if (!is.null(halign_left_cols) && length(halign_left_cols) > 0)
    add_style(left_align_style, c(hdr_rows, body_rows), left_cols_absolute)

  ## FLAGGED ROWS (shade, italic, bold, bottom line, merge, right align)
  flag_rows <- function(col) which(df[[col]] == 1) + hdr_end
  if (!is.null(shade_col)) add_style(shade_style, flag_rows(shade_col), data_cols)
  if (!is.null(italic_col)) add_style(italic_style, flag_rows(italic_col), data_cols)
  if (!is.null(bold_col)) add_style(bold_style, flag_rows(bold_col), data_cols)
  if (!is.null(bottomline_col)) add_style(bottomline_style, flag_rows(bottomline_col), data_cols)
  if (!is.null(halign_right_col)) add_style(right_align_style, flag_rows(halign_right_col), data_cols)
  if (!is.null(merge_col)) for (r in flag_rows(merge_col)) openxlsx::mergeCells(wb, sheet_name, rows = r, cols = data_cols)

  ## CELL FILLS (cell by cell, e.g. for shading relative differences)
  if (!is.null(cell_fills)) {
    if (!all(dim(cell_fills) == dim(out_df))) stop("cell_fills must have the same dimensions as the written data")
    for (colour in setdiff(unique(as.vector(cell_fills)), NA)) {
      idx <- which(cell_fills == colour, arr.ind = TRUE)
      add_style(cs(fgFill = colour), idx[, 1] + hdr_end, idx[, 2] + startCol - 1, grid = FALSE)
    }
  }

  ## NUMBER FORMATS
  for (nm in names(num_fmt)) {
    add_style(cs(numFmt = num_fmt[[nm]]), body_rows, startCol + match(nm, names(out_df)) - 1)
  }

  ## COLUMN WIDTHS
  openxlsx::setColWidths(wb, sheet_name, cols = data_cols, widths = col_widths)

  ## FOOTNOTES
  if (!is.null(footnotes)) {
    fn_rows <- final_row + seq_along(footnotes)
    openxlsx::writeData(wb, sheet_name, data.frame(x = footnotes), startRow = final_row + 1, startCol = startCol, colNames = FALSE)
    add_style(cs(fontName = font_name, fontSize = font_size, halign = "left", valign = "top", wrapText = footnote_merge),
              fn_rows, startCol)
    if (footnote_merge) {
      # Approximate row height from text length (merged cells do not autofit in Excel)
      tab_width <- if (is.numeric(col_widths)) sum(col_widths) else 12 * length(data_cols)
      # Only long footnotes are merged (short ones, e.g. colour legend, stay in one cell)
      for (i in which(nchar(footnotes) > tab_width)) {
        openxlsx::mergeCells(wb, sheet_name, rows = fn_rows[i], cols = data_cols)
        n_lines <- max(1, ceiling(nchar(footnotes[i]) / tab_width))
        if (n_lines > 1) openxlsx::setRowHeights(wb, sheet_name, rows = fn_rows[i], heights = n_lines * font_size * 1.35)
      }
    }
    if (!is.null(footnote_fills)) {
      for (i in which(!is.na(footnote_fills))) add_style(cs(fgFill = footnote_fills[i]), fn_rows[i], startCol)
    }
  }

  # Keep sheets in name order (so re-running one script doesn't shuffle STable5/STable6)
  openxlsx::worksheetOrder(wb) <- order(names(wb))

  # Workaround for openxlsx bug: after loadWorkbook(), empty drawing parts are not re-saved but their
  # relationships are kept, so Excel may ask to "repair" the file. Remove relationships to empty drawings/comments.
  if (loaded) {
    for (i in seq_along(wb$worksheets_rels)) {
      if (length(wb$drawings[[i]]) == 0)
        wb$worksheets_rels[[i]] <- wb$worksheets_rels[[i]][!grepl("relationships/drawing\"", wb$worksheets_rels[[i]])]
      if (length(wb$comments[[i]]) == 0)
        wb$worksheets_rels[[i]] <- wb$worksheets_rels[[i]][!grepl("relationships/vmlDrawing\"", wb$worksheets_rels[[i]])]
    }
  }

  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)

  message(sprintf('Dataframe written to sheet "%s" in %s.', sheet_name, file))
}
