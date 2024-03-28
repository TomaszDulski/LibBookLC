#' pre_format_GR
#'
#' This function will partly transform the data generated from www.lubimyczytac.pl which are acquired for www.goodreads.com. Function changes the system books rating and status of the book from polish to english.
#' @param raw_data input generated from Make_library function
#'
#' @return Data frame data frame with partly formated information about books in library for portal www.goodreads.com
#' @export
#'
#' @examples pre_format_book_df <- pre_format_GR(raw_data = book_df)
pre_format_GR <- function(raw_data){
  # Change dots to white spaces in colnames
  colnames(raw_data) <- gsub("\\.", " ", colnames(raw_data))
  #put everything after comma in shelfs to another, delete commas and ordered
  raw_data$Bookshelves <- str_extract(raw_data$`Exclusive Shelf`, "(?<=,)[^,]+")
  raw_data <- raw_data[c(1:7)]
  raw_data$`Exclusive Shelf` <- sub(",.*", "", raw_data$`Exclusive Shelf`)
  # change Shelf to english
  raw_data$`Exclusive Shelf` <-
    ifelse(
      raw_data$`Exclusive Shelf` == "Przeczytane",
      "read",
      ifelse(
        raw_data$`Exclusive Shelf` == "Chcę przeczytać",
        "to-read",
        ifelse(raw_data$`Exclusive Shelf` == "Teraz czytam", "currently-reading", raw_data$`Exclusive Shelf`)
      )
    )

  #remove all NA to empty rows
  raw_data[is.na(raw_data)] <- ""

  #testing changing rates
  raw_data[4] <-
    ifelse(raw_data[4] >= 9, 5,
           ifelse(
             raw_data[4] >= 7,
             4,
             ifelse(
               raw_data[4] >= 5 & raw_data[4] <= 6,
               3,
               ifelse(
                 raw_data[4] >= 3 & raw_data[4] <= 5,
                 2,
                 ifelse(raw_data[4] >= 1 &
                          raw_data[4] <= 2, 1, NA)
               )
             )
           ))

  raw_data[4][raw_data[4] == 1] <- 5
  raw_data[4][is.na(raw_data[4])] <- ""

  #convert date
  raw_data$`Date Read` <- gsub("-", "/", raw_data$`Date Read`)
  return(raw_data)
}

#' GR_df_matrix
#'
#'Function create the empty data frame matrix according to sample table rom GoodReads website.
#' @param num_books insert the number of book in your whole library
#'
#' @return empty data frame with appropriate columns name for GoodReads.
#' @export
#'
#' @examples GR_df_empty <- GR_df_matrix(num_books = 551)
GR_df_matrix <- function(num_books){
  GD_df <- data.frame(
    "Book Id" = character(num_books),
    "Title" = character(num_books),
    "Author" = character(num_books),
    "Author l - f" = character(num_books),
    "Additional Authors" = character(num_books),
    "ISBN" = character(num_books),
    "ISBN13" = character(num_books),
    "My Rating" = character(num_books),
    "Average Rating" = character(num_books),
    "Publisher" = character(num_books),
    "Binding" = character(num_books),
    "Number of Pages" = character(num_books),
    "Year Published" = character(num_books),
    "Original Publication Year" = character(num_books),
    "Date Read" = character(num_books),
    "Date Added" = character(num_books),
    "Bookshelves" = character(num_books),
    "Bookshelves with positions" = character(num_books),
    "Exclusive Shelf" = character(num_books),
    "My Review" = character(num_books),
    "Spoiler" = character(num_books),
    "Private Notes" = character(num_books),
    "Read Count" = character(num_books),
    "Owned Copies" = character(num_books)
  )
  return(GD_df)
}

#' goodREADS_data
#'
#' @param GD_df input generated from GR_df_matrix function
#' @param book_data input generated from pre_format_GR function
#'
#' @return ready Table for Good Reads Library
#' @export
#'
#' @examples ready_table <- goodREADS_data(GD_df = GR_df_empty, book_data = pre_format_book_df)
goodREADS_data <- function(GD_df, book_data) {
  colnames(GD_df) <- gsub("\\.", " ", colnames(GD_df))

  # Copy data from book_data to GD_df for matching column names
  matching_cols <- intersect(colnames(GD_df), colnames(book_data))
  GD_df[, matching_cols] <- book_data[, matching_cols]


  # Set "My Rating" to 0 only where it's empty
  GD_df$`My Rating`[GD_df$`My Rating` == ""] <- 0

  # Assign the formatted date to the "Date Added" column for all rows
  today_date <- Sys.Date()
  formatted_date <- format(today_date, "%Y/%m/%d")
  GD_df$`Date Added` <- formatted_date


  #Copy rows to other shelves

  to_read_or_reading <- GD_df$`Exclusive Shelf` %in% c("to-read", "currently-reading")
  GD_df$`Bookshelves`[to_read_or_reading] <- GD_df$`Exclusive Shelf`[to_read_or_reading]
  GD_df$`Bookshelves with positions`[to_read_or_reading] <- GD_df$`Exclusive Shelf`[to_read_or_reading]

  ############add positions################

  # Create a new column "Status" with empty values
  GD_df$Status <- ""

  # Initialize counters for "to-read" and "currently-reading"
  to_read_counter <- 1
  reading_counter <- 1

  # Add "to-read" and "currently-reading" statuses in descending order
  for (i in rev(seq_along(to_read_or_reading))) {
    if (to_read_or_reading[i]) {
      status <- GD_df$`Exclusive Shelf`[i]
      if (status == "to-read") {
        GD_df$Status[i] <- paste("to-read (#", to_read_counter, ")", sep = "")
        to_read_counter <- to_read_counter + 1
      } else if (status == "currently-reading") {
        GD_df$Status[i] <- paste("currently-reading (#", reading_counter, ")", sep = "")
        reading_counter <- reading_counter + 1
      }
    }
  }

  # Update "Bookshelves with positions" with the "Status" column
  GD_df$`Bookshelves with positions` <- GD_df$Status
  # Remove the "Status" column
  GD_df$Status <- NULL
  ########################################################

  #add read count
  GD_df$`Read Count` <- ifelse(GD_df$`Exclusive Shelf` %in% c("read", "currently-reading"), 1, 0)


  #check duplications of title

  duplicates <- duplicated(GD_df$Title)

  if (any(duplicates)) {
    duplicate_rows <- GD_df[duplicates, ]
    print(duplicate_rows)
  } else {
    cat("No duplicates found in the Title column.\n")
  }
  return(GD_df)
}
