#' Make_Library
#'
#' This function extract the basic information of the books from the popular portal of library www.lubimyczytac.pl like: Title, Authors,ISBN,Rating, Date Read, Shelves, Review.
#' @param URL generated adress URL to your own library from www.lubimyczytac.pl (example -> "https://lubimyczytac.pl/ksiegozbior/9YsJPg7kuPc")
#' @param num_pages the numbers of the pages in your library. Add extra 2 to number of pages.
#'
#' @return data frame with all necessary information about books in library
#' @export
#'
#' @examples book_df <- LibBookLC::Make_Library(URL = "https://lubimyczytac.pl/ksiegozbior/9YsJPg7kuPc", num_pages = 30)
Make_Library <- function(URL, num_pages) { #add extra 2 to number of pages
  # Start a Selenium WebDriver session
  driver <-
    rsDriver(
      # browser = "chrome",
      # chromever = "latest",
      # verbose = FALSE
    )
  remote_driver <- driver$client

  first_page_url <- URL
  remote_driver$navigate(first_page_url)
  Sys.sleep(5)  # Allow the page to load

  # Locate and click the "Accept" button by ID
  accept_button <-
    remote_driver$findElement(using = "css selector", value = ".banner-actions-container #onetrust-accept-btn-handler")
  accept_button$clickElement()

  # Initialize vectors to store data
  titles <- c()
  authors <- c()
  isbns <- c()
  shelfs <- c()
  my_reviews <- character()
  my_ratings <- character()
  my_dates_read <- c()

  # Initialize an empty data frame to store the data
  book_data <- data.frame(
    Title = character(),
    Author = character(),
    ISBN = character(),
    "My Rating" = character(),
    "Date Read" = character(),
    Shelves = character(),
    "My Review" = character(),
    stringsAsFactors = FALSE
  )

  # Timer to measure the execution time
  start_time <- Sys.time()

  # base_href <- read_html(URL) %>% html_node("base") %>% html_attr("href")
  for (p in 1:num_pages) {
    # Read the HTML content of the current page
    page_content <- remote_driver$getPageSource()[[1]]
    page_content <- read_html(page_content)


    # Extract all book elements from the current page
    book_elements <-
      page_content %>% html_nodes(".authorAllBooks__single")

    # Loop through book elements to extract titles, authors, ISBNs, and subpage URLs
    for (i in seq_along(book_elements)) {
      title <-
        book_elements[i] %>% html_node(".authorAllBooks__singleTextTitle") %>% html_text(trim = TRUE)
      author <-
        book_elements[i] %>% html_node(".authorAllBooks__singleTextAuthor") %>% html_text(trim = TRUE)
      shelf <-
        book_elements[i] %>% html_node("div.authorAllBooks__singleTextShelfRight") %>% html_text(trim = TRUE)

      titles <- c(titles, title)
      authors <- c(authors, author)
      shelfs <- c(shelfs, shelf)
    }

    for (i in seq_along(book_elements)) {
      # Extract the subpage URL for each book
      subpage_url <-
        book_elements[i] %>% html_node(".authorAllBooks__singleTextTitle") %>% html_attr("href")


      # Read the HTML content of the subpage
      subpage <-
        read_html(paste0("https://lubimyczytac.pl", subpage_url))

      # Extract the ISBN from the subpage
      isbn <-
        subpage %>% html_node("dt:contains('ISBN:') + dd") %>% html_text()
      #subpage_urls <- c(subpage_urls, subpage_url)
      isbns <- c(isbns, isbn)
    }


    # Iterate through the rating elements
    for (i in seq_along(book_elements)) {
      rating_text <-
        book_elements[i] %>% html_elements(".listLibrary__ratingText") %>% html_text(trim = TRUE)

      if (grepl("Ocenił na:", rating_text[2])) {
        # If "Twoja ocena:" is present, extract the user's rating
        rating <-
          book_elements[i] %>% html_elements(".listLibrary__ratingStarsNumber") %>% html_text(trim = TRUE)
        # Extract only the first character (e.g., 8 from "8", 7 from "7")
        rating <- rating[2]
      } else {
        # If "Twoja ocena:" is not present, assign NA
        rating <- NA
      }

      # Store the rating in the vector
      my_ratings <- c(my_ratings, rating)

      # Extract reviews if they exist, otherwise assign NA
      review_elements <-
        book_elements[i] %>% html_elements(".expandTextNoJS")
      if (length(review_elements) > 0) {
        # Extract and concatenate reviews if available
        reviews <-
          paste(review_elements %>% html_text2(), collapse = "\n")
      } else {
        # Assign NA if no reviews found
        reviews <- NA
      }
      my_reviews <- c(my_reviews, reviews)

      # Extract the date of read element
      date_read_element <-
        book_elements[i] %>% html_node("div.small.grey")  # Adjust the selector as needed

      # Check if the date of read element exists
      if (!is.null(date_read_element)) {
        # Extract the text from the date of read element
        date_read_text <- date_read_element %>% html_text(trim = TRUE)

        # Extract the date part using regex
        date_read <-
          stringr::str_match(date_read_text, "([0-9]{4}-[0-9]{2}-[0-9]{2})")

        if (!is.null(date_read) && length(date_read) > 1) {
          # Store the date of read in the vector
          dates_read <- date_read[2]
        } else {
          # If the date extraction fails, assign NA
          dates_read <- NA
        }
      } else {
        # If the date of read element doesn't exist, assign NA
        dates_read <- NA
      }
      my_dates_read <- c(my_dates_read, dates_read)
    }

    # Append the data from the current page to the book_data data frame
    page_data <- data.frame(
      Title = titles,
      Author = authors,
      ISBN = isbns,
      "My Rating" = my_ratings,
      "Date Read" = my_dates_read,
      "Exclusive Shelf" = shelfs,
      "My Review" = my_reviews,
      stringsAsFactors = FALSE
    )

    book_data <- rbind(book_data, page_data)

    # Click the "Next" button to navigate to the next page
    next_page_button <-
      remote_driver$findElement(using = "css selector", value = ".next-page a.page-link")

    if (!is.null(next_page_button)) {
      next_page_button$clickElement()
      Sys.sleep(5)  # Allow the next page to load
    } else {
      break  # No more pages to navigate to
    }
  }

  #Filter df by unique values
  book_data <- book_data %>% distinct()

  # Calculate execution time
  end_time <- Sys.time()
  execution_time <- end_time - start_time

  # Print the execution time
  cat("Execution time:", execution_time, "seconds\n")

  # Stop the WebDriver session
  remote_driver$close()

  # Quit the Selenium WebDriver process
  driver$server$stop()

  return(book_data)
}
