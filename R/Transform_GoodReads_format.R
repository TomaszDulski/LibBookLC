#' Transform_GoodReads_format
#'
#'The function will  format data from lubimyczytac.pl website ready to import to Good Reads library website
#' @param raw_data input generated from Make_library function
#' @param num_books insert the number of book in your whole library
#'
#' @return data frame ready to import to Good reads
#' @export
#'
#' @examples all_books <- Transform_GoodReads_format(raw_data = book_df, num_books = 551)
Transform_GoodReads_format <- function(raw_data,
                                       num_books
                                       ){
  res <- pre_format_GR(raw_data)
  matrix <- GR_df_matrix(num_books)
  all_books <- goodREADS_data(matrix, res)
  return(all_books)
}
