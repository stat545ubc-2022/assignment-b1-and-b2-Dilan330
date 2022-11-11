#' @title Average Height Difference Function
#' @description The `Avg_height_diff` function calculates the absolute difference of the average adult height per sex (178.1 cm for Canadian men & 164.7 cm for Canadian women) to an inputted height (in cm).
#' @param height Height of person (in cm). I named this paramater as height because that is name the input.
#' @param male Variable to indicate the sex of the inputted height. I named this paramater as male because that corresponds to the input.
#' @return A message (class type: Character) which contains the absolute value of the difference of the average height and the inputted height (in cm)
#' @examples
#' Avg_height_diff(181, male = TRUE)
#' Avg_height_diff(114, male = FALSE)
#' @export
Avg_height_diff <- function(height, male = TRUE){
  if(!is.numeric(height)) {
    stop('This function only works for numeric input!\n',
         'You have provided an object of class: ', class(height)[1])
  }
  if(is.na(height)) {
    stop('This function only works for numeric input!\n',
         'You have provided an NA value ')
  }
  if(male == TRUE) {
    result <- abs(178.1 - height)
    paste("With the average Canadian male height at 178.1 cm, you are", round(result, 2), "cm different than the average.")
  } else {
    result2 <- abs(164.7 - height)
    paste("With the average Canadian female height at 164.7 cm, you are", round(result2, 2),  "cm different than the average.")
  }
}
