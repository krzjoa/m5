#' Taken from https://github.com/trnnick/tsutils/blob/master/R/leadtrail.R
#'
#' Remove leading/training zeros/NAs
#'
#' Remove leading or trailing zeros or NAs from a vector.
#'
#' @param x vector of values to check.
#' @param rm what to remove, can be \code{"zeros"} or \code{"na"}.
#' @param lead If \code{TRUE}, then leading values are removed.
#' @param trail If \code{TRUE}, then trailing values are removed.
#'
#' @return Resulting vector.
#'
#' @author Nikolaos Kourentzes, \email{nikolaos@kourentzes.com}.
#'
#' @examples
#' x <- c(rep(0,5),rnorm(100),rep(0,5))
#' leadtrail(x)
#'
leadtrail <- function(x,rm=c("zeros","na"),lead=c(TRUE,FALSE),trail=c(TRUE,FALSE)){

  # Defaults
  rm <- match.arg(rm,c("zeros","na"))
  lead <- lead[1]
  trail <- trail[1]

  # Select what to remove
  if (rm=="zeros"){
    idx <- which(x == 0)
  } else {
    idx <- which(is.na(x))
  }

  n <- length(x)
  l <- length(idx)

  # Handle leading observations
  if (lead==TRUE & l>0){

    if (idx[1]==1){
      d.idx <- diff(idx)
      loc <- which(d.idx > 1)[1]
      if (is.na(loc)){
        loc <- l
      }
      lead.rm <- 1:loc
    } else {
      lead.rm <- NULL
    }

  } else {
    lead.rm <- NULL
  }

  # Handle trailing observations
  if (trail==TRUE & l>0){

    if (tail(idx,1)==n){
      d.idx <- diff(rev(idx))
      loc <- which(d.idx != -1)[1]
      if (is.na(loc)){
        loc <- l
      }
      trail.rm <- (n-loc+1):n
    } else {
      trail.rm <- NULL
    }

  } else {
    trail.rm <- NULL
  }

  keep <- rep(TRUE,n)
  keep[lead.rm] <- FALSE
  keep[trail.rm] <- FALSE

  y <- x[keep]
  return(y)

}



#' https://deep-and-shallow.com/2020/10/07/forecast-error-measures-intermittent-demand/
#' Helper functions
#' Consider moving them into a separate package
adi <- function(x, ...){
  # sequences <- rle(x > 0)
  # n_seq <- length(sequences$values)
  # non_zero_seq  <- sum(sequences$values)
  # n_seq / non_zero_seq
  length(x) / sum(x > 0)
}

#' CV²
cv2 <- function(x, ...){
  x <- x[x > 0]
  (sd(x) / mean(x)) ^ 2
}


#' Classify time series of the particular items
#'
#' Each time series in the dataset can be assigned one of the following classes:
#'
#' * Smooth (ADI < 1.32 and CV² < 0.49).
#' * Intermittent  (ADI >= 1.32 and CV² < 0.49)
#' * Erratic (ADI < 1.32 and CV² >= 0.49)
#' * Lumpy (ADI >= 1.32 and CV² >= 0.49)

#' @param data The result of the `m5_prepare` function.
#'
#' @returns
#' A `data.frame` containing item ids, ADI and CV2 scores as well as the final
#' class chosen based on the aforementioned scores.
#'
#'
#' @references
#' \dontrun{
#' m5_download('data')
#' c(sales_train,
#'   sales_test,
#'   sell_prices,
#'   calendar) %<-% m5_get_raw_evaluation('data')
#'
#' m5_data  <-
#'    m5_prepare(sales_train, sales_test, calendar, sell_prices)
#' }
#'
#' m5_demand <- m5_demand_type(m5_data)
#' }
#'
#' @export
m5_demand_type <- function(data){
  data[, .(
      # adi = adi(value),
      # cv2 = cv2(value),
      adi = adi(leadtrail(value, lead = TRUE, trail = FALSE)),
      cv2 = cv2(leadtrail(value, lead = TRUE, trail = FALSE))
    ), by = .(item_id, store_id)][
      , demand_type := fcase(
        adi < 1.32 & cv2 < 0.49, 'Smooth',
        adi >= 1.32 & cv2 < 0.49, 'Intermittent',
        adi < 1.32 & cv2 >= 0.49, 'Erratic',
        adi >= 1.32 & cv2 >= 0.49, 'Lumpy'
      )
    ]
}