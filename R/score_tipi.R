
#' Score the Ten-Item Personality Inventory
#'
#' Calculate five personality trait means from ten TIPI items.
#' @param data A data frame containing numeric item responses.
#' @param items Character vector of ten column names in TIPI item order.
#'   NULL detects TRAITS1 through TRAITS10, with or without _Y1 suffixes.
#' @param coding Either "gfs" (1 = strongly agree) or "standard"
#'   (7 = strongly agree).
#' @return The input data frame with five additional numeric columns:
#'   extraversion, agreeableness, conscientiousness, emotional_stability,
#'   and openness.
#' @details Both items must be observed for each trait. Scores range from
#'   1 to 7. GFS missing codes -98, 98 and 99 are converted to NA for
#'   scoring only. Other invalid responses cause an error. Baseline items
#'   retained in long GFS data produce baseline scores on each row, not
#'   repeated personality measurements. Original items are unchanged.
#' @references Gosling, S. D., Rentfrow, P. J., and Swann, W. B. (2003).
#'   A very brief measure of the Big-Five personality domains.
#'   Journal of Research in Personality, 37, 504-528.
#'   doi:10.1016/S0092-6566(03)00046-1.
#' @examples
#' d <- as.data.frame(as.list(stats::setNames(rep(4, 10), paste0("TRAITS", 1:10))))
#' score_tipi(d)
#' @export
score_tipi <- function(data, items = NULL, coding = c("gfs", "standard")) {
  coding <- match.arg(coding)
  if (!is.data.frame(data) || anyDuplicated(names(data))) {
    stop("data must be a data frame with unique column names.")
  }
  if (is.null(items)) {
    candidates <- paste0("TRAITS", 1:10)
    suffixed <- paste0(candidates, "_Y1")
    plain <- all(candidates %in% names(data))
    baseline <- all(suffixed %in% names(data))
    if (plain && baseline) stop("Both item sets found; specify items explicitly.")
    items <- if (baseline) suffixed else candidates
  }
  if (!is.character(items) || length(items) != 10L || anyNA(items) ||
      anyDuplicated(items) || !all(items %in% names(data))) {
    stop("items must identify ten distinct existing columns in TIPI order.")
  }
  traits <- c("extraversion", "agreeableness", "conscientiousness",
              "emotional_stability", "openness")
  if (any(traits %in% names(data))) stop("Trait score columns already exist.")
  x <- lapply(items, function(item) {
    v <- data[[item]]
    if (!is.numeric(v) || is.matrix(v)) stop("Non-numeric item: ", item)
    v <- as.numeric(v)
    if (coding == "gfs") v[v %in% c(-98, 98, 99)] <- NA_real_
    if (any(!is.na(v) & !v %in% 1:7)) stop("Invalid response in ", item)
    v
  })
  reverse <- if (coding == "gfs") c(1, 3, 5, 7, 9) else c(2, 4, 6, 8, 10)
  for (i in reverse) x[[i]] <- 8 - x[[i]]
  for (i in 1:5) data[[traits[i]]] <- (x[[i]] + x[[i + 5]]) / 2
  data
}
