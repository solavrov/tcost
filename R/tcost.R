
#' Load liquidity data from files
#'
#' Attributes:
#' spreads - spreads in percentage;
#' terms - terms of bonds;
#' volumes - trade volumes
#'
#' @param file Names of csc data files without extansion
#' @param instrument Names of instruments
#'
#' @return Liquidity data object
#' @export
loadLiquidity <- function(file = c("liq_data/liq_t", "liq_data/liq_b"),
                          instrument = c("T", "B")) {

  liq <- list()

  len <- length(file)

  for (i in 1:len) {

    m <- as.matrix(
      read.csv(paste0(file[i],".csv"), header = FALSE)
      )

    d1 <- dim(m)[1]
    d2 <- dim(m)[2]

    rownames(m) <- paste0("t", 1:d1 - 1)
    colnames(m) <- paste0("v", 1:d2 - 1)

    liq[[instrument[i]]]$spreads <- m[2:d1, 2:d2]
    liq[[instrument[i]]]$terms <- m[2:d1, 1]
    liq[[instrument[i]]]$volumes <- m[1, 2:d2]

  }

  return (liq)

}


#' Return spread for a given bond trade
#'
#' @param term Bond's term to maturity in years
#' @param volume Volume of trade in millions
#' @param instrument Name of instrument
#' @param liquidity Liquidity data object
#'
#' @return Spread in percentage
#' @export
getSpread <- function(term, volume, instrument, liquidity) {

  len <- length(instrument)
  spread <- numeric()

  for (i in 1:len)
    spread[i] <- akima::bilinear(liquidity[[instrument[i]]]$term,
                                 liquidity[[instrument[i]]]$volumes,
                                 liquidity[[instrument[i]]]$spreads,
                                 term[i],
                                 volume[i])$z

  spread[which(spread == 0)] <- NA

  return (spread)

}


#' Return cost of trade
#'
#' @param volume Volume of trade in millions
#' @param spread Spread of trade in percentage
#'
#' @return Cost in millions
#' @export
getCost <- function(volume, spread) {
  volume * spread / 100 / 2
}


