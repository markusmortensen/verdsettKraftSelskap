

#' Title
#'
#' @param prisbane
#' @param produksjon
#' @param op_cost_kwh
#' @param inv_kwh
#' @param dep_kwh
#'
#' @import dplyr
#' @return returener noe
#' @export
#'
#' @examples
kalkuler_kostnader_inntekter <- function(prisbane,
                                         produksjon,
                                         op_cost_kwh = 0.3,
                                         inv_kwh = 0.2,
                                         dep_kwh = 0.2) {
  prisbane %>%
    mutate(op_cost = produksjon * op_cost_kwh,
                  inv = produksjon * inv_kwh,
                  dep = produksjon * dep_kwh,
                  inntekt = produksjon * power_price)
}

#' Title
#'
#' @param cost_df
#'
#' @return
#' @export
#'
#' @examples
kalkuler_fri_kontantflyt <- function(cost_df) {
  cost_df %>%
    mutate(ebit = inntekt - op_cost,
                  kontantflyt = ebit * (1 - .22) - inv + dep)
}

#' Title
#'
#' @param asset_beta
#' @param risk_free_rate
#' @param market_risk_premium
#' @param other_adj
#' @param debt_premium
#' @param debt_share
#' @param corp_income_tax
#' @param inflation
#'
#' @return
#' @export
#'
#' @examples
kalkuler_wacc <- function(asset_beta = .6,
                          risk_free_rate = .02,
                          market_risk_premium = .05,
                          other_adj = .02,
                          debt_premium = .025,
                          debt_share = 0.5,
                          corp_income_tax = .22,
                          inflation = .02) {
  equity_beta <- asset_beta * (1 + debt_share / (1 - debt_share))
  cost_of_equity <- risk_free_rate +
    other_adj +
    market_risk_premium * equity_beta
  cost_of_debt <- (risk_free_rate + debt_premium) * (1 - corp_income_tax)
  cost_of_capital <- (cost_of_equity * (1 - debt_share)) + cost_of_debt * debt_share
  real_cost_of_capital <- ((1 + cost_of_capital) / (1 + inflation)) - 1
}


#' Title
#'
#' @param df
#' @param wacc
#'
#' @return
#' @export
#'
#' @examples
kalkuler_selskapsverdi <- function(df, wacc) {
  sum(df$kontantflyt / (1 + wacc) ^ (1:nrow(df)))
}
