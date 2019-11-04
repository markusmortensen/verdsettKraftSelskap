

#' Legg til kostnader og inntekter
#'
#' Denne funksjonen tar i
#'
#' @param prisbane data.frame med forventet prisbane. Må inneholde en
#' kolonne med forventet pris. Hver rad representerer ett år.
#' @param produksjon Årlig kraftproduksjon i kwh
#' @param op_cost_kwh Operating costs per kwh
#' @param inv_kwh Investment per kwh
#' @param dep_kwh Avskrivning per kwh
#' @param priskolonne Navn på kolonnen i prisbane som inneholder forventet pris per år
#'
#' @import dplyr
#' @return Returnerer en data.frame med kolonnene op_cost, inv, dep og inntekt.
#' @export
#'
#' @examples
kalkuler_kostnader_inntekter <- function(prisbane,
                                         produksjon,
                                         op_cost_kwh = 0.3,
                                         inv_kwh = 0.2,
                                         dep_kwh = 0.2,
                                         priskolonne = power_price) {
  prisbane %>%
    mutate(op_cost = produksjon * op_cost_kwh,
                  inv = produksjon * inv_kwh,
                  dep = produksjon * dep_kwh,
                  inntekt = produksjon * {{priskolonne}})
}

#' Kalkulerer årlig fri kontantflyt
#'
#' Bør brukes i en pipeline etter \link{kalkuler_kostnader_inntekter}.
#'
#' @param cost_df data.frame som inneholder følgende felter:
#' dep, op_cost, inv og inntekt.
#'
#' @return Returnerer samme data.frame som i input, med to nye kolonner; ebit og kontantflyt
#' @export
#'
#' @examples
kalkuler_fri_kontantflyt <- function(cost_df) {
  cost_df %>%
    mutate(ebit = inntekt - op_cost,
                  kontantflyt = ebit * (1 - .22) - inv + dep)
}

#' Kalkulerer wacc med gitte inputverdier
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


#' Kalkuler nåverdien til selskapet
#'
#'
#' @param df data.frame som har en kolonne kalt kontantflyt. Hver rad representerer
#' et år, der første rad er det første året.
#' @param wacc En numerisk verdi for wacc.
#'
#' @return Returnerer nåverdien for et selskap.
#' @export
#'
#' @examples
kalkuler_selskapsverdi <- function(df, wacc) {
  sum(df$kontantflyt / (1 + wacc) ^ (1:nrow(df)))
}
