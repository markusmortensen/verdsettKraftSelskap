# verdsettKraftSelskap

<!-- badges: start -->
<!-- badges: end -->

Pakke brukt i R-kurs. Dette er en mulig løsning, men det er også andre måter pakken kan lages på. 

## Installasjon

Pakken kan installeres slik: 
``` r
install_packages("markusmortensen/verdsettKraftSelskap")
```

## Eksempel på bruk

This is a basic example which shows you how to solve a common problem:

``` r
library(verdsettKraftSelskap)

df <- hent_produksjonsdata()

kraftproduksjon <- hent_produksjon_for_kraftverk(df, "Novle")

wacc <- kalkuler_wacc()

kraftpriser %>% 
  kalkuler_kostnader_inntekter(kraftproduksjon) %>% 
  kalkuler_fri_kontantflyt() %>% 
  kalkuler_selskapsverdi(wacc)
```
## Merk
Det er dårlig testdekning av funksjonene i pakken. 
