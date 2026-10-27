# Scoring specification

- Entire vegetation: indicators 1–7
- Herbaceous vegetation: 8–17
- Woody perennials: 18–25
- Ecosystem functional integrity: 26–37

Ratings: 1=least degradation, 5=greatest degradation.

`q_i = 100*(5-r_i)/4`

`Q_g = sum(q_i in component g)/n_g`

`HQI_37 = (Q_entire + Q_herbaceous + Q_woody + Q_functional)/4`

All 37 valid ratings are mandatory for a final HQI. No imputation. Each component contributes 25% even though their numbers of indicators differ. The original `D_25 = sum(r_i for i=1..25)` is retained separately. Descriptive bands are provisional, not empirically validated.
