Life Insurance reserves are key to risk management in insurance companies, failure to adequately prepare for predictable and random losses can be otherwise fatal.
This project simplifies modelling for a life insurance company, using modern actuarial models and real data.

Full project [summary](LifeInsuranceReserves.pdf) available in LifeInsuranceReserves.pdf

The project simulates randomly generated portfolio of policyholders and estimates:
 - Their net premiums,
 - Life insurance reserves over time.

The project uses the lifecontingencies package, as well as manual Vasicek modelling for stochastic interest rates. The code itself is generalisable, changing the data in data/ and altering the original readxl calls in Main.R will permit calculation for any set of mortality tables or bank spot rates. Minor adjustments to portfolio generation would allow the calculator to be applied to any portfolio of policyholders.

/data holds the data for this project, as downloaded from the Office of National Statistics (ONS) and Bank of England (BoE).

Methodology:

Data Sources:
 - Mortality tables are taken from the ONS, the latest as per October 2025.
 - Yield curves were taken from the BoE, the latest as per October 2025. The Vasicek model was then applied to pull long run interest rate values and make projections.


Policyholder Generation:
 - Native Monte Carlo sampling on R was used to generate a portfolio of 1,000 people.
 - Attributes were general, such as age, sum-assured, term, gender, driving license, etc.


Premium Calculation:
 - Using the equivalence principle: P = S * A_x / a[x]:n, we can calculate the premium on the policy.
 - The Axn and axn functions in lifecontingencies simplify calculations significantly.


Reserve Calculation:
 - The Actuarial Reserve formula: V_x = S * A_(x+t) - P_x * a_(x+t), lets us calculate the prospective reserves.
 - The Vasicek function provides interest rates and lifecontingencies supplies Axn and axn.
 


Key Code Sections in src/Main.R:

  Mortality table - Line 26
  Yield Curve and Vasicek Interest Rate modelling - Line 46
  Portfolio of policyholder generation - Line 148
  Premium Calculation - Line 162
  Calculating Actuarial Reserve - Line 220