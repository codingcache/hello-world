# hello-world

Hi Human!

Start Here:

File contents:

delta_hedge.R: back test time-fixed and volatility band strategies of delta hedge, with the sample of 3 minutes iron futures from I1709 to I1909. The data are named as iron3min.csv. The output is the total payoff on options and futures, as well as the average volatility of trading price.

delta_hedge_types.R: back test 37 types of commodities with the same strategies in delta_hedge.R, while change the notional principal from one board lot to ￥10,000,000, and change the computing method of volatility by excluding open and close price. The data are in file /futures data; the results are in file /results, and the file types.xls lists detailed parameters of the data used.
