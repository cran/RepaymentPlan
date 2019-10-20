\name{RepaymentPlan}
\alias{RepaymentPlan}
\title{Calculation of Repayment Schedule under Mortgage Plan or Repayment Plan}

\description{
The function RepaymentPlan() calculates repayment schedules for various mortgage plans.
}
\usage{
RepaymentPlan(L, r, n, Last_year_due)
}
\arguments{
   \item{L}{Loan amount in the money terms. It could be dollar or any other unit of currency.}
  \item{r}{Rate of interest charges on loan amount annually in fraction. If instead of providing 'n' in years, it is provided in months then divide the rate by 12 or if 'n' is provied in quarters, then divide the rate by 4}
  \item{n}{No. of years in repayment plan or loan tenure.The loan tenure can also be provided in number of months and quarters but then one should not forget to adjust the rate of interest from annual to monthly or quarterly as needed.}
  \item{Last_year_due}{Amount Reserved for repayment in last year in case of only for partial repayment plan. Higher the value of this amount, greater is the cost of interest component in partial repayment plan. In case this argument is equal to principal sum then answer would be same as that of stragith end repayment plan}
}
\details{
This function will provide four repayment plans detailed in the research paper of Vani et. al.,2018. According to Reddy et al.,2004, there are mainly four types of repayment plans for mortgage loan. These are straight end repayment plan, partial or balloon repayment plan, amortized decreasing repayment plan and amortized even repayment plan. In case of crop loans which are repaid within a year, straight end repayment plan is preferred. This is an interest only repayment plan where borrower pays no part of principal for any of the year in loan tenure except for the last year. However, interest is paid for all the years in loan tenure.

In case of loan taken for orchard development or machinary loans, partial repayment plan is preferred. In this plan, borrower commits to pay a specified sum of money towards principal in the last year of loan tenure. In rest of the years, the principal component remains fixed and the interest is charged on the balance.Thus, installment varies over the loan tenure.

For machinary loans, amortized decreasing repayment plan is also used. Amortized even repayment plan is the most popular one which is used for working out EMI or equated monthly installment. In this plan, installment remains constant unlike other plans. This is also used for loans related to livestock, construction work, land development and diggin of wells.

This function returns all the five repayment plans along with the summary of all the plans by calculating the total principal, interest and installment amount for each plan. Additionally this function also plots the barplot of interest amount in each repayment plan.
}
\value{
  \item{Partial.Repayment.Plan}{Repayment Schedule under Partial Repayment plan}
  \item{Amortized.Decreasing.Repayment.Plan}{Repayment Schedule under Amortized Decreasing Repayment plan}
  \item{Amortized.Even.Repayment.Plan}{Repayment Schedule under Amortized Even Repayment plan}
  \item{Straight.End.Repayment.Plan}{Repayment Schedule underStraight End Repayment Plan}
  \item{Summary.of.all.repayment.plans}{This table provides the summary of all repayment plans by calculating the sum of principal, interest and installment in each repayment plan}
}
\author{
Gourav Kumar Vani <kumaragri.vani1@gmail.com>, Dr. Vivek Badhe <vivekbadhe.jbp@gmail.com>
}
\references{
Vani, G. K., Awasthi, P. K., & Sahu, R. M. (2018). Generalized Mathematical Expressions for Various Repayment Plans and Long Term Cost Comparison. Economic Affairs, 63(3), 761-768.<doi:10.30954/0424-2513.3.2018.23>


Reddy, S. S., Ram, P. R., & Sastry, T. V. N. Devi. I. B (2009). Agricultural Economics. Oxford & IBH Publishing Co. PVT. Ltd. New Delhi <ISBN 10: 8120415965  ISBN 13: 9788120415966>.}

\examples{
RepaymentPlan(L=100,r=0.12,n=10,Last_year_due=10)
RepaymentPlan(2000,0.06,7,2000)# Last_year_due is set equal to principal sum
RepaymentPlan(2000,0.06/4,7*4,200)# Adjustment for quarterly calculation
RepaymentPlan(2000,0.06/12,7*12,200)# Adjustment for monthly calculation
RepaymentPlan(2000,0.06/2,7*2,200)# Adjustment for harlf yearly calculation
}

