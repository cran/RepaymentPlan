RepaymentPlan=function (L, r, n, Last_year_due)
{
  year = c(1:n)
  amortized_even_repayment_plan = function(L = L, r = r, n = n) {

    I = function(L, r, n) {
      installment = (L * r/(1 - (1 + r)^-n))
      return(installment)
    }
    Inst = I(L, r, n)
    interest = function(L, r, Inst, n) {
      interest = ((L * r - Inst) * (1 + r)^(n - 1)) + Inst
      return(interest)
    }
    principal = function(L, r, Inst, n) {
      principal = (Inst - L * r) * (1 + r)^(n - 1)
      return(principal)
    }
    balance = function(L, r, Inst, n) {
      balance = (L * (1 + r)^n) - Inst * ((((1 + r)^n) -
                                             1)/r)
      return(balance)
    }
    Installment = c(rep(Inst, n))
    Interest = interest(L = L, r = r, Inst = Inst, year)
    Principal = principal(L = L, r = r, Inst = Inst, year)
    Balance = balance(L = L, r = r, Inst = Inst, year)
    amortized_even_repayment_plan = data.frame(Year = year,
                                               Principal = Principal, Interest = Interest, Installment = Installment,
                                               Balance = Balance)
    return(amortized_even_repayment_plan)
  }
  amortized.decreasing.rp = function(L = L, r = r, n = n) {
    delta = L/n
    interest = function(L, r, n) {
      interest = (L - (n - 1) * delta) * r
      return(interest)
    }
    Principal = c(rep(delta, n))
    installment = function(L, r, n) {
      installment = delta + (L - (n - 1) * delta) * r
      return(installment)
    }
    balance = function(L, delta, n) {
      balance = L - n * delta
      return(balance)
    }
    Interest = interest(L = L, r = r, year)
    Installment = installment(L = L, r = r, year)
    Balance = balance(L = L, delta = delta, year)
    amortized.decreasing.rp = data.frame(Year = year,
                                         Principal = Principal, Interest = Interest, Installment = Installment,
                                         Balance = Balance)
    return(amortized.decreasing.rp)
  }
  partial.repayment.plan = function(L, r, n, Last_year_due) {
    N = n
    n = N - 1
    delta = (L - Last_year_due)/n
    m = L/delta
    principal = function(L, delta, n) {
      if (n < N)
        principal = delta
      else principal = Last_year_due
      return(principal)
    }
    interest = function(L, r, n, delta) {
      if (n < N)
        interest = (L - (n - 1) * delta) * r
      else interest = Last_year_due * r
      return(interest)
    }
    installment = function(L, r, n, delta) {
      if (n < N)
        installment = delta + (L - (n - 1) * delta) *
          r
      else installment = principal(L = L, delta = delta,
                                   n = N) + interest(L = L, r = r, n = N, delta = delta)
      return(installment)
    }
    balance = function(L, n, delta) {
      if (n < N)
        balance = L - n * delta
      else balance = 0
      return(balance)
    }
    Principal = vector(length = N)
    Interest = vector(length = N)
    Installment = vector(length = N)
    Balance = vector(length = N)
    for (i in 1:N) {
      Principal[i] = principal(L = L, delta = delta, n = i)
      Interest[i] = interest(L = L, r = r, n = i, delta = delta)
      Installment[i] = installment(L, r, i, delta)
      Balance[i] = balance(L = L, n = i, delta = delta)
    }
    partial.repayment.plan = data.frame(Year = year,
                                        Principal = Principal, Interest = Interest, Installment = Installment,
                                        Balance = Balance)
    return(partial.repayment.plan)
  }
  straightEnd_repayment_plan = function(L = L, r = r, n = n) {
    Principal = c(rep(0, n - 1), L)
    interest = function(L = L, r = r) {
      interest = L * r
      return(interest)
    }
    Interest = rep(interest(L = L, r = r), n)
    Installment = c(rep(interest(L = L, r = r), n - 1), L +
                      interest(L = L, r = r))
    Balance = c(rep(L, n - 1), 0)
    straightEnd.repayment.plan = data.frame(Year = year,
                                            Principal = Principal, Interest = Interest, Installment = Installment,
                                            Balance = Balance)
    return(straightEnd.repayment.plan)
  }
  x1 = partial.repayment.plan(L = L, r = r, n = n, Last_year_due)
  x2 = amortized.decreasing.rp(L = L, r = r, n = n)
  x3 = amortized_even_repayment_plan(L = L, r = r, n = n)
  x4 = straightEnd_repayment_plan(L = L, r = r, n = n)

  matrix1=matrix(c(apply(x1[,-c(1,5)],2,sum),apply(x2[,-c(1,5)],2,sum),
                   apply(x3[,-c(1,5)],2,sum),apply(x4[,-c(1,5)],2,sum)),byrow=TRUE,nrow=4)

  row_names=c("Partial Repayment Plan","Amortized Decreasing Repayment Plan",
              "Amortized Even Repayment Plan","Straight End Repayment Plan")

  colnames=c("Principal","Interest","Installment")

  barplot(matrix1[,2],names.arg=row_names,xlab="Repayment Plans",ylab="Interest Amount")

  dimnames(matrix1) <- list(row_names,colnames)

  table1=as.table(round(matrix1))
  return(list("Partial.Repayment.Plan"=round(x1),
              "Amortized.Decreasing.Repayment.Plan"=round(x2),
              "Amortized.Even.Repayment.Plan"=round(x3),
              "Straight.End.Repayment.Plan"=round(x4),"Summary.of.all.repayment.plans"=table1
  ))
}

