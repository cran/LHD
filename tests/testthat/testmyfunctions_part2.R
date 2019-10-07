context("Other Handy functions")

test_that("Calculate the Inter-site Distance", {
  set.seed(1)
  X=rLHD(6,3)
  expect_equal(dij(X,1,2),7)
  expect_equal(dij(X,2,4),6)
})

test_that("Calculate the phi_p Criterion", {
  set.seed(1)
  X=rLHD(6,3)
  expect_equal(round(phi_p(X,p=50),7),0.2500002)
  expect_equal(round(phi_p(X,p=50,q=2),7),0.4082486)
})

test_that("Exchange two random elements", {
  set.seed(1)
  X=rLHD(6,3)
  Xnew=exchange(X,1) #exchange 2 random elements within the 1st column
  expect_equal(sum(X[,1]),sum(Xnew[,1]))    #column total should be the same
  expect_equal(as.numeric(table(X[,1]==Xnew[,1])),c(2,4))
  #the result should be c(2,4) since there should be 2 elements does not equal to each other while the rest are equal
})

test_that("Transfer an Orthogonal Array (OA) into a LHD", {
  #create an OA(9,2,3,2)
  OA=matrix(c(rep(1:3,each=3),rep(1:3,times=3)),ncol=2,nrow=9,byrow = F)
  tryOA=OA2LHD(OA=OA,9,2,3,2)
  expect_equal(dim(tryOA),c(9,2))    #dim is the same
  expect_equal(sort(tryOA[,1]),1:9)  #the resulting matrix should be a LHD
})
