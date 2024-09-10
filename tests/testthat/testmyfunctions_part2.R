context("Other functions")

test_that("Calculate the Inter-site Distance", {
  X=matrix(c(1,3,5,4,2,2,3,6,6,6,4,4,2,1,3,5,5,1),ncol=3,nrow=6,byrow=TRUE)
  expect_equal(dij(X,1,2),7)
  expect_equal(dij(X,2,4),6)
})

test_that("Calculate the phi_p Criterion", {
  X=matrix(c(1,3,5,4,2,2,3,6,6,6,4,4,2,1,3,5,5,1),ncol=3,nrow=6,byrow=TRUE)
  expect_equal(round(phi_p(X,p=50),7),0.2500002)
  expect_equal(round(phi_p(X,p=50,q=2),7),0.4082486)
})

test_that("Calculate the AvgAbsCor Criterion", {
  X=matrix(c(1,3,5,4,2,2,3,6,6,6,4,4,2,1,3,5,5,1),ncol=3,nrow=6,byrow=TRUE)
  expect_equal(round(AvgAbsCor(X),7),0.3714286)
})

test_that("Calculate the MaxAbsCor Criterion", {
  X=matrix(c(1,3,5,4,2,2,3,6,6,6,4,4,2,1,3,5,5,1),ncol=3,nrow=6,byrow=TRUE)
  expect_equal(round(MaxAbsCor(X),7),0.4285714)
})

test_that("Calculate the MaxProCriterion Criterion", {
  X=matrix(c(1,3,5,4,2,2,3,6,6,6,4,4,2,1,3,5,5,1),ncol=3,nrow=6,byrow=TRUE)
  expect_equal(round(MaxProCriterion(X),7),0.3539991)
})

test_that("Exchange two random elements", {
  X=matrix(c(1,3,5,4,2,2,3,6,6,6,4,4,2,1,3,5,5,1),ncol=3,nrow=6,byrow=TRUE)
  Xnew=exchange(X,1) #exchange 2 random elements within the 1st column
  expect_equal(sum(X[,1]),sum(Xnew[,1]))    #column total should be the same
  expect_equal(as.numeric(table(X[,1]==Xnew[,1])),c(2,4))
  #the result should be c(2,4) since there should be 2 elements does not equal to each other while 4 elements are equal
})

test_that("Transfer an Orthogonal Array (OA) into a LHD", {
  #create an OA(9,2,3,2)
  OA=matrix(c(rep(1:3,each=3),rep(1:3,times=3)),ncol=2,nrow=9,byrow = F)
  try.OA=OA2LHD(OA=OA)

  #check if the dimensions are expected
  expect_equal(dim(try.OA),c(9,2))    #dim is the same

  #check if the result satisfies the LHD properties
  n=9;k=2

  for (j in 1:k) {

    #check whether each column has elements of 1 to n
    expect_equal(sort(try.OA[,j]),1:n)

  }

})
