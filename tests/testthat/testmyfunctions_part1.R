context("Functions whose result is a design matrix")

test_that("Generate a random LHD matrix with dimension n by k", {
  #check if the dimensions are expected
  expect_equal(dim(rLHD(n=6,k=3)),c(6,3))
  expect_equal(dim(rLHD(n=9,k=2)),c(9,2))

  #check if the result satisfies the LHD properties
  n=5;k=3;try.rLHD=rLHD(n=n,k=k)

  for (j in 1:k) {

    #check whether each column has elements of 1 to n
    expect_equal(sort(try.rLHD[,j]),1:n)

  }

})

test_that("Generate a maximin distance LHD matrix with dimension n by k via SA", {
  #check if the dimensions are expected
  expect_equal(dim(SA(n=6,k=3)),c(6,3))
  expect_equal(dim(SA(n=9,k=2)),c(9,2))

  #check if the result satisfies the LHD properties
  n=5;k=3;try.SA=SA(n=n,k=k)

  for (j in 1:k) {

    #check whether each column has elements of 1 to n
    expect_equal(sort(try.SA[,j]),1:n)

  }

})

test_that("Generate a maximin distance LHD matrix with dimension n by k via OASA", {
  #check if the dimensions are expected
  OA1=matrix(c(rep(1:3,each=3),rep(1:3,times=3)),ncol=2,nrow=9,byrow = F)
  OA2=matrix(c(rep(1:2,each=4),rep(1:2,each=2,times=2),rep(1:2,times=4),
             c(rep(1:2,each=2),rep(2:1,each=2)),c(rep(1:2,times=2),rep(2:1,times=2))),
             ncol=5,nrow=8,byrow = F)

  expect_equal(dim(OASA(OA=OA1)),c(9,2))
  expect_equal(dim(OASA(OA=OA2)),c(8,5))

  #check if the result satisfies the LHD properties
  n=8;k=5;try.OASA=OASA(OA=OA2)

  for (j in 1:k) {

    #check whether each column has elements of 1 to n
    expect_equal(sort(try.OASA[,j]),1:n)

  }

})

test_that("Generate a maximin distance LHD matrix with dimension n by k via SA2008", {
  #check if the dimensions are expected
  expect_equal(dim(SA2008(n=6,k=3)),c(6,3))
  expect_equal(dim(SA2008(n=9,k=2)),c(9,2))

  #check if the result satisfies the LHD properties
  n=5;k=3;try.SA2008=SA2008(n=n,k=k)

  for (j in 1:k) {

    #check whether each column has elements of 1 to n
    expect_equal(sort(try.SA2008[,j]),1:n)

  }

})

test_that("Generate a maximin distance LHD matrix with dimension n by k via SLHD", {
  #check if the dimensions are expected
  expect_equal(dim(SLHD(n=6,k=3)),c(6,3))
  expect_equal(dim(SLHD(n=9,k=2)),c(9,2))

  #check if the result satisfies the LHD properties
  n=5;k=3;try.SLHD=SLHD(n=n,k=k)

  for (j in 1:k) {

    #check whether each column has elements of 1 to n
    expect_equal(sort(try.SLHD[,j]),1:n)

  }

})

test_that("Generate a maximin distance LHD matrix with dimension n by k via LaPSO", {
  #check if the dimensions are expected
  expect_equal(dim(LaPSO(n=6,k=3)),c(6,3))
  expect_equal(dim(LaPSO(n=9,k=2)),c(9,2))

  #check if the result satisfies the LHD properties
  n=5;k=3;try.LaPSO=LaPSO(n=n,k=k)

  for (j in 1:k) {

    #check whether each column has elements of 1 to n
    expect_equal(sort(try.LaPSO[,j]),1:n)

  }

})


test_that("Generate a maximin distance LHD matrix with dimension n by k via GA", {
  #check if the dimensions are expected
  expect_equal(dim(GA(n=6,k=3)),c(6,3))
  expect_equal(dim(GA(n=9,k=2)),c(9,2))

  #check if the result satisfies the LHD properties
  n=5;k=3;try.GA=GA(n=n,k=k)

  for (j in 1:k) {

    #check whether each column has elements of 1 to n
    expect_equal(sort(try.GA[,j]),1:n)

  }

})

test_that("Generate an orthogonal LHD matrix via OLHD.Y1998", {
  #check if the dimensions are expected
  expect_equal(dim(OLHD.Y1998(m=3)),c(9,4))
  expect_equal(dim(OLHD.Y1998(m=4)),c(17,6))

  #check the orthogonality
  expect_equal(MaxAbsCor(OLHD.Y1998(m=3)),0)
  expect_equal(MaxAbsCor(OLHD.Y1998(m=4)),0)

})

test_that("Generate a good lattice point design matrix with dimension n by k via GLP", {
  #check if the dimensions are expected
  expect_equal(dim(GLP(n=6,k=3)),c(6,3))
  expect_equal(dim(GLP(n=9,k=2)),c(9,2))
})

test_that("Implement the Williams transformation on a n by k matrix via WT", {
  X=matrix(c(1,3,5,4,2,2,3,6,6,6,4,4,2,1,3,5,5,1),ncol=3,nrow=6,byrow=TRUE)
  #check if the dimensions are expected
  expect_equal(dim(WT(X)),c(6,3))
  expect_equal(dim(WT(X-5)),c(6,3))
})

test_that("Generate a maximin distance LHD matrix with dimension n by k via FastMmLHD", {
  #check if the dimensions are expected
  expect_equal(dim(FastMmLHD(n=6,k=3)),c(6,3))
  expect_equal(dim(FastMmLHD(n=9,k=2)),c(9,2))

  #check if the result satisfies the LHD properties
  n=5;k=3;try.FastMmLHD=FastMmLHD(n=n,k=k)+1

  for (j in 1:k) {

    #check whether each column has elements of 1 to n
    expect_equal(sort(try.FastMmLHD[,j]),1:n)

  }

})


test_that("Generate an orthogonal LHD matrix via OLHD.C2007", {
  #check if the dimensions are expected
  expect_equal(dim(OLHD.C2007(m=4)),c(17,7))
  expect_equal(dim(OLHD.C2007(m=5)),c(33,11))

  #check the orthogonality
  expect_equal(MaxAbsCor(OLHD.C2007(m=4)),0)
  expect_equal(MaxAbsCor(OLHD.C2007(m=5)),0)

})

test_that("Generate an orthogonal LHD matrix via OLHD.L2009", {
  OLHD=OLHD.C2007(m=2)
  OA=matrix(c(2,2,2,2,2,1,2,1,5,4,3,5,3,2,1,5,4,5,1,5,4,3,2,5,
              4,1,3,5,2,3,1,2,3,4,5,2,1,3,5,2,4,3,1,1,1,1,1,1,4,3,2,1,5,5,
              5,5,5,5,5,1,4,4,4,4,4,1,3,1,4,2,5,4,3,3,3,3,3,1,3,5,2,4,1,3,
              3,4,5,1,2,2,5,4,3,2,1,5,2,3,4,5,1,2,2,5,3,1,4,4,1,4,2,5,3,4,
              4,2,5,3,1,4,2,4,1,3,5,3,5,3,1,4,2,4,5,2,4,1,3,3,5,1,2,3,4,2,
              4,5,1,2,3,2),ncol=6,nrow=25,byrow=T)

  #check if the dimensions are expected
  expect_equal(dim(OLHD.L2009(OLHD,OA)),c(25,12))

  #check the orthogonality
  expect_equal(MaxAbsCor(OLHD.L2009(OLHD,OA)),0)

})

test_that("Generate an orthogonal LHD matrix via OLHD.S2010", {
  #check if the dimensions are expected
  expect_equal(dim(OLHD.S2010(C=3,r=3,type="odd")),c(49,8))
  expect_equal(dim(OLHD.S2010(C=3,r=3,type="even")),c(48,8))

  #check the orthogonality
  expect_equal(MaxAbsCor(OLHD.S2010(C=3,r=3,type="odd")),0)
  expect_equal(MaxAbsCor(OLHD.S2010(C=3,r=3,type="even")),0)

})

test_that("Generate an orthogonal LHD matrix via OLHD.B2001", {
  #check if the dimensions are expected
  expect_equal(dim(OLHD.B2001(n=11,k=5)),c(11,5))
  expect_equal(dim(OLHD.B2001(n=7,k=6)),c(7,6))

  #check the orthogonality
  expect_equal(MaxAbsCor(OLHD.B2001(n=11,k=5)),0.09090909)
  expect_equal(MaxAbsCor(OLHD.B2001(n=7,k=6)),0.2142857)

})
