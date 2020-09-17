context("Functions whose result is a design matrix")

test_that("Generate a random LHD matrix with dimension n by k", {
  expect_equal(dim(rLHD(n=6,k=3)),c(6,3))
  expect_equal(dim(rLHD(n=9,k=2)),c(9,2))
})

test_that("Generate a maximin distance LHD matrix with dimension n by k via SA", {
  expect_equal(dim(SA(n=6,k=3)),c(6,3))
  expect_equal(dim(SA(n=9,k=2)),c(9,2))
})

test_that("Generate a maximin distance LHD matrix with dimension n by k via OASA", {
  OA1=matrix(c(rep(1:3,each=3),rep(1:3,times=3)),ncol=2,nrow=9,byrow = F)
  OA2=matrix(c(rep(1:2,each=4),rep(1:2,each=2,times=2),rep(1:2,times=4),
             c(rep(1:2,each=2),rep(2:1,each=2)),c(rep(1:2,times=2),rep(2:1,times=2))),
             ncol=5,nrow=8,byrow = F)

  expect_equal(dim(OASA(OA=OA1)),c(9,2))
  expect_equal(dim(OASA(OA=OA2)),c(8,5))
})

test_that("Generate a maximin distance LHD matrix with dimension n by k via SA2008", {
  expect_equal(dim(SA2008(n=6,k=3)),c(6,3))
  expect_equal(dim(SA2008(n=9,k=2)),c(9,2))
})

test_that("Generate a maximin distance LHD matrix with dimension n by k via SLHD", {
  expect_equal(dim(SLHD(n=6,k=3)),c(6,3))
  expect_equal(dim(SLHD(n=9,k=2)),c(9,2))
})

test_that("Generate a maximin distance LHD matrix with dimension n by k via LaPSO", {
  expect_equal(dim(LaPSO(n=6,k=3)),c(6,3))
  expect_equal(dim(LaPSO(n=9,k=2)),c(9,2))
})


test_that("Generate a maximin distance LHD matrix with dimension n by k via GA", {
  expect_equal(dim(GA(n=6,k=3)),c(6,3))
  expect_equal(dim(GA(n=9,k=2)),c(9,2))
})

test_that("Generate an orthogonal LHD matrix via OLHD1998", {
  expect_equal(dim(OLHD1998(m=3)),c(9,4))
  expect_equal(dim(OLHD1998(m=4)),c(17,6))
})

test_that("Generate a good lattice point design matrix with dimension n by k via GLP", {
  expect_equal(dim(GLP(n=6,k=3)),c(6,3))
  expect_equal(dim(GLP(n=9,k=2)),c(9,2))
})

test_that("Implement the Williams transformation on a n by k matrix via WT", {
  X=matrix(c(1,3,5,4,2,2,3,6,6,6,4,4,2,1,3,5,5,1),ncol=3,nrow=6,byrow=TRUE)
  expect_equal(dim(WT(X)),c(6,3))
  expect_equal(dim(WT(X-5)),c(6,3))
})

test_that("Implement the modified Williams transformation on a n by k matrix via MWT", {
  X=matrix(c(1,3,5,4,2,2,3,6,6,6,4,4,2,1,3,5,5,1),ncol=3,nrow=6,byrow=TRUE)
  expect_equal(dim(MWT(X)),c(6,3))
  expect_equal(dim(MWT(X+5)),c(6,3))
})

test_that("Generate a maximin distance LHD matrix with dimension n by k via LPWT", {
  expect_equal(dim(LPWT(n=6,k=3)[[1]]),c(6,3))
  expect_equal(dim(LPWT(n=9,k=2)[[1]]),c(9,2))
})

test_that("Generate a maximin distance LHD matrix with dimension n by k via LOO", {
  expect_equal(dim(LOO(n=6,k=3)[[1]]),c(6,3))
  expect_equal(dim(LOO(n=9,k=2)[[1]]),c(9,2))
})

test_that("Generate a maximin distance LHD matrix with dimension n by k via LOO", {
  expect_equal(dim(FastMmLHD(n=6,k=3)),c(6,3))
  expect_equal(dim(FastMmLHD(n=9,k=2)),c(9,2))
})
