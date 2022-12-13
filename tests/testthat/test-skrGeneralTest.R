

test_that("predict skr error Checks", {
  # testing data
  personaldata = as.data.frame(t((c(320,100,5.8,3,4,5,1))))
  colnames(personaldata) = c('GRE.Score','TOEFL.Score','University.Rating','SOP',
                             'LOR','CGPA','Research')

  na_data_all = as.data.frame(t((c(NA,NA,NA,NA,NA,NA,NA))))
  na_data_one = as.data.frame(t((c(NA,100,5.8,3,4,5,1))))

  expect_error(predict_skr(model = lm(Chance.of.Admit ~., data=gradApplication[,-1]),
                           newx = personaldata, methods = c("rf")))
  expect_error(predict_skr(model = lm(Chance.of.Admit ~., data=gradApplication[,-1]),
                           newx = personaldata, methods = c("ridge")))
  expect_error(predict_skr(model = lm(Chance.of.Admit ~., data=gradApplication[,-1]),
                                        newx = na_data_all, methods = c("lm")))
  expect_error(predict_skr(model = lm(Chance.of.Admit ~., data=gradApplication[,-1]),
                                        newx = na_data_one, methods = c("lm")))
})

test_that("model comparison error Checks", {
  expect_type(model_comparison(methods = c("ols")), "list")
  expect_type(model_comparison(methods = c("ols", "ridge")), "list")
  expect_type(model_comparison(methods = c("ols", "ridge", "lasso")), "list")
  expect_type(model_comparison(methods = c("OLS", "riDge", "laSsO")), "list")
  expect_error(model_comparison(methods = c("rf")))
  expect_error(model_comparison(train_p = -1))
  expect_error(model_comparison(train_p = 2))
  expect_error(model_comparison(train_p = c(0.5, 0.5)))
  expect_warning(model_comparison(train_p = 0.3))
})
