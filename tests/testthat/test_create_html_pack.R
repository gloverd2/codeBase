testthat::context("Testing create_html_pack")


testthat::test_that("Test that function runs - Complex output", {
  print(getwd())

  # Create output list to export
  output_list <- list()

  for (ii in 1:2){
    jj_list <- list()
    for (jj in 1:2){
      temp_plot <- ggplot2::ggplot() + ggplot2::geom_point(ggplot2::aes(x=runif(25), y=runif(25))) # Dummy plots
      jj_list[[paste0("plot ", ii, ".", jj)]] <- temp_plot
    }
    output_list[[paste0("section ", ii)]] <- jj_list
  }

  output_list[[paste0("table 1")]] <- data.frame(a=runif(25), b=runif(25)) # Dummy table



  create_html_pack(output_list = output_list, filepath="./testing_create_html_pack.html")
  testthat::expect_true(file.exists("./testing_create_html_pack.html")) # check file exists

  # If the function is edited this line should be removed and the output inspected
  file.remove("./testing_create_html_pack.html") # Remove file to avoid clutter.

})

testthat::test_that("Simple checks on inputs - Simple output", {

  # Should run -----------------------------------------
  create_html_pack(output_list = list("a"=1, "b"=1), filepath="./testing_create_html_pack2.html", title="Testing create_html_pack.R", clean_up=TRUE)
  testthat::expect_true(file.exists("./testing_create_html_pack2.html")) # check file exists

  testthat::expect_true(!file.exists("./testing_create_html_pack2.Rmd")) # check doesn't file exists
  testthat::expect_true(!file.exists("./testing_create_html_pack2.rmd")) # check doesn't file exists

  create_html_pack(output_list = list("a"=1, "b"=list("b.1"=1, "b.2"=2)), filepath="./testing_create_html_pack2.html", title="Testing create_html_pack.R", clean_up=FALSE)

  testthat::expect_true(file.exists("./testing_create_html_pack2.Rmd")) # check now file exists
  testthat::expect_true(file.exists("./testing_create_html_pack2.rds")) # check now file exists


  # Shouldn't run ---------------------------------------
  # Must be named list
  testthat::expect_error(create_html_pack(output_list = list(1, list("b.1"=1, "b.2"=2)), filepath="./testing_create_html_pack2.html", title="Testing create_html_pack.R"))
  testthat::expect_error(create_html_pack(output_list = list("a"=1, "b"=list(1, 2)), filepath="./testing_create_html_pack2.html", title="Testing create_html_pack.R"))

  # File path must exist
  suppressWarnings(
  testthat::expect_error(create_html_pack(output_list = list("a"=1, "b"=list("b.1"=1, "b.2"=2)), filepath="./fake_dir/testing_create_html_pack2.html", title="Testing create_html_pack.R"))
  )
  testthat::expect_error(create_html_pack(output_list = list("a"=1, "b"=list("b.1"=1, "b.2"=2)), filepath=123, title="Testing create_html_pack.R"))
  testthat::expect_error(create_html_pack(output_list = list("a"=1, "b"=list("b.1"=1, "b.2"=2)), filepath=c("./1.html","./2.html"), title="Testing create_html_pack.R"))

  # Title should be single string
  testthat::expect_error(create_html_pack(output_list = list("a"=1, "b"=list("b.1"=1, "b.2"=2)), filepath="./testing_create_html_pack2.html", title=123))
  testthat::expect_error(create_html_pack(output_list = list("a"=1, "b"=list("b.1"=1, "b.2"=2)), filepath="./testing_create_html_pack2.html", title=c("1", "2")))

  #Clean up should be single logical
  testthat::expect_error(create_html_pack(output_list = list("a"=1, "b"=list("b.1"=1, "b.2"=2)), filepath="./testing_create_html_pack2.html", title="Testing create_html_pack.R", clean_up = "TRUE"))
  testthat::expect_error(create_html_pack(output_list = list("a"=1, "b"=list("b.1"=1, "b.2"=2)), filepath="./testing_create_html_pack2.html", title="Testing create_html_pack.R", clean_up = c(TRUE, TRUE)))

  # If the function is edited this line should be removed and the output inspected
  file.remove("./testing_create_html_pack2.html") # Remove file to avoid clutter.
  file.remove("./testing_create_html_pack2.Rmd") # Remove file to avoid clutter.
  file.remove("./testing_create_html_pack2.rds") # Remove file to avoid clutter.

})




