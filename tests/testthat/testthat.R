# test for make_filename
expect_that(myfirstpackage::make_filename(2013), is_identical_to("accident_2013.csv.bz2"))
