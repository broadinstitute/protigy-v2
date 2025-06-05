
test_that("smart_trim works on example labels", {
  # test labels
  labels = c('xxxx_stuffandthings_xasf',
             'xxxx_stuffandthings_ejwt',
             'xxxx_stuffandthings_fdsh')
  
  labels_reverse = c('fsax_sgnihtdnaffuts_xxxx',
                     'twje_sgnihtdnaffuts_xxxx',
                     'hsdf_sgnihtdnaffuts_xxxx')
  
  labels_equal_difference = c('xxxx_stuffandthings_yyyy',
                              'xxxx_thingsandstuff_yyyy',
                              'xxxx_stuffandthings_yyyy')
  
  
  expect_equal(suppressWarnings(smart_trim(labels)), 
               c("hings_xasf", "hings_ejwt", "hings_fdsh"))
  expect_equal(suppressWarnings(smart_trim(labels_reverse)),
               c("fsax_sgnih", "twje_sgnih", "hsdf_sgnih"))
  expect_equal(suppressWarnings(smart_trim(labels_equal_difference)),
               c("xxxx_stuff", "xxxx_thing", "xxxx_stuff"))
})

test_that("scale_font_size returns appropriate font sizes", {
  # test different parameters
  expect_equal(suppressWarnings(scale_font_size(dimension=50,max.size=14,scale.factor=50)), 14)
  expect_equal(suppressWarnings(scale_font_size(dimension=100,max.size=14,scale.factor=50)), 7)
  expect_equal(suppressWarnings(scale_font_size(dimension=75,max.size=12,scale.factor=50)), 8)
  expect_equal(suppressWarnings(scale_font_size(dimension=75,max.size=12,scale.factor=100)), 12)
})