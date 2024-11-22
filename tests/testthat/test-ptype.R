test_that("validate_df_ptype() accepts data frames meeting requirements", {
  expect_no_error(validate_df_ptype(
    input = tibble::tibble(
      guid = NA_character_,
      name = NA_character_,
      owner_guid = NA_character_
    ),
    required = tibble::tibble(
      guid = NA_character_,
      name = NA_character_
    )
  ))
})

test_that("validate_df_ptype() rejects data missing required names", {
  expect_error(
    validate_df_ptype(
      input = tibble::tibble(
        content_guid = NA_character_,
        content_name = NA_character_,
        owner_guid = NA_character_
      ),
      required = tibble::tibble(
        guid = NA_character_,
        name = NA_character_
      )
    ),
    "Missing required columns: guid, name"
  )
})

test_that("validate_df_ptype() rejects data missing required names", {
  expect_error(
    validate_df_ptype(
      input = tibble::tibble(
        content_guid = NA_character_,
        content_name = NA_character_,
        owner_guid = NA_character_
      ),
      required = tibble::tibble(
        guid = NA_character_,
        name = NA_character_
      )
    ),
    "Missing required columns: guid, name"
  )
})

test_that("validate_df_ptype() rejects data with wrong types", {
  expect_error(
    validate_df_ptype(
      input = tibble::tibble(
        guid = NA_integer_,
        name = NA_character_
      ),
      required = tibble::tibble(
        guid = NA_character_,
        name = NA_character_
      )
    ),
    "Column `guid` has type `int`; needs `chr:`"
  )
})
