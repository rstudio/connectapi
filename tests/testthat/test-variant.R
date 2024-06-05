with_mock_api({
  scoped_experimental_silence()

  test_that("Variant requests", {
    con <- Connect$new(server = "https://connect.example", api_key = "fake")
    content <- content_item(con, "f2f37341-e21d-3d80-c698-a935ad614066")
    expect_true(validate_R6_class(content, "Content"))
    variant <- get_variant(content, "12345")
    expect_true(validate_R6_class(variant, "Variant"))

    expect_GET(
      variant$get_variant_remote(),
      "https://connect.example/__api__/variants/12345"
    )

    expect_POST(
      variant$send_mail(),
      "https://connect.example/__api__/variants/12345/sender",
      '{"email":"me"}'
    )

    expect_GET(
      variant$get_schedule(),
      "https://connect.example/__api__/variants/12345/schedules"
    )

    expect_GET(
      variant$get_subscribers(),
      "https://connect.example/__api__/variants/12345/subscribers"
    )
    expect_DELETE(
      variant$remove_subscriber("asdf"),
      "https://connect.example/__api__/variants/12345/subscribers/asdf"
    )

    # TODO: test for add_subscribers, it doesn't look like it could be right
    # (but the API is not documented). Function does not look like it would
    # send valid JSON in the POST body

    # See code comment in this function: does it really need query params and body?
    expect_POST(
      variant$render(),
      "https://connect.example/__api__/variants/12345/render?email=none&activate=true",
      '{"email":"none","activate":true}'
    )

    expect_GET(
      variant$renderings(),
      "https://connect.example/__api__/variants/12345/renderings"
    )

    expect_POST(
      variant$update_variant(key = "value"),
      "https://connect.example/__api__/variants/12345",
      '{"key":"value"}'
    )
  })
})
