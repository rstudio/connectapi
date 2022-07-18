# check_connect_version warning snapshot

    Code
      capture_warning(check_connect_version("2022.02", "2022.01"))
    Output
      <warning: You are using a newer version of RStudio Connect (2022.02) than was tested (2022.01). Most APIs should function as expected.
      This warning is displayed once per session.>

---

    Code
      capture_warning(check_connect_version("2022.01", "2022.02"))
    Output
      <warning: You are using an older version of RStudio Connect (2022.01) than was tested (2022.02). Some APIs may not function as expected.
      This warning is displayed once per session.>

