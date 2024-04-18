# check_connect_version warning snapshot

    Code
      capture_warning(check_connect_version("2022.02", "2022.01"))
    Output
      NULL

---

    Code
      capture_warning(check_connect_version("2022.01", "2022.02"))
    Output
      <warning: You are using an older version of Posit Connect (2022.01) than is tested (2022.02). Some APIs may not function as expected.
      This warning is displayed once per session.>

