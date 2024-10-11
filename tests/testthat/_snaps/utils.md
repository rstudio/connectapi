# warn_old_connect warning snapshot

    Code
      capture_warning(warn_old_connect("2022.02", "2022.01"))
    Output
      NULL

---

    Code
      capture_warning(warn_old_connect("2022.01", "2022.02"))
    Output
      <warning/rlang_warning>
      Warning:
      You are using an older version of Posit Connect (2022.01) than is tested (2022.02). Some APIs may not function as expected.
      This warning is displayed once per session.

