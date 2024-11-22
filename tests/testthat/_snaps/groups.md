# get_group_content() successfully gets the content for multiple groups

    Code
      get_group_content(client, groups_df)
    Condition
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"guid"` instead of `.data$guid`
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"name"` instead of `.data$name`
    Output
      # A tibble: 27 x 7
         group_guid group_name  content_guid content_name    content_title access_type
         <chr>      <chr>       <chr>        <chr>           <chr>         <chr>      
       1 a6fb5cea   connect_dev ec0c03f8     rmd-linked-doc  rmd-linked-d~ acl        
       2 a6fb5cea   connect_dev 8b57f54e     app-1197-9825-~ app-1197-982~ acl        
       3 a6fb5cea   connect_dev d6b2e4c6     okr4_ui_metrics OKR4 UI Metr~ logged_in  
       4 a6fb5cea   connect_dev eb747f8c     Title-1232-168~ Title 1232    logged_in  
       5 a6fb5cea   connect_dev bf7ad642     stats-summary-2 stats-summar~ acl        
       6 a6fb5cea   connect_dev 818065e4     stats-apr-11-w~ stats-apr-11~ acl        
       7 a6fb5cea   connect_dev 4df4c439     choices-4-19    choices-4-19  acl        
       8 a6fb5cea   connect_dev 3756e60f     choices-5-5     choices-5-5   acl        
       9 a6fb5cea   connect_dev b00ab022     zendesk_25849   Zendesk_25849 acl        
      10 a6fb5cea   connect_dev fcad1958     dogfood-top-qu~ Dogfood-top-~ acl        
      # i 17 more rows
      # i 1 more variable: role <chr>

