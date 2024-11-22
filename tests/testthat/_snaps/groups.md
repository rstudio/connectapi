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
      # A tibble: 4 x 7
        group_guid group_name  content_guid content_name     content_title access_type
        <chr>      <chr>       <chr>        <chr>            <chr>         <chr>      
      1 a6fb5cea   connect_dev 8b57f54e     app-1197-9825-t~ app-1197-982~ acl        
      2 a6fb5cea   connect_dev 8bf70c85     quarto-email-de~ quarto-email~ acl        
      3 a6fb5cea   connect_dev fcad1958     top-queries      top-queries   logged_in  
      4 ae5c3b2c   group12     46fb83eb     forecast-email-~ forecast-ema~ logged_in  
      # i 1 more variable: role <chr>

# get_group_content() works when just provided group guids

    Code
      get_group_content(client, group_guids)
    Condition
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"guid"` instead of `.data$guid`
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"name"` instead of `.data$name`
    Output
      # A tibble: 4 x 7
        group_guid group_name  content_guid content_name     content_title access_type
        <chr>      <chr>       <chr>        <chr>            <chr>         <chr>      
      1 a6fb5cea   connect_dev 8b57f54e     app-1197-9825-t~ app-1197-982~ acl        
      2 a6fb5cea   connect_dev 8bf70c85     quarto-email-de~ quarto-email~ acl        
      3 a6fb5cea   connect_dev fcad1958     top-queries      top-queries   logged_in  
      4 ae5c3b2c   group12     46fb83eb     forecast-email-~ forecast-ema~ logged_in  
      # i 1 more variable: role <chr>

