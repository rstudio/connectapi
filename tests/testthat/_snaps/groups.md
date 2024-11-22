# get_group_content() successfully gets the content for multiple groups

    Code
      get_group_content(client, groups_df)
    Output
      # A tibble: 4 x 7
        group_guid group_name  `"content_guid"` `"content_name"` `"content_title"`
        <chr>      <chr>       <chr>            <chr>            <chr>            
      1 a6fb5cea   connect_dev content_guid     content_name     content_title    
      2 a6fb5cea   connect_dev content_guid     content_name     content_title    
      3 a6fb5cea   connect_dev content_guid     content_name     content_title    
      4 ae5c3b2c   group12     content_guid     content_name     content_title    
      # i 2 more variables: `"access_type"` <chr>, role <chr>

# get_group_content() works when just provided group guids

    Code
      get_group_content(client, group_guids)
    Output
      # A tibble: 4 x 7
        group_guid group_name  `"content_guid"` `"content_name"` `"content_title"`
        <chr>      <chr>       <chr>            <chr>            <chr>            
      1 a6fb5cea   connect_dev content_guid     content_name     content_title    
      2 a6fb5cea   connect_dev content_guid     content_name     content_title    
      3 a6fb5cea   connect_dev content_guid     content_name     content_title    
      4 ae5c3b2c   group12     content_guid     content_name     content_title    
      # i 2 more variables: `"access_type"` <chr>, role <chr>

