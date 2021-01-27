request_msg <- function(resource_url, base_url, user_agent, response) {

  url <- response$url
  code <- httr::status_code(response)
  type <- httr::http_type(response)

  if(identical(type, 'application/json')) {
    content <- httr::content(response, 'text', encoding = 'UTF-8')
    is_paginated <- is_paginated(content)
    count <- ifelse(is_paginated, count(content), 'not applicable')
  } else {
    is_paginated <- 'not applicable'
    count <- 'not applicable'
  }

  msg <- glue::glue(
    '\n\n',
    '* Base URL:       {base_url}\n',
    '* Resource:       {resource_url}\n',
    '* Endpoint:       {url}\n',
    '* User agent:     {user_agent$options$useragent}\n',
    '* Status code:    {code}\n',
    '* MIME type:      {type}\n',
    '* Paginated:      {is_paginated}\n',
    '* Count:          {count}\n\n'
  )
}
