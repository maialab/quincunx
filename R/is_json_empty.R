is_json_empty <- function(json_txt) {

  empty_not_paginated <- '{}'
  empty_not_paginated2 <- '[]'
  empty_paginated <- '{"size":0,"count":0,"next":null,"previous":null,"results":[]}'


  return(identical(json_txt, empty_not_paginated) ||
           identical(json_txt, empty_paginated) ||
           identical(json_txt, empty_not_paginated2))

}
