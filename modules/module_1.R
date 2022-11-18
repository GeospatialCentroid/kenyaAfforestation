
# define ui ---------------------------------------------------------------
module_UI <- function(id){
  ns <- NS(id)
  tagList( # ensures that correct html returns. Functions as a list. 
    # define ui elements within here. 
    actionButton(
      inputId = ns("start_calculation"), # need to wrap the object id in the ns 
      # call so it know what module instance to reference. 
      label = "calculate"
    )
  )
}


# define server  ---------------------------------------------------------- 
module_server <- function(id){
  moduleServer(
    id,
    function(input,output,session){
       
    }
  )
}

