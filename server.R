# Define server logic
shinyServer(function(input, output) {
  
  # Footer
  footer_server('psrcfooter')
  
  # Dashboard Overview
  overview_server('OVERVIEW')
  
  # Center Metrics
  place_server('SCHOOL', place_type = school_title)
  
})    
