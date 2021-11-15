module Range where 

data Range = Range 
  {
    absolute_start::Int
    ,line_start::Int
    ,column_start::Int
    ,absolute_end::Int
    ,line_end::Int
    ,column_end::Int
  }
  deriving (Eq,Show)
