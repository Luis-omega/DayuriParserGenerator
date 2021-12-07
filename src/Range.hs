module Range where 

-- | A data type that abstract the begin and 
-- end position of some contiguous text.
data Range = Range 
  {
    absolute_start::Int -- ^ Must be positive, counts the 
                        -- number of unicode code points 
                        -- from begin of file
    ,line_start::Int   -- ^ Number of line break since 
                       -- file begin
    ,column_start::Int -- ^ Counts number of unicode code 
                       -- points from last line break 
                       -- or file begin if no previous line 
                       -- break exists.
    ,absolute_end::Int
    ,line_end::Int
    ,column_end::Int
  }
  deriving (Eq,Show)
