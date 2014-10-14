module LMemcache.Responses (CommandResponse) where
data CommandResponse = ByteString -- TODO(jpg): Something much better here

data SetResponse = Stored | NotStored | Exists | NotFound deriving (Show)
data GetResponse = End deriving (Show)
