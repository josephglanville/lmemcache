{-
Copyright (c) 2014 Joseph Glanville <jgp@jpd.id.au>
                   Duncan Burke <duncankburke@gmail.com>

This software may be modifid and distributed under the terms
of the MIT license. See the LICENSE file for details.
-}

{- |
   module      : LMemcache.Errors
   copyright   : (c) Joseph Glanville, Duncan Burke
   license     : MIT

   maintaner   : jpg@jpg.id.au
   stability   : experimental
-}

data Error = CommandError | ClientError | ServerError deriving (Show)
