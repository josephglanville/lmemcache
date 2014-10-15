{-
Copyright (c) 2014 Joseph Glanville <jgp@jpg.id.au>
                   Duncan Burke <duncankburke@gmail.com>

This software may be modified and distributed under the terms
of the MIT license. See the LICENSE file for details.
-}

{- |
   module      : LMemcache.Errors
   copyright   : (c) Joseph Glanville, Duncan Burke
   license     : MIT

   maintainer  : jpg@jpg.id.au
   stability   : experimental
-}

data Error = CommandError | ClientError | ServerError deriving (Show)
