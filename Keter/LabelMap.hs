{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Keter.LabelMap
    ( -- * Types
      LabelMap
      -- * Helper functions
    , insert
    , delete
    , lookup
    , notMember
    , empty
    ) where

import Prelude hiding (lookup)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.ByteString.Char8 as BS
import Data.ByteString (ByteString)

type LabelTree a = Map ByteString (LabelEntry a)

-- | A data structure for storing a hierarchical set of domain labels
-- from TLD down, supporting wildcards.
--
-- Data structure is mutually recursive with 'LabelEntry', and each level
-- of the tree supports a static assignment for a hostname such as:
--
-- >  example.com
--
-- Or a wildcard assignment for a hostname such as:
--
-- >  *.example.com
--
-- Or a wildcard assignment with a set of teptions, for example:
--
-- >  *.example.com
-- >  admin.example.com
--
-- And lastly, empty labels are supported so that, of course, an assignment
-- for example.com does not necessarily have any subdomains available. As an example
-- suppose we have the following assigned domains:
--
-- >          example.com
-- >      foo.example.com
-- >    *.bar.example.com
-- >    *.qux.example.com
-- >  baz.qux.example.com
--
-- This will resolve to the following value, with some loose pseudocode notation.
--
-- >  Static (map)
-- >    'com' -> Unassigned Static (map)
-- >      'example' -> Assigned a (map)
-- >         'foo'  -> Assigned a EmptyLabelMap
-- >         'bar'  -> Unassigned (Wildcard (Assigned a EmptyLabelMap)
-- >         'qux'  -> Unassigned (WildcardExcept (Assigned a (map)))
-- >           'baz' -> Assigned a EmptyLabelMap
--
-- Note that the hostname "bar.example.com" is unassigned, only the wildcard was set.
--
data LabelMap a = EmptyLabelMap
                | Static         !(LabelTree a)
                | Wildcard       !(LabelEntry a)
                | WildcardExcept !(LabelEntry a) !(LabelTree a)

-- | Indicates whether a given label in the
data LabelEntry a = Assigned   !a !(LabelMap a)
                  | Unassigned    !(LabelMap a)

hostToLabels :: ByteString -> [ByteString]
hostToLabels h =
  if BS.null h
  then []
  else 
    if BS.last h == '.'
    then drop 1 $ labels
    else labels
  where labels = reverse . BS.split '.' $ h

lemap :: (LabelMap a -> LabelMap a) -> LabelEntry a -> LabelEntry a
lemap f (Assigned e m) = Assigned e (f m)
lemap f (Unassigned m) = Unassigned (f m)

labelEntryMap :: LabelEntry a -> LabelMap a
labelEntryMap (Assigned _ m) = m
labelEntryMap (Unassigned m) = m

insert :: ByteString -> a -> LabelMap a -> LabelMap a
insert h e m = insertTree (hostToLabels h) e m

insertTree :: [ByteString] -> a -> LabelMap a -> LabelMap a
insertTree []    _ _ = error "Cannot assign empty label in hostname."

insertTree ["*"] e EmptyLabelMap = Wildcard (Assigned e EmptyLabelMap)
insertTree [l]   e EmptyLabelMap = Static (Map.insert l (Assigned e EmptyLabelMap) Map.empty)

insertTree ["*"] e (Static t) = WildcardExcept (Assigned e EmptyLabelMap) t
insertTree [l]   e (Static t) =
    case Map.lookup l t of
        Nothing  -> Static (Map.insert l (Assigned e EmptyLabelMap) t)
        Just le  -> Static (Map.insert l (Assigned e (labelEntryMap le)) t)

insertTree ["*"] e (Wildcard w) = Wildcard (Assigned e (labelEntryMap w))
insertTree [l]   e (Wildcard w) = WildcardExcept w (Map.insert l (Assigned e EmptyLabelMap) Map.empty)

insertTree ["*"] e (WildcardExcept w t) = WildcardExcept (Assigned e (labelEntryMap w)) t
insertTree [l]   e (WildcardExcept w t) =
    case Map.lookup l t of
        Nothing -> WildcardExcept w (Map.insert l (Assigned e EmptyLabelMap) t)
        Just le -> WildcardExcept w (Map.insert l (Assigned e (labelEntryMap le)) t)

insertTree ("*":ls) e EmptyLabelMap = Wildcard (Unassigned (insertTree ls e EmptyLabelMap))
insertTree (l:ls)   e EmptyLabelMap = Static (Map.insert l (Unassigned $ insertTree ls e EmptyLabelMap) Map.empty)

insertTree ("*":ls) e (Static t) = WildcardExcept (Unassigned (insertTree ls e EmptyLabelMap)) t
insertTree (l:ls)   e (Static t) =
    case Map.lookup l t of
        Nothing -> Static (Map.insert l (Unassigned (insertTree ls e EmptyLabelMap)) t)
        Just le -> Static (Map.insert l (lemap (insertTree ls e) le) t)

insertTree ("*":ls) e (Wildcard w) = Wildcard (lemap (insertTree ls e) w)
insertTree (l:ls)   e (Wildcard w) = WildcardExcept w (Map.insert l (Assigned e (insertTree ls e EmptyLabelMap)) Map.empty)

insertTree ("*":ls) e (WildcardExcept w t) = WildcardExcept (lemap (insertTree ls e) w) t
insertTree (l:ls)   e (WildcardExcept w t) =
    case Map.lookup l t of
        Nothing -> WildcardExcept w (Map.insert l (Unassigned (insertTree ls e EmptyLabelMap)) t)
        Just le -> WildcardExcept w (Map.insert l (lemap (insertTree ls e) le) t)

delete :: ByteString -> LabelMap a -> LabelMap a
delete h m = deleteTree (hostToLabels h) m

deleteTree :: [ByteString] -> LabelMap a -> LabelMap a
deleteTree [] _ = error "Cannot assign empty label in hostname."

deleteTree _ EmptyLabelMap = EmptyLabelMap

deleteTree ["*"] (Static t) = Static t
deleteTree [l]   (Static t) = Static (Map.delete l t)

deleteTree ["*"] (Wildcard w) = Wildcard (Unassigned (labelEntryMap w))
deleteTree [_] (Wildcard w) = Wildcard w

deleteTree ["*"] (WildcardExcept w t) = WildcardExcept (Unassigned (labelEntryMap w)) t
deleteTree [l] (WildcardExcept w t) = WildcardExcept w (Map.delete l t)

deleteTree ("*":_) (Static t) = Static t
deleteTree (l:ls)  (Static t) =
    case Map.lookup l t of
        Nothing -> Static t
        Just le -> Static (Map.insert l (lemap (deleteTree ls) le) t)

deleteTree ("*":ls) (Wildcard w) = Wildcard (lemap (deleteTree ls) w)
deleteTree (_:_)    (Wildcard w) = Wildcard w

deleteTree ("*":ls) (WildcardExcept w t) = WildcardExcept (lemap (deleteTree ls) w) t
deleteTree (l:ls) (WildcardExcept w t) =
    case Map.lookup l t of
        Nothing            -> WildcardExcept w t
        Just le             -> WildcardExcept w (Map.insert l (lemap (deleteTree ls) le) t)

lookup :: ByteString -> LabelMap a -> Maybe a
lookup h m = lookupTree (hostToLabels h) m

lookupTree :: [ByteString] -> LabelMap a -> Maybe a
lookupTree [] _ = Nothing

lookupTree _ EmptyLabelMap = Nothing

lookupTree [l] (Static t)   = labelToMaybePortEntry $ Map.lookup l t
lookupTree [_] (Wildcard w) = labelToMaybePortEntry . Just $ w
lookupTree [l] (WildcardExcept w t) =
    case labelToMaybePortEntry $ Map.lookup l t of
        Just e  -> Just e
        Nothing -> labelToMaybePortEntry . Just $ w

lookupTree (l:ls) (Static t) =
    case Map.lookup l t of
        Just le -> lookupTree ls $ labelEntryMap le
        Nothing -> Nothing
lookupTree (_:ls) (Wildcard w) = lookupTree ls $ labelEntryMap w
lookupTree (l:ls) (WildcardExcept w t) =
    case Map.lookup l t of
        Just le -> lookupTree ls $ labelEntryMap le
        Nothing -> lookupTree ls $ labelEntryMap w

notMember :: ByteString -> LabelMap a -> Bool
notMember h m =
    case lookup h m of
        Just _  -> False
        Nothing -> True 

labelToMaybePortEntry :: Maybe (LabelEntry a) -> Maybe a
labelToMaybePortEntry (Just (Assigned e _)) = Just e
labelToMaybePortEntry (Just (Unassigned _)) = Nothing
labelToMaybePortEntry Nothing               = Nothing

empty :: LabelMap a
empty = EmptyLabelMap
