{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Keter.LabelMap
    ( -- * Types
      LabelMap
      -- * Helper functions
    , insert
    , delete
    , lookup
    , labelAssigned
    , empty
    ) where

import Prelude hiding (lookup)
import Data.Maybe (isJust)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.ByteString.Char8 as BS
import Data.ByteString (ByteString)
import qualified Data.CaseInsensitive as CI
import Data.CaseInsensitive (CI)

type LabelTree a = Map (CI ByteString) (LabelEntry a)

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
    deriving (Show, Eq)

-- | Indicates whether a given label in the
data LabelEntry a = Assigned   !a !(LabelMap a)
                  | Unassigned    !(LabelMap a)
                  deriving Eq

instance Show (LabelEntry a) where
    show (Assigned _ m) = "Assigned _ (" ++ show m ++ ")"
    show (Unassigned m) = "Unassigned (" ++ show m ++ ")"

hostToLabels :: ByteString -> [ByteString]
hostToLabels h
  | BS.null h        = []
  | BS.last h == '.' = drop 1 labels
  | otherwise        = labels
  where labels = reverse . BS.split '.' $ h

lemap :: (LabelMap a -> LabelMap a) -> LabelEntry a -> LabelEntry a
lemap f (Assigned e m) = Assigned e (f m)
lemap f (Unassigned m) = Unassigned (f m)

labelEntryMap :: LabelEntry a -> LabelMap a
labelEntryMap (Assigned _ m) = m
labelEntryMap (Unassigned m) = m

getPortEntry :: LabelEntry a -> Maybe a
getPortEntry (Assigned e _) = Just e
getPortEntry (Unassigned _) = Nothing

insert :: ByteString -> a -> LabelMap a -> LabelMap a
insert h e m = insertTree (hostToLabels h) e m
--insert h e m = trace
--       ( "Inserting hostname " ++ (show h) ++ "\n"
--       ++"  into tree        " ++ (show m) ++ "\n"
--       ++"  with result      " ++ (show result)
--       )
--       result
--    where result = insertTree (hostToLabels h) e m

insertTree :: [ByteString] -> a -> LabelMap a -> LabelMap a
insertTree []    _ _ = error "Cannot assign empty label in hostname."

insertTree ["*"] e EmptyLabelMap = Wildcard (Assigned e EmptyLabelMap)
insertTree [l]   e EmptyLabelMap = Static (Map.insert (CI.mk l) (Assigned e EmptyLabelMap) Map.empty)

insertTree ["*"] e (Static t) = WildcardExcept (Assigned e EmptyLabelMap) t
insertTree [l']   e (Static t) =
    case Map.lookup l t of
        Nothing  -> Static (Map.insert l (Assigned e EmptyLabelMap) t)
        Just le  -> Static (Map.insert l (Assigned e (labelEntryMap le)) t)
  where
    l = CI.mk l'

insertTree ["*"] e (Wildcard w) = Wildcard (Assigned e (labelEntryMap w))
insertTree [l]   e (Wildcard w) = WildcardExcept w (Map.insert (CI.mk l) (Assigned e EmptyLabelMap) Map.empty)

insertTree ["*"] e (WildcardExcept w t) = WildcardExcept (Assigned e (labelEntryMap w)) t
insertTree [l']   e (WildcardExcept w t) =
    case Map.lookup l t of
        Nothing -> WildcardExcept w (Map.insert l (Assigned e EmptyLabelMap) t)
        Just le -> WildcardExcept w (Map.insert l (Assigned e (labelEntryMap le)) t)
  where
    l = CI.mk l'

insertTree ("*":ls) e EmptyLabelMap = Wildcard (Unassigned (insertTree ls e EmptyLabelMap))
insertTree (l:ls)   e EmptyLabelMap = Static (Map.insert (CI.mk l) (Unassigned $ insertTree ls e EmptyLabelMap) Map.empty)

insertTree ("*":ls) e (Static t) = WildcardExcept (Unassigned (insertTree ls e EmptyLabelMap)) t
insertTree (l':ls)   e (Static t) =
    case Map.lookup l t of
        Nothing -> Static (Map.insert l (Unassigned (insertTree ls e EmptyLabelMap)) t)
        Just le -> Static (Map.insert l (lemap (insertTree ls e) le) t)
  where
    l = CI.mk l'

insertTree ("*":ls) e (Wildcard w) = Wildcard (lemap (insertTree ls e) w)
insertTree (l:ls)   e (Wildcard w) = WildcardExcept w (Map.insert (CI.mk l) (Assigned e (insertTree ls e EmptyLabelMap)) Map.empty)

insertTree ("*":ls) e (WildcardExcept w t) = WildcardExcept (lemap (insertTree ls e) w) t
insertTree (l:ls)   e (WildcardExcept w t) =
    case Map.lookup l' t of
        Nothing -> WildcardExcept w (Map.insert l' (Unassigned (insertTree ls e EmptyLabelMap)) t)
        Just le -> WildcardExcept w (Map.insert l' (lemap (insertTree ls e) le) t)
  where
    l' = CI.mk l

cleanup :: LabelMap a -> LabelMap a
cleanup EmptyLabelMap = EmptyLabelMap
cleanup m@(Static t) =
    case Map.null (Map.filter p t) of
        True  -> EmptyLabelMap
        False -> m
    where
        p (Unassigned EmptyLabelMap) = False
        p _ = True

cleanup m@(Wildcard w) =
    case w of
        Unassigned EmptyLabelMap -> EmptyLabelMap
        _ -> m

cleanup m@(WildcardExcept w t) =
    case (w, Map.null t) of
        (Unassigned EmptyLabelMap, True)  -> EmptyLabelMap
        (Unassigned EmptyLabelMap, False) -> Static t
        (_,                        True)  -> Wildcard w
        (_,                        False) -> m

delete :: ByteString -> LabelMap a -> LabelMap a
delete h m = deleteTree (hostToLabels h) m
--delete h m = trace
--       ( "Deleting hostname  " ++ (show h) ++ "\n"
--       ++"  into tree        " ++ (show m) ++ "\n"
--       ++"  with result      " ++ (show result)
--       )
--       result
--    where result = deleteTree (hostToLabels h) m

deleteTree :: [ByteString] -> LabelMap a -> LabelMap a
deleteTree [] _ = error "Cannot assign empty label in hostname."

deleteTree _ EmptyLabelMap = EmptyLabelMap

deleteTree ["*"] (Static t) = Static t
deleteTree [l]   (Static t) = cleanup $ Static m
   where
    m = case l' `Map.lookup` t of
      Just (Assigned _ EmptyLabelMap) -> Map.delete l' t
      Just (Assigned _ b) -> Map.insert l' (Unassigned b) (Map.delete l' t)
      _ -> t
    l' = CI.mk l

deleteTree ["*"] (Wildcard w) = cleanup $ Wildcard (Unassigned (labelEntryMap w))
deleteTree [_] (Wildcard w) = Wildcard w

deleteTree ["*"] (WildcardExcept w t) = cleanup $ WildcardExcept (Unassigned (labelEntryMap w)) t
deleteTree [l] (WildcardExcept w t) = cleanup $ WildcardExcept w (Map.delete (CI.mk l) t)

deleteTree ("*":_) (Static t) = Static t
deleteTree (l:ls)  (Static t) = cleanup $
    case Map.lookup l' t of
        Nothing -> Static t
        Just le -> Static (Map.insert l' (lemap (deleteTree ls) le) t)
  where
    l' = CI.mk l

deleteTree ("*":ls) (Wildcard w) = cleanup $ Wildcard (lemap (deleteTree ls) w)
deleteTree (_:_)    (Wildcard w) = Wildcard w

deleteTree ("*":ls) (WildcardExcept w t) = cleanup $ WildcardExcept (lemap (deleteTree ls) w) t
deleteTree (l:ls) (WildcardExcept w t) = cleanup $
    case Map.lookup l' t of
        Nothing            -> WildcardExcept w t
        Just le            -> WildcardExcept w (Map.insert l' (lemap (deleteTree ls) le) t)
  where
    l' = CI.mk l

lookup :: ByteString -> LabelMap a -> Maybe a
lookup h m = lookupTree (hostToLabels h) m
--lookup h m = trace
--       ( "Looking up hostname  " ++ (show h) ++ "\n"
--       ++"  in tree            " ++ (show m) ++ "\n"
--       ++"  and found entry?   " ++ (show $ isJust result)
--       )
--       result
--    where result = (lookupTree (hostToLabels h) m)

lookupTree :: [ByteString] -> LabelMap a -> Maybe a
lookupTree [] _ = Nothing

lookupTree _ EmptyLabelMap = Nothing

lookupTree [l] (Static t)   = Map.lookup (CI.mk l) t >>= getPortEntry
--lookupTree (_:_) (Wildcard w) = getPortEntry $ w
lookupTree [l] (WildcardExcept w t) =
    case Map.lookup (CI.mk l) t >>= getPortEntry of
        Just e  -> Just e
        Nothing -> getPortEntry w

lookupTree (l:ls) (Static t) =
    case Map.lookup (CI.mk l) t of
        Just le -> lookupTree ls $ labelEntryMap le
        Nothing -> Nothing
lookupTree (_:ls) (Wildcard w) = lookupTree ls $ labelEntryMap w
lookupTree (l:ls) (WildcardExcept w t) =
    case Map.lookup (CI.mk l) t of
        Just le ->
            case lookupTree ls $ labelEntryMap le of
                Just  e -> Just e
                Nothing -> lookupTree ls $ labelEntryMap w
        Nothing -> lookupTree ls $ labelEntryMap w

-- This function is similar to lookup but it determines strictly
-- whether or not a record to be inserted would override an existing
-- entry exactly. i.e.: When inserting *.example.com, this function
-- will return true for precisely *.example.com, but not foo.example.com.
--
-- This is so that different keter applications may establish ownership
-- over different subdomains, including exceptions to a wildcard.
--
-- This function *does not* test whether or not a given input would
-- resolve to an existing host. In the above example, given only an
-- inserted *.example.com, foo.example.com would route to the wildcard.
-- Even so, labelAssigned will return false, foo.example.com has not
-- been explicitly assigned.
labelAssigned :: ByteString -> LabelMap a -> Bool
labelAssigned h m = memberTree (hostToLabels h) m
--labelAssigned h m = trace
--       ( "Checking label assignment for " ++ (show h) ++ "\n"
--       ++"  in tree            " ++ (show m) ++ "\n"
--       ++"  and found?         " ++ (show result)
--       )
--       result
--    where result = memberTree (hostToLabels h) m

memberTree :: [ByteString] -> LabelMap a -> Bool
memberTree [] _ = False

memberTree ["*"] (Static _)   = False
memberTree [l]   (Static t)   = isJust $ Map.lookup (CI.mk l) t >>= getPortEntry

memberTree ["*"] (Wildcard _) = True
memberTree [_]   (Wildcard _) = False

memberTree ["*"] (WildcardExcept w _) = isJust $ getPortEntry w
memberTree [l]   (WildcardExcept _ t) = isJust $ Map.lookup (CI.mk l) t >>= getPortEntry

memberTree ("*":_) (Static _) = False
memberTree (l:ls)  (Static t) =
    case Map.lookup (CI.mk l) t of
        Just le -> memberTree ls $ labelEntryMap le
        Nothing -> False

memberTree ("*":ls) (Wildcard w) = memberTree ls $ labelEntryMap w
memberTree (_:_)    (Wildcard _) = False

memberTree ("*":ls) (WildcardExcept w _) = memberTree ls $ labelEntryMap w
memberTree (l:ls)   (WildcardExcept _ t) =
    case Map.lookup (CI.mk l) t of
        Just le -> memberTree ls $ labelEntryMap le
        Nothing -> False

memberTree _ EmptyLabelMap = False

empty :: LabelMap a
empty = EmptyLabelMap
