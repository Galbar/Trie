data Trie a b =  Trie [a] (Maybe b) [Trie a b] | EmptyTrie deriving (Show)

get :: Ord a => Trie a b -> [a] -> Maybe b
get EmptyTrie _ = Nothing
get t x = get_ [t] x
    where
        get_ :: Ord a => [Trie a b] -> [a] -> Maybe b
        get_ [] _ = Nothing
        get_ ((Trie c d l):ls) x
            | c == tx && length dx == 0 = d
            | c == tx = get_ l dx
            | gte = Nothing
            | otherwise = get_ ls x
            where
                len = length c
                (tx, dx) = splitAt len x
                gte = c >= tx

put :: Ord a => Trie a b -> [a] -> b -> Trie a b
put EmptyTrie x d = Trie x (Just d) []
put (Trie [] o l) [] d = Trie [] (Just d) l
put (Trie c o l) x v
    | length c == 0 = Trie c o (put_ l x v)
    | length r > 1 = Trie [] Nothing r
    | otherwise = head r
    where
        put_ :: Ord a => [Trie a b] -> [a] -> b -> [Trie a b]
        -- new value is direct child of parent Trie
        put_ [] x d = [Trie x (Just d) []]
        put_ ((Trie c o l):ls) x d
            -- new value is not child of current && check precedence to keep sorting
            | length k2 == 0 && lt = ((Trie c o l):(put_ ls k3 d))
            | length k2 == 0 = ((Trie x (Just d) []):(Trie c o l):ls)
            -- new value is assigned to current
            | length k3 == 0 = ((Trie c (Just d) l):ls)
            -- new value is child of current
            | length k1 == 0 = ((Trie c o (put_ l k3 d)):ls)
            -- new value has a common part with current
            | otherwise = (Trie k2 Nothing (put_ [(Trie k1 o l)] k3 d):ls)
            where
                -- Get the (existent non-common part, common part, new non-common part)
                slice :: Ord a => [a] -> [a] -> ([a], [a], [a])
                slice (c:cs) (x:xs)
                    | c == x = fix (slice cs xs) c
                    | otherwise = ((c:cs), [], (x:xs))
                    where
                        fix :: ([a], [a], [a]) -> a -> ([a], [a], [a])
                        fix (x, y, z) c = (x, (c:y), z)
                slice c x = (c, [], x)
                (k1, k2, k3) = slice c x
                lt = k1 < k3
        r = put_ [(Trie c o l)] x v

erase :: Ord a => Trie a b -> [a] -> Trie a b
erase EmptyTrie _ = EmptyTrie
erase t x
    | length r == 0 = EmptyTrie
    | otherwise = head r
    where
        erase_ :: Ord a => [Trie a b] -> [a] -> [Trie a b]
        erase_ [] _ = []
        erase_ ((Trie c d l):ls) x
            | c == tx && length dx == 0 && length l == 0 = ls
            | c == tx && length dx == 0 = ((Trie c Nothing l):ls)
            | c == tx = (fix (Trie c d (erase_ l dx)):ls)
            | gte = ((Trie c d l):ls)
            | otherwise = (Trie c d l:erase_ ls x)
            where
                fix :: Ord a => Trie a b -> Trie a b
                fix (Trie c Nothing [(Trie x d l)]) = Trie (c ++ x) d l
                fix t = t
                len = length c
                (tx, dx) = splitAt len x
                gte = c >= tx
        r = erase_ [t] x
