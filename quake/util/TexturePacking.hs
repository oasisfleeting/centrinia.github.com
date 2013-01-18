module TexturePacking (pack_textures,placements,query_tree) where {

import Control.Monad (mplus,(=<<));
import Data.Function (on);
import Data.List (sortBy);

data Splitter
= Horizontal Integer
| Vertical Integer
deriving (Show);

splitter_value :: Splitter -> Integer;
splitter_value (Horizontal x) = x;
splitter_value (Vertical x) = x;

transpose_splitter (Horizontal x) = Vertical x;
transpose_splitter (Vertical x) = Horizontal x;

data Tree a
= Node (Integer,Integer) Splitter (Tree a) (Tree a)
| Leaf (Integer,Integer) a
| Empty (Integer,Integer)
deriving (Show);

transpose_tree (Node (w,h) s l r) = Node (h,w) (transpose_splitter s) (transpose_tree l) (transpose_tree r);
transpose_tree (Empty (w,h)) = Empty (h,w);
transpose_tree (Leaf (w,h) a) = Leaf (h,w) a;

tree_width (Empty _) = 0;
tree_width (Leaf _ _) = 0;
tree_width (Node _ (Horizontal _) l r) = tree_width l `max` tree_width r;
tree_width (Node _ (Vertical w) l r) = w + tree_width r;

tree_height = tree_width . transpose_tree;

insert :: a -> (Integer,Integer) -> Tree a -> Maybe (Tree a);
insert item (w,h) t@(Empty (w',h'))
| not (w<=w' && h<=h') = Nothing
| w'-w<h'-h = insert item (h,w) (transpose_tree t) >>= return . transpose_tree
| True = Just $ Node (w',h') (Vertical w) 
	(Node (w,h') (Horizontal h) (Leaf (w,h) item) (Empty (w,h'-h))) (Empty (w'-w,h'));

insert item (w,h) (Leaf _ _) = Nothing;
insert item (w,h) t@(Node (w',h') s l r)
| w<h = insert item (h,w) (transpose_tree t) >>= return . transpose_tree
| not (w<=w' && h<=h') = Nothing
| True =
	(if (case s of { Vertical _ -> w; Horizontal _ -> h; }) <= splitter_value s
		then do { l' <- insert item (w,h) l; return $ Node (w',h') s l' r; } 
		else Nothing)
	`mplus` do { r' <- insert item (w,h) r;
		return $ Node (w',h') s l r'; };

data Placement a
= Placement (Integer,Integer) (Integer,Integer) a
deriving (Show);

placements :: (Integer,Integer) -> Tree a -> [Placement a];
placements (x,y) (Empty _) = [];
placements (x,y) (Leaf (w,h) a) = [Placement (x,y) (w,h) a];
placements (x,y) (Node _ (Horizontal h) l r) =
	placements (x,y) l ++ placements (x,y+h) r;
placements (x,y) (Node _ (Vertical w) l r) =
	placements (x,y) l ++ placements (x+w,y) r;

pack_textures :: (Integer,Integer) -> [(a,(Integer,Integer))] -> Maybe (Tree a);
pack_textures (w,h) textures = 
	(foldl (\acc (item,(w,h)) -> acc >>= insert item (w,h)) (Just (Empty (w,h)))
	$ reverse
	$ sortBy (compare `on` (\(_,(w,h)) -> w+h)) 
	$ textures);


query_tree :: ((Integer,Integer) -> a -> b) -> b -> Tree a -> (Integer,Integer) -> b;
query_tree f base (Empty _) _ = base;
query_tree f base (Leaf _ a) (x,y) = f (x,y) a;
query_tree f base (Node _ (Vertical s) l r) (x,y) =
if x>=s
	then query_tree f base r (x-s,y)
	else query_tree f base l (x,y);
query_tree f base (Node _ (Horizontal s) l r) (x,y) =
if y>=s
	then query_tree f base r (x,y-s)
	else query_tree f base l (x,y);


main = let (w,h) = (2000,2000) in do {
print 
--   $ sortBy (compare `on` (\(Placement (x,y) _ _) -> x+y)) 
--   $ (\(Just x) -> placements (0,0) x)
--   $ (\(Just x) -> (tree_width x,tree_height x))
   $ (\(Just x) -> 1 - (fromIntegral $ tree_width x*tree_height x) / (fromIntegral $ w*h))
	$ foldl (\acc (item,(w,h)) -> acc >>= insert item (w,h)) (Just (Empty (w,h)))
	$ reverse
	$ sortBy (compare `on` (\(_,(w,h)) -> w*h))
	--[(0,(32,32)),(1,(16,32)),(2,(32,16))]
	[((i,j),(2^i-1,2^j-1)) | j <- [2..8],i<-[2..8]]
};

}
