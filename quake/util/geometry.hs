
module Geometry where {
import Data.Maybe (catMaybes);
import Data.List;

-- Matrix minors
minors mat' =
let {
	x = transpose mat';
} in zipWith (++) (inits x) (tail $ tails x);

--determinant :: (Num a) => [[a]] -> a;
determinant [[a]] = a;
determinant mat =
let {
	mat' = tail mat; -- The rows below the first.
	dets = zipWith (*) (cycle [1,-1]) (map determinant $ minors mat');
} in sum $ zipWith (*) (head mat) dets;

intersect_planes :: (Eq a, Fractional a) => [(a,[a])] -> Maybe [a];
intersect_planes planes =
let {
   dists = [d | (d,_) <- planes];
	mat = [normal | (_,normal) <- planes];
	cofactors = let x = transpose mat in
		map determinant $ zipWith (++) (inits x) (zipWith (:) (repeat dists) (tail $ tails x));
	det = determinant mat;
} in if det == 0 then Nothing else Just $ map (/det) cofactors;

choose2 :: [a] -> [[a]];
choose2 [] = [];
choose2 [_] = [];
choose2 [x,y] = [[x,y]];
choose2 (x:xs) = map (\y -> [x,y]) xs ++ choose2 xs;
	
-- Produce a face from a plane by intersecting it with the interior of an axis-aligned box.
bound_plane :: (Ord a, Fractional a) => [[a]] -> (a,[a]) -> [[a]];
bound_plane boundaries plane =
let {
	boundary_planes = 
	[ (b,[if j==i then 1 else 0 | j <- [0..length boundaries-1]])
	| (bs,i) <- zip boundaries [0..], b <- bs];
	candidates = catMaybes $ map (intersect_planes . (plane:)) $ choose2 boundary_planes;
	filter_candidate _ [] = True;
	filter_candidate ([b0,b1]:bs) (c:cs) = b0 <= c && c <= b1 && filter_candidate bs cs;
} in nub $ filter (filter_candidate boundaries) candidates;

x `dot` y = sum $ zipWith (*) x y;

-- Vector subtraction.
($-$) :: (Num a) => [a] -> [a] -> [a];
($-$) = zipWith (-);

-- Cross product.
cross :: (Num a) => [a] -> [a] -> [a];
-- (. return) . (:) == \x y -> [x,y]
x `cross` y = zipWith (*) (cycle [1,-1]) . map determinant . minors $ [x,y];

colinear [] = True;
colinear [x] = True;
colinear [x,x'] = True;
colinear (x:x':x'':xs) = (all (==0) $ (x'$-$x) `cross` (x''$-$x)) && colinear (x':x'':xs);

-- Produce an array of vertices on the given plane that circles the normal counter-clockwise.
--order_vertices :: (a,[a]) -> [[a]] -> [[a]];
order_vertices (dist,normal) vertices' =
let {
-- Three non-colinear vertices.
	([v0,v1,v2],vertices) =
	let {
		(v0:vs) = vertices';
		[v1,v2] = head $ dropWhile (colinear . (v0:) . (take 2)) $ choose2 vs;
	} in ([v0,v1,v2], tail $ dropWhile (v2/=) vs);
-- The first triangle.
	initial =
	if ((v1 $-$ v0) `cross` (v2 $-$ v0)) `dot` normal > 0 -- The vectors should follow the left hand rule.
		then [v0,v1,v2]
		else [v0,v2,v1];
	insert_vertex face vertex =
	let {
		face' = last face : face;
		holes = zip (map reverse $ tail $ inits $ face') (tail $ tails $ face');
		test_hole v ([],_) = False;
		test_hole v (_,[]) = False;
		-- Deal with cycles.
		test_hole v ((x,y),(xs,ys)) =
		let {
		-- Normals pointing away from the interior of the face.
			nx = normal `cross` (x $-$ v);
			ny = (y $-$ v) `cross` normal;
		-- Displacements.
			dx = x `dot` nx;
			dy = y `dot` ny;
			colinear_and_between x y v = colinear [x,y,v] && (x$-$v) `dot` (y$-$v) < 0;
		} in all (\t -> all (\(n,d) -> n`dot`t - d <= 0) [(nx,dx),(ny,dy)]) (xs++ys);
		rejoin v (xs,ys) = (reverse $ v:(xs)) ++ (ys);
	} in rejoin vertex $ head $ dropWhile (not . test_hole vertex) holes;

	f [v0,v1,v2] = 
	let {
		nx = (v2 $-$ v1) `cross` normal;
		dx = v1 `dot` nx;
	} in v0 `dot` nx - dx;
} in (\x -> (x,map (map f . filter ((>=3) . length) . map (take 3) . tails) x)) $ scanl insert_vertex initial vertices;
}


