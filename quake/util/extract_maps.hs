-- extract_maps.hs
--

module Main (main) where {
import System.FilePath.Posix as FP;
import Control.Arrow (first,second);
import System.Exit;
import System.Environment;
import Data.Maybe (isNothing);
import TexturePacking;
import qualified System.Directory;
import Data.Function (on);
import Control.Monad;
import Text.ParserCombinators.Parsec;
import Data.Word;
import Data.Binary.Get;
import Data.Binary.IEEE754;
import qualified Data.ByteString as BS;
import qualified Data.ByteString.Lazy as BL;
import qualified Data.Char;
import qualified Data.List;
import qualified Data.Bits;
import qualified Data.Map;
import qualified Data.Array as Arr;
import Data.Map ((!));
import qualified Text.JSON as JS;
import Codec.Picture;

data Header f = Header {
	dheader_version :: Integer,
	dheader_entities :: Data.Map.Map String (Entities f)
} deriving (Show);

m_fromIntegral :: (Integral a,Monad m) => m a -> m Integer;
m_fromIntegral = (flip (>>=)) $ return . fromIntegral;

load_string :: (Integral a) => Get a -> Integer -> Get String;
load_string g n =
do {
	bytes <- sequence $ replicate (fromIntegral n) g;
	return $ map (Data.Char.chr . fromIntegral) $ removeNulls $ bytes;
} where {
	removeNulls = reverse . dropWhile (==0) . reverse;
};

load_until :: Get a -> Get [a];
load_until f =
do {
	empty <- isEmpty;
	if empty
		then return []
		else do {
			v <- f;
			rest <- load_until f;
			return $ v:rest;
		}
};

load_header :: BL.ByteString -> Get (Header Float);
load_header input =
do {
	--identification <- load_string getWord8 8;
	identification <- m_fromIntegral $ getWord32le;
	entities <- mapM (\name -> load_entities name input >>= \entities -> return (name,entities)) entities_names;
	--numlumps <- getWord32le;
	--infotableofs <- getWord32le;
	return $ Header identification $ Data.Map.fromList entities;
} where {
	entities_names = [
		"entities",           -- List of Entities.
		"planes",             -- Map Planes.
		"miptex",             -- Wall Textures.
		"vertices",           -- Map Vertices.
		"visilist",           -- Leaves Visibility lists.
		"nodes",              -- BSP Nodes.
		"texinfo",            -- Texture Info for faces.
		"faces",              -- Faces of each surface.
		"lightmaps",          -- Wall Light Maps.
		"clipnodes",          -- clip nodes, for Models.
		"leaves",             -- BSP Leaves.
		"lface",              -- List of Faces.
		"edges",              -- Edges of faces.
		"ledges",             -- List of Edges.
		"models"             -- List of Models.
	]
};

data Entities f = Entities {
	entities_offset :: Integer, -- Offset to entry, in bytes, from start of file
	entities_size :: Integer, -- Size of entry in file, in bytes
	entities_items :: [Entity f] -- The entities themselves
} deriving (Show);

load_entities :: String -> BL.ByteString -> Get (Entities Float);
load_entities name input = do {
	offset <- m_fromIntegral getWord32le;
	size <- m_fromIntegral getWord32le;
	return $ Entities {
		entities_offset = offset,
		entities_size = size,
		entities_items = load_entity_list name offset size input
	};
} where {
	load_entity_list name offset size input =
	if name `elem` [
		"entities","planes","miptex","vertices","visilist",
		"nodes","texinfo","faces","leaves","lface","edges","ledges","models"] then
	let {
		contents = (BL.take (fromIntegral size) $ BL.drop (fromIntegral offset) input);
	} in case name of {
		"miptex" -> [runGet (load_entity contents name) (BL.drop (fromIntegral offset) input)];
		_ -> runGet (load_until $ load_entity contents name) contents;
	}
	else [];
};

data Miptex = Miptex {
	miptex_index :: Integer,
	miptex_name :: String,
	miptex_width :: Integer,
	miptex_height :: Integer,
	miptex_images :: [Image Pixel8]
	--miptex_images :: [Integer]
} deriving (Show);

instance (Show a) => Show (Image a) where {
	show _ = "{image}";
};

data Entity f
= EntityEntities {
	entity_contents :: String
}
| EntityPlane {
	plane_normal :: [f],
	plane_dist :: f,
	plane_type :: Integer
}
| EntityMiptex {
	--miptex_offsets :: [Integer]
	miptex_textures :: [Miptex]
}
| EntityVertex {
	vertex_x :: f,
	vertex_y :: f,
	vertex_z :: f
}
| EntityVisibilityList {
	visibility_list :: BL.ByteString
}
| EntityNode {
	node_planenum :: Integer, -- Int32
	node_children :: [Either Integer Integer], -- Int16[2]	-- negative numbers are -(leafs+1), not nodes
	node_mins :: [Integer], -- Int16[3]		-- for sphere culling
	node_maxs :: [Integer], -- Int16[3]
	node_firstface :: Integer, -- Word16
	node_numfaces :: Integer -- Word16	-- counting both sides
}
| EntityTexinfo {
	texinfo_vectors :: [[f]],
	texinfo_dists :: [f],
	texinfo_index :: Integer,
	texinfo_animated :: Integer
}
| EntityFace {
	face_plane_id :: Integer, -- Word16            -- The plane in which the face lies
--           must be in [0,numplanes[ 
	face_side_front :: Bool, -- Word16                -- 0 if in front of the plane, 1 if behind the plane
	face_ledge_id :: Integer, -- Int32               -- first edge in the List of edges
--           must be in [0,numledges[
	face_ledge_num :: Integer, -- Word16           -- number of edges in the List of edges
	face_texinfo_id :: Integer, -- Word16          -- index of the Texture info the face is part of
--           must be in [0,numtexinfos[ 
	face_typelight :: Integer, -- Word8            -- type of lighting, for the face
	face_baselight :: Integer, -- Word8            -- from 0xFF (dark) to 0 (bright)
	face_light :: [Integer], -- Word8,Word8             -- two additional light models  
	face_lightmap :: Integer -- Int32               -- Pointer inside the general light map, or -1
-- this define the start of the face light map
}
| EntityLeaf {
	leaf_type :: Integer, -- Int32 -- Special type of leaf
	leaf_visilist :: Integer, -- Int32 -- Beginning of visibility lists
-- must be -1 or in [0,numvislist[
	leaf_mins :: [Integer], -- Int16[3]	
	leaf_maxs :: [Integer], -- Int16[3] -- Bounding box of the leaf
	leaf_lface_id :: Integer, -- Word16 -- First item of the list of faces
-- must be in [0,numlfaces[
	leaf_lface_num :: Integer, -- Word16 -- Number of faces in the leaf 
	leaf_sndwater :: Integer, -- Word8 -- level of the four ambient sounds:
	leaf_sndsky :: Integer, -- Word8 -- 0 is no sound
	leaf_sndslime :: Integer, -- Word8 -- 0xFF is maximum volume
	leaf_sndlava :: Integer -- Word8 --
}
| EntityFaceListEntry {
	face_list_index :: Integer -- index of the face
}
| EntityEdge {
	edge_vertexes :: [Integer]	-- index of the vertexes
											-- must be in [0,numvertices[
}
| EntityEdgeListEntry {
	edge_list_index :: Either Integer Integer -- index of the edge
}
| EntityModel {
	model_mins :: [f],	-- Float32^3
	model_maxs :: [f],	-- Float32^3
	model_origin :: [f],	-- Float32^3
	model_nodes :: [Integer],	-- Int32^4
	model_numleafs :: Integer,	-- Int32
	model_face_id :: Integer,	-- Int32
	model_face_num :: Integer	-- Int32
}
| EntityUndefined String deriving (Show);

load_entity :: BL.ByteString -> String -> Get (Entity Float);
load_entity _ "entities" = do {
	str <- getRemainingLazyByteString;
	return $ EntityEntities {
		entity_contents = map (Data.Char.chr . fromIntegral) $ BL.unpack $ str
	};
};
load_entity _ "planes" = do {
	normal <- sequence $ replicate 3 getFloat32le;
	dist <- getFloat32le;
	planetype <- m_fromIntegral getWord32le;
	return $ EntityPlane {
		plane_normal = normal,
		plane_dist = dist,
		plane_type = planetype
	};
};
load_entity contents "miptex" = do {
	numtex <- m_fromIntegral getInt32le;
	offsets <- sequence $ replicate (fromIntegral numtex) $ m_fromIntegral getInt32le;
	--offsets <- sequence $ replicate numtex $ m_fromIntegral getWord32le;
	--str <- getRemainingLazyByteString;
	--offsets <- sequence $ replicate 4 $ getInt32le;
	return $ EntityMiptex {
		--miptex_offsets = offsets
		miptex_textures =
			map (\(index,offset) -> load_miptex (fromIntegral offset) index)
			$ filter ((>=0) . snd) $ zip [0..] offsets
	};
} where {
	{-
	runGet
		(load_until $ load_entity name)
		(BL.take (fromIntegral size) $ BL.drop (fromIntegral offset) input);
	-}
	{-
	miptex_name :: String,
	miptex_width :: Integer,
	miptex_height :: Integer,
	miptex_images :: [BL.Bytestring]
	-}
	load_miptex offset index = (flip runGet) miptex_contents $
	do {
		name <- sequence $ replicate 16 getWord8;
		--name <- sequence $ replicate 16 (getWord8 >>= return . Data.Char.chr . fromIntegral);
		width <- m_fromIntegral getWord32le;
		height <- m_fromIntegral getWord32le;
		image_offsets <- sequence $ replicate 4 $ m_fromIntegral getInt32le;
		images <- mapM
			(\(miplevel,offset) -> return $ load_texture miptex_contents width height miplevel offset) $
			filter (\(_,offset) -> offset > 0) $
			zip [0..3] image_offsets;
		return $ Miptex {
			miptex_index = index,
			miptex_name = map (Data.Char.chr . fromIntegral) $ takeWhile (/=0) name,
			miptex_width = width,
			miptex_height = height,
			miptex_images = if width*height > 0 then images 
			--(fromIntegral $ BL.length miptex_contents, image_offsets)
			--map (size) images
			else []
		};
	} where {
		miptex_contents = BL.drop (fromIntegral offset) contents;
	};
	load_texture contents width height miplevel offset = 
	extract_image (fromIntegral width',fromIntegral height') indexes
	where {
		indexes = 
			(BL.take $ fromIntegral size) .
			(BL.drop $ fromIntegral offset) $
			contents;
		width' = width `div` (2^miplevel);
		height' = height `div` (2^miplevel);
		size = width' * height';
	};
};
load_entity _ "vertices" = do {
	x' <- getFloat32le;
	y' <- getFloat32le;
	z' <- getFloat32le;
	return $ EntityVertex {
		vertex_x = x',
		vertex_y = y',
		vertex_z = z'
	};
} where {
	float_bytes = 4;
};
load_entity _ "visilist" = do {
	list <- getRemainingLazyByteString;
	return $ EntityVisibilityList {
		visibility_list = list
	}
};
load_entity _ "nodes" = do {
	planenum <- m_fromIntegral $ getInt32le;
	children <- fmap (map readChild) $ sequence $ replicate 2 $ m_fromIntegral getInt16le;
	mins <- sequence $ replicate 3 $ m_fromIntegral getInt16le;
	maxs <- sequence $ replicate 3 $ m_fromIntegral getInt16le;
	firstface <- m_fromIntegral $ getWord16le;
	numfaces <- m_fromIntegral $ getWord16le;
	return $ EntityNode {
		node_planenum = planenum,
		node_children = children,
		node_mins = mins,
		node_maxs = maxs,
		node_firstface = firstface,
		node_numfaces = numfaces
	};
} where {
	readChild x = if not $ Data.Bits.testBit x 15 then Left x else Right $ -x - 1;
};
load_entity _ "texinfo" = do {
	vector_dist <- sequence $ replicate 2 load_vector_dist;
	index <- m_fromIntegral getWord32le;
	animated <- m_fromIntegral getWord32le;
	return $ EntityTexinfo {
		texinfo_vectors = map fst vector_dist,
		texinfo_dists = map snd vector_dist,
		texinfo_index = index,
		texinfo_animated = animated
	};
} where {
	load_vector_dist = do {
		vec <- sequence $ replicate 3 getFloat32le;
		dist <- getFloat32le;
		return (vec,dist);
	};
};
load_entity _ "faces" = do {
	plane_id <- m_fromIntegral $ getWord16le;
--           must be in [0,numplanes[ 
	side <- m_fromIntegral $ getWord16le;
	ledge_id <- m_fromIntegral $ getInt32le; -- Int32               -- first edge in the List of edges
--           must be in [0,numledges[
	ledge_num <- m_fromIntegral $ getWord16le;
	texinfo_id <- m_fromIntegral $ getInt16le;
--           must be in [0,numtexinfos[ 
	typelight <- m_fromIntegral $ getWord8;
	baselight <- m_fromIntegral $ getWord8;
	light <- sequence $ replicate 2 $ m_fromIntegral getWord8; -- Word8,Word8
		-- two additional light models  
	lightmap <- m_fromIntegral $ getInt32le; -- Int32               -- Pointer inside the general light map, or -1
	return $ EntityFace {
		face_plane_id = plane_id,
		face_side_front = side == 0,
		face_ledge_id = ledge_id,
		face_ledge_num = ledge_num,
		face_texinfo_id = texinfo_id,
		face_typelight = typelight,
		face_baselight = baselight,
		face_light = light,
		face_lightmap = lightmap
	};
};
load_entity _ "leaves" = do {
	leaftype <- m_fromIntegral getInt32le; -- Special type of leaf
	vislist <- m_fromIntegral getInt32le; -- Beginning of visibility lists
	mins <- sequence $ replicate 3 $ m_fromIntegral getInt16le;
	maxs <- sequence $ replicate 3 $ m_fromIntegral getInt16le;
	lface_id <- m_fromIntegral getWord16le; -- First item of the list of faces
	lface_num <- m_fromIntegral getWord16le; -- Number of faces in the leaf 
	sndwater <- m_fromIntegral getWord8; -- level of the four ambient sounds:
	sndsky <- m_fromIntegral getWord8; -- 0 is no sound
	sndslime <- m_fromIntegral getWord8; -- 0xFF is maximum volume
	sndlava <- m_fromIntegral getWord8; --
	return $ EntityLeaf {
		leaf_type = leaftype,
		leaf_visilist = vislist,
		leaf_mins = mins,
		leaf_maxs = maxs,
		leaf_lface_id = lface_id,
		leaf_lface_num = lface_num,
		leaf_sndwater = sndwater,
		leaf_sndsky = sndsky,
		leaf_sndslime = sndslime,
		leaf_sndlava = sndlava 
	}
};
load_entity _ "lface" = do {
	index <- m_fromIntegral $ getWord16le;
	return $ EntityFaceListEntry {
		face_list_index = index
	};
};
load_entity _ "edges" = do {
	vertexes <- sequence $ replicate 2 $ m_fromIntegral getWord16le;
	return $ EntityEdge {
		edge_vertexes = vertexes
	};
};
load_entity _ "ledges" = do {
	index <- m_fromIntegral getInt32le;
	return $ EntityEdgeListEntry {
		edge_list_index = if index<0 then Right (-index) else Left index
	};
};
load_entity _ "models" = do {
	mins <- sequence $ replicate 3 getFloat32le;
	maxs <- sequence $ replicate 3 getFloat32le;
	origin <- sequence $ replicate 3 getFloat32le;
	nodes <- sequence $ replicate 4 $ m_fromIntegral getInt32le;
	numleafs <- m_fromIntegral getInt32le;
	face_id <- m_fromIntegral getInt32le;
	face_num <- m_fromIntegral getInt32le;
	return $ EntityModel {
		model_mins = mins,
		model_maxs = maxs,
		model_origin = origin,
		model_nodes = nodes,
		model_numleafs = numleafs,
		model_face_id = face_id,
		model_face_num = face_num
	};
};

load_entity _ s = do {
	return $ EntityUndefined s;
};

extract_image :: (Int,Int) -> BL.ByteString -> Image Pixel8;
extract_image (width,height) indexes =
let {
	--get_pixel i j = BL.index indexes (fromIntegral $ j*width+i);
	get_pixel i j = BL.index indexes (fromIntegral $ pixelBaseIndex image i j);
	image = generateImage get_pixel width height;
} in image;

parse_entity :: String -> Either ParseError [Data.Map.Map String String];
parse_entity input = parse parse_entity "(unknown)" input
where {
	parse_entity :: GenParser Char st [Data.Map.Map String String];
	parse_entity = do {
		result <- many entity;
		--eof;	
		return result;
	};
	entity = do {
		char '{';
		eol;
		result <- many line;
		char '}';
		eol;
		return $ Data.Map.fromList result;
	};
	line = do {
		char '"';
		key <- many (noneOf "\"");
		char '"';
		spaces;
		char '"';
		value <- many (noneOf "\"");
		char '"';
		eol;
		return (key,value);
	};
	eol = char '\n';
};

getInt32le = m_fromIntegral getWord32le >>= return . signed 32;
getInt16le = m_fromIntegral getWord16le >>= return . signed 16;

signed :: Integer -> Integer -> Integer;
signed n x = if x >= 2^(n-1) then x-2^n else x;

marshall_json :: String -> Header Float -> JS.JSValue;
marshall_json filename header =
let {
	marshall_general f name = JS.showJSON $ map (JS.showJSON . f) $ entities_items $ (dheader_entities header) ! name;
	marshall_vertex vertex = map ($vertex) [vertex_x,vertex_y,vertex_z];
	marshall_texinfo texinfo = JS.toJSObject $
	[
		("vectors",JS.showJSON $ texinfo_vectors texinfo),
		("displacements",JS.showJSON $ texinfo_dists texinfo),
		("miptex index",JS.showJSON $ texinfo_index texinfo)
	];
	marshall_leaf leaf = JS.toJSObject $
	[
		("visilist start", JS.showJSON $ leaf_visilist leaf),
		("type", JS.showJSON $ leaf_type leaf),
		("face indexes", JS.showJSON $ face_indices)
	] where {
		face_indices = [face_indexes Arr.! (fromIntegral $ leaf_lface_id leaf + i) | i <- [0..leaf_lface_num leaf-1]];
	};
	face_indexes = make_array face_list_index "lface";
	marshall_plane plane = JS.toJSObject $
	[
		("normal", JS.showJSON $ plane_normal plane),
		("dist", JS.showJSON $ plane_dist plane)
	];
	marshall_face face = JS.toJSObject $
	[
		("plane id", JS.showJSON $ face_plane_id face),
		("front side", JS.showJSON $ face_side_front face),
		("vertices index", JS.showJSON $ face_vertices),
		("texture index", JS.showJSON $ face_texinfo_id face)
		{-
		face_plane_id = plane_id,
		face_side_front = side == 0,
		face_ledge_id = ledge_id,
		face_ledge_num = ledge_num,
		face_texinfo_id = texinfo_id,
		face_typelight = typelight,
		face_baselight = baselight,
		face_light = light,
		face_lightmap = lightmap
		-}
	] where {
		edge_selections = [edge_indexes Arr.! (fromIntegral $ face_ledge_id face + i) 
			| i <- [0..fromIntegral $ face_ledge_num face-1]];
		face_vertices = map choose_vertex edge_selections;
		choose_vertex (Left x) = (edges Arr.! fromIntegral x) !! 0;
		choose_vertex (Right x) = (edges Arr.! fromIntegral x) !! 1;
	};
	make_array f name = Arr.listArray (0,length list - 1) list
	where {
		list = map f $ entities_items $ (dheader_entities header) ! name;
	};
	edge_indexes = make_array edge_list_index "ledges";
	edges = make_array edge_vertexes "edges";

	player_start_entity = JS.showJSON $ JS.toJSObject $ 
	[
		("origin",JS.showJSON (origin_coordinates :: [Float])),
		("angle",JS.showJSON (angle :: Integer))
	]
	where {
		contents = entity_contents $ head $ entities_items $ (dheader_entities header) ! "entities";
		entities = (\(Right x) -> x) $ parse_entity contents;
		player_start = head $ filter (\x -> x!"classname" == "info_player_start") entities;
		origin = player_start ! "origin";
		origin_coordinates = map read $ words origin;
		angle = read $ player_start ! "angle";
	};
	marshall_node node = JS.toJSObject $
	[
		("plane id", JS.showJSON $ node_planenum node),
		("children nodes", JS.showJSON $
			map (\x -> case x of { Left x -> JS.showJSON x; Right _ -> JS.JSNull}) $ node_children node),
		("children leaves", JS.showJSON $
			map (\x -> case x of { Right x -> JS.showJSON x; Left _ -> JS.JSNull}) $ node_children node)
	];
	visilist = 
		JS.showJSON $
		BL.unpack $ 
		visibility_list $
		head $ entities_items $ (dheader_entities header) ! "visilist";
} in JS.showJSON $ JS.toJSObject $ [
	("vertices",marshall_general marshall_vertex "vertices"),
	("leaves",marshall_general marshall_leaf "leaves"),
	("planes",marshall_general marshall_plane "planes"),
	("faces",marshall_general marshall_face "faces"),
	("nodes",marshall_general marshall_node "nodes"),
	("textures",marshall_general marshall_texinfo "texinfo"),
	("visibility list",visilist),
	("player start",player_start_entity),
	("filename",JS.showJSON filename)
];

-- Placement {placement_begin = (320,0), placement_size = (320,192), placement_item = [("pak0/maps/e1m7.bsp",4)]}
marshall_texture_index_json :: [Placement [(String,Miptex)]] -> JS.JSValue;
marshall_texture_index_json ps =
let {
	filenames =
		Data.List.union []
		$ concatMap (\p -> map (\(fn,_) -> fn) (placement_item p)) ps;
	gather_textures filename = JS.showJSON 
	$ JS.toJSObject 
	$ map (first show)
	$ Data.List.sortBy (compare `on` fst)
	[ 
		(miptex_index miptex,
		JS.toJSObject [
			("index",JS.showJSON $ miptex_index miptex),
			("texture name",JS.showJSON $ miptex_name miptex),
			("begin",JS.showJSON $ placement_begin p),
			("size",JS.showJSON $ placement_size p)
		])
	| p <- ps, (fn,miptex) <- placement_item p, fn == filename];
} in JS.showJSON $ JS.toJSObject $ [
(filename, gather_textures filename)
| filename <- filenames
];

main = getArgs >>= handle_args;
{-main =
do {
	input <- BL.getContents;
	let {
		header = runGet (load_header input) input;
		--directory = runGet (load_directory input) $ BL.drop (fromIntegral $ infotableofs header) input;
	} in do {
		--putStrLn $ JS.encode $ marshall_json header;
		
		--print header;
		--putStrLn $ entity_contents $ head $ entities_items $ dheader_entities header Data.Map.! "entities";
		--print $ parse_entity $ entity_contents $ head $ entities_items $ dheader_entities header Data.Map.! "entities";
		--print $ entities_items $ dheader_entities header Data.Map.! "entities";
		--print $ map face_ledge_num $ entities_items $ dheader_entities header Data.Map.! "faces";
		--print $ 
	--BS.putStr
	
	palette <- BS.readFile "palette.lmp" >>=
		return .
		(PixelRGBA8 0 0 0 0:) .
		map (\[r,g,b] -> PixelRGBA8 r g b 255) .
		tail . map (take 3) . takeWhile (not . null) . iterate (drop 3) . BS.unpack;

	{-
	mapM_ (\(x,(mip,img)) ->
	let dirname = "textures/mip_" ++ show mip ++ "/res_" ++ (show $ miptex_width x)++ "x" ++ (show $ miptex_height x);
	in System.Directory.createDirectoryIfMissing True dirname
	>>
	BS.writeFile (dirname ++ "/" ++ miptex_name x ++ ".png") img
	) 
	$ concat 
--	$ Data.List.maximumBy (compare `on` BS.length .)
	$ map (\x -> 
	map
	(\(mip,img) ->
		(x,(mip,encodePng . pixelMap (\a -> palette !! fromIntegral a) $ img))
	)
	(zip [0..] $ miptex_images x)
	) 
	$ miptex_textures $ head $ entities_items $ dheader_entities header Data.Map.! "miptex";
	-}
	--print $ map miptex_images $ miptex_textures $ head $ entities_items $ dheader_entities header Data.Map.! "miptex";
	BS.writeFile "texture.png" $ encodePng $ pack_textures
		palette
		(map (head . miptex_images) $ concat $ map miptex_textures $ entities_items $ dheader_entities header Data.Map.! "miptex");
	};
};
-}

for = flip map;

handle_args [] = exitFailure;
handle_args
("--out":outpath
:"--extract-map"
:"--palette":palette_filename
:"--extract-texture":fs) = 
--handle_args ("--out":outpath:"--extract-map":fs) =
let {
	truncate_filepath = FP.joinPath . drop (length filepath_prefix) . FP.splitDirectories;
	filepath_prefix =
		Data.List.takeWhile ((==1) . length)
		$ map (Data.List.union [])
		$ Data.List.transpose
		$ map FP.splitDirectories
		$ fs;
	out_filename path =
	let {
		subpath = truncate_filepath path;
		fn = FP.takeFileName subpath `FP.replaceExtension` "json" ;
	} in FP.replaceFileName (outpath </> subpath) fn;
	padding = 0;
} in do {

-- Extract the maps.
	headers <- forM fs $ \filepath -> do {
		input <- BL.readFile filepath;
		--print (takeDirectory $ out_filename filepath);
			System.Directory.createDirectoryIfMissing True (takeDirectory $ out_filename filepath);
		let {
			header = runGet (load_header input) input;
		} in do {
		--print (out_filename filepath);
			writeFile (out_filename filepath) $ JS.encode $ marshall_json (truncate_filepath filepath) header;
			return (filepath,header);
		}
	};

	--putStrLn $ JS.encode $ marshall_json filepath header;
	palette <- BS.readFile palette_filename >>=
		return .
		map (\[r,g,b] -> PixelRGBA8 r g b 255) .
		map (take 3) . takeWhile (not . null) . iterate (drop 3) . BS.unpack;

--print $ length $ sort_nub (compare `on` imageData) $ concat $ images;
--print $ length $ concat $ images;
	let {
	images =
	--forM fs $ \filename ->
	for headers $ \(filename,header) ->
		--input <- BL.readFile filename;
		let {
			--header = runGet (load_header input) input;
			miptexes = concatMap miptex_textures
				$ entities_items 
				$ dheader_entities header Data.Map.! "miptex";
		} in map (\miptex -> ((filename,miptex),  head . miptex_images $ miptex)) miptexes;
	;

		(tree,texture_image) = pack_textures padding palette 
		$ map (\g -> (map fst g,snd $ head g))
		$ sort_group (compare `on` (imageData . snd)) 
		$ concat 
		$ images;
		positions = placements (0,0) 
			$ map_tree (map (\(fn,miptex) -> (handle_filename fn,miptex)) . fst)
			$ tree;
		filepath_prefix =
			Data.List.takeWhile ((==1) . length)
			$ map (Data.List.union [])
			$ Data.List.transpose
			$ map FP.splitDirectories
			$ fs;
		handle_filename fn = FP.joinPath $ drop (length filepath_prefix) $ FP.splitDirectories $ fn;
		{-filepath_suffixes = 
			map (\(fn,x) -> (x,drop (length filepath_prefix) . splitDirectories $ fn))
			$ concatMap placement_item
			$ positions;-}
	} in do {
		System.Directory.createDirectoryIfMissing True outpath;

		BS.writeFile (outpath </> "texture.png") $ encodePng texture_image;
		--mapM print positions;
		--mapM print filepath_suffixes;
		--print filepath_prefix;
		writeFile (outpath </> "texture_index.json") $ JS.encode $ marshall_texture_index_json positions;
	};

	exitSuccess;
};


sort_group :: (a -> a -> Ordering) -> [a] -> [[a]];
sort_group cmp = Data.List.groupBy (\x y -> EQ == x `cmp` y) . Data.List.sortBy cmp;

pack_textures :: Integer -> [PixelRGBA8] -> [(a,Image Pixel8)] -> (Tree (a,Image PixelRGBA8),Image PixelRGBA8);
pack_textures padding palette textures =
let {
do_packing :: Integer -> (Integer,Integer) -> [PixelRGBA8] -> [(a,Image Pixel8)]
-> Maybe (Tree (a, Image PixelRGBA8));
do_packing padding (width,height) palette textures = let {
	tree_element (a,img) =
	let {
		w = fromIntegral $ imageWidth img;
		h = fromIntegral $ imageHeight img;
	} in ((a,img),(w,h));
	lookup_palette = pixelMap (\a -> palette !! fromIntegral a);
	tree =
	pack_boxes (width,height)
	$ map tree_element
	$ map (second (lookup_palette))
	$ textures;
} in tree;

inside_box (x,y) (w,h) padding = all (\(t,l) -> padding<=t && t<l-padding) [(x,w),(y,h)];

get_pixel x y =
query_tree (\(x',y') (_,img) -> 
if inside_box (x',y') (fromIntegral $ imageWidth img,fromIntegral $ imageHeight img) padding
	{-padding<=x' && x'<w-padding &&
	padding<=y' && y'<h-padding-}
		then pixelAt img (fromIntegral $ x'-padding) (fromIntegral $ y'-padding)
		else base_color
)
base_color tree (fromIntegral x,fromIntegral y);

base=256;
--base_color = palette !! 0;
base_color = PixelRGBA8 0 0 0 0;
increment = 16;
(tsize,Just tree) = head
	$ dropWhile (\(_,t) -> isNothing t)
	$ map (\x -> (x,do_packing padding (x,x) palette textures)) [base,base+increment..];
resize_image img =
let {
	iwidth = imageWidth img;
	iheight = imageHeight img;
	isize' = max iwidth iheight;
	isize = head $ dropWhile (<isize') $ iterate (*2) 1;
	f x y = if x<iwidth && y<iheight then pixelAt img x y else base_color;
} in generateImage f isize isize;
} in (tree, resize_image$ generateImage get_pixel (fromIntegral tsize) (fromIntegral tsize));

}
