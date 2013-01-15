-- extract_maps.hs
--

module Main (main) where {
import Text.ParserCombinators.Parsec;
import Data.Word;
import Data.Binary.Get;
import Data.Binary.IEEE754;
import qualified Data.ByteString.Lazy as BL;
import qualified Data.Char;
import qualified Data.List;
import qualified Data.Bits;
import qualified Data.Map;
import qualified Data.Array as Arr;
import Data.Map ((!));
import qualified Text.JSON as JS;

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
	if name `elem` ["entities","planes","vertices","visilist","nodes","faces","leaves","lface","edges","ledges","models"] then
	runGet
		(load_until $ load_entity name)
		(BL.take (fromIntegral size) $ BL.drop (fromIntegral offset) input);
	else [];
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

load_entity :: String -> Get (Entity Float);
load_entity "entities" = do {
	str <- getRemainingLazyByteString;
	return $ EntityEntities {
		entity_contents = map (Data.Char.chr . fromIntegral) $ BL.unpack $ str
	};
};
load_entity "planes" = do {
	normal <- sequence $ replicate 3 getFloat32le;
	dist <- getFloat32le;
	planetype <- m_fromIntegral getWord32le;
	return $ EntityPlane {
		plane_normal = normal,
		plane_dist = dist,
		plane_type = planetype
	};
};
load_entity "vertices" = do {
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
load_entity "visilist" = do {
	list <- getRemainingLazyByteString;
	return $ EntityVisibilityList {
		visibility_list = list
	}
};
load_entity "nodes" = do {
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
load_entity "faces" = do {
	plane_id <- m_fromIntegral $ getWord16le;
--           must be in [0,numplanes[ 
	side <- m_fromIntegral $ getWord16le;
	ledge_id <- m_fromIntegral $ getInt32le; -- Int32               -- first edge in the List of edges
--           must be in [0,numledges[
	ledge_num <- m_fromIntegral $ getWord16le;
	texinfo_id <- m_fromIntegral $ getWord16le;
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
load_entity "leaves" = do {
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
load_entity "lface" = do {
	index <- m_fromIntegral $ getWord16le;
	return $ EntityFaceListEntry {
		face_list_index = index
	};
};
load_entity "edges" = do {
	vertexes <- sequence $ replicate 2 $ m_fromIntegral getWord16le;
	return $ EntityEdge {
		edge_vertexes = vertexes
	};
};
load_entity "ledges" = do {
	index <- m_fromIntegral getInt32le;
	return $ EntityEdgeListEntry {
		edge_list_index = if index<0 then Right (-index) else Left index
	};
};
load_entity "models" = do {
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

load_entity s = do {
	return $ EntityUndefined s;
};

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

marshall_json :: Header Float -> JS.JSValue;
marshall_json header =
let {
	marshall_general f name = JS.showJSON $ map (JS.showJSON . f) $ entities_items $ (dheader_entities header) ! name;
	marshall_vertex vertex = map ($vertex) [vertex_x,vertex_y,vertex_z];
	marshall_leaf leaf = JS.toJSObject $
	[
		("visilist start", JS.showJSON $ leaf_visilist leaf),
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
		("vertices index", JS.showJSON $ face_vertices)
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
	("visibility list",visilist),
	("player start",player_start_entity)
];

main =
do {
	input <- BL.getContents;
	let {
		header = runGet (load_header input) input;
		--directory = runGet (load_directory input) $ BL.drop (fromIntegral $ infotableofs header) input;
	} in do {
		putStrLn $ JS.encode $ marshall_json header;
		--print header;
		--putStrLn $ entity_contents $ head $ entities_items $ dheader_entities header Data.Map.! "entities";
		--print $ parse_entity $ entity_contents $ head $ entities_items $ dheader_entities header Data.Map.! "entities";
		--print $ entities_items $ dheader_entities header Data.Map.! "entities";
		--print $ map face_ledge_num $ entities_items $ dheader_entities header Data.Map.! "faces";
	}
}

}
