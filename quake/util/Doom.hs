-- Doom.hs

{-# LANGUAGE TemplateHaskell #-}

module Doom where {

import qualified Data.ByteString.Lazy as BL;
import Data.Binary (Get,getWord8);
import Data.Binary.Get (runGet,isEmpty,getRemainingLazyByteString);
import Data.Binary.IEEE754 (getFloat32le);
import Data.Char (chr);
import Data.Word (Word8);
import Control.Monad (forM);

data TypeDefinition
 = TypeInteger Integer
 | TypeFloat Integer
 | TypeString (Maybe Integer)
 | TypeBuffer String String TypeDefinition
 | TypePointer String String TypeDefinition
 | TypeCustom [(String,TypeDefinition)]
 | TypeEmpty
 ;
data TypeStore
 = StoreInteger Integer
 | StoreFloat Float
 | StoreString String
 | StorePointer [TypeStore]
 | StoreCustom [(String,TypeStore)]
 | StoreEmpty
 deriving (Show);

wad_definition = TypeCustom [
("identification",TypeString $ Just 4),
("numlumps",TypeInteger 4),
("infotableofs",TypeInteger 4),
("lumps",TypePointer "numlumps" "infotableofs" $
	TypeCustom [
		("filepos",TypeInteger 4),
		("size",TypeInteger 4),
		("name",TypeString $ Just 8)
	]
)];
{-bsp_definition = TypeCustom (
("version",TypeInteger 4):
[
(field_name,TypeCustom [
	("offset",TypeInteger 4),
	("size",TypeInteger 4),
	("data",TypeBuffer "size" "offset" (TypeInteger 1))
])
| field_name <- ["entities","planes","miptex","vertices","visilist","nodes",
	"texinfo","faces","lightmaps","clipnodes","leaves","lface","edges","ledges",
	"models"]
]
);-}
bsp_definition = TypeCustom (
("version",TypeInteger 4):
[
("entities",f $ TypeString Nothing),
("planes",f $ TypeCustom [
	("normal",vec3),
	("dist",scalar),
	("type",long)
	]),
("miptex",f $ TypeEmpty),
("vertices",f $ vec3),
("visilist",f $ TypeInteger 1),
("nodes", f $ TypeCustom [
	("plane_id",long),
	("front",u_short),
	("back",u_short),
	("box",bboxshort),
	("face_id",u_short),
	("face_num",u_short)
	]),
("texinfo", f $ TypeCustom [
	("vectorS",vec3),
	("distS",scalar),
	("vectorT",vec3),
	("distT",scalar),
	("texture_id",u_long),
	("animated",u_long)
	]),
("faces", f $ TypeCustom [
	("plane_id",u_short),
	("side",u_short),
	("ledge_id",long),
	("ledge_num",u_short),
	("texinfo_id",u_short),
	("typelight",u_char),
	("baselight",u_char),
	("lights2",u_char),
	("lights3",u_char),
	("lightmap",long)
	]),
("lightmaps",f $ u_char),
("clipnodes",f $ TypeCustom [
	("planenum",u_long),
	("front",short),
	("back",short)
	]),
("leaves",f $ TypeCustom [
	("type",long),
	("vislist",long),
	("bound",bboxshort),
	("lface_id",u_short),
	("lface_num",u_short),
	("sndwater",u_char),
	("sndsky",u_char),
	("sndslime",u_char),
	("sndlava",u_char)
	]),
("lface",f $ u_short),
("edges",f $ TypeCustom [
	("vertex0",u_short),
	("vertex1",u_short)
	]),
("ledges",f $ long),
]
) where {
	u_char = TypeInteger 1;
	short = TypeInteger 2;
	u_short = TypeInteger 2;
	u_long = TypeInteger 4;
	long = TypeInteger 4;
	scalar = TypeFloat 4;
	bboxshort = TypeCustom [
		("x low",short),
		("y low",short),
		("z low",short),
		("x high",short),
		("y high",short),
		("z high",short)];
	vec3 = TypeCustom [
		("x",scalar),
		("y",scalar),
		("z",scalar)
	];
	f x= TypeCustom [("offset",TypeInteger 4),("size",TypeInteger 4),("data",TypeBuffer "size" "offset" x)];
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

get_bytes :: (Integral i) => i -> Get [Word8];
get_bytes n = sequence $ replicate (fromIntegral n) getWord8;

make_integer :: [Word8] -> Integer;
make_integer = foldr (\x a -> a*256+x) 0 . map fromIntegral;

make_get :: BL.ByteString -> TypeDefinition -> Get TypeStore;

make_get _ (TypeEmpty) = getRemainingLazyByteString >> return StoreEmpty;
make_get _ (TypeInteger field_length) =
	get_bytes field_length >>= return . StoreInteger . make_integer;
make_get _ (TypeFloat 4) =
	getFloat32le >>= return . StoreFloat;

make_get _ (TypeString Nothing) =
	load_until getWord8 >>= return . StoreString . map (chr . fromIntegral) . takeWhile (/=0);
make_get _ (TypeString (Just field_length)) =
	get_bytes field_length >>= return . StoreString . map (chr . fromIntegral) . takeWhile (/=0);

make_get contents (TypeCustom fields) = do {
	associations <- forM (filter (\(_,field_type) -> is_simple $ field_type) fields) $
	\(field_name,field_type) -> do {
		field_data <- make_get contents field_type;
		return (field_name,field_data);
	};

	pointers <- forM (filter (\(_,field_type) -> is_pointer field_type) fields) $
	\(field_name,TypePointer count_field offset_field field_type) ->
	let {
		Just (StoreInteger count) = lookup count_field associations;
		Just (StoreInteger offset) = lookup offset_field associations;
		out = runGet (sequence $ replicate (fromIntegral count) (make_get contents field_type))
			$ BL.drop (fromIntegral offset) contents;
	} in return (field_name,StorePointer out);

	buffers <- forM (filter (\(_,field_type) -> is_buffer field_type) fields) $
	\(field_name,TypeBuffer size_field offset_field field_type) ->
	let {
		Just (StoreInteger size) = lookup size_field associations;
		Just (StoreInteger offset) = lookup offset_field associations;
		out = runGet (load_until (make_get contents field_type))
			$ BL.take (fromIntegral size)
			$ BL.drop (fromIntegral offset) contents;
	} in return (field_name,StorePointer out);

	return $ StoreCustom $ associations ++ pointers ++ buffers;
} where {
	is_pointer (TypePointer _ _ _) = True;
	is_pointer _ = False;
	is_buffer (TypeBuffer _ _ _) = True;
	is_buffer _ = False;
	is_simple x = not (is_pointer x || is_buffer x);
};
}
