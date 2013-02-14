
module Main (main) where {
import BSPRead;
import Data.Binary.Get (runGet);
import qualified Data.ByteString.Lazy as BL;
import Data.Map (empty);
import Control.Monad.Reader;
import Control.Monad.State;

bsp_definition = TypeCustom (
	("version",long):[
	("entities",make_entry False False $ TypeString Nothing),
	("planes",make_entry False True $ TypeCustom [
		("normal",vec3 $ scalar),
		("dist",scalar),
		("type",long)
	]),
	("miptex",make_entry True False $ TypeCustom [
		("numtex",long),
		("offsets",TypeArray (IntegerField "numtex") (
				TypeCustom [
					("offset",long),
					("texture data",
						TypePointer False True
						(IntegerConst 1)
						(IntegerField "offset")
						(TypeCustom [
							("name",TypeString (Just 16)),
							("width",u_long),
							("height",u_long),
							("offsets",TypeArray (IntegerConst 4) 
							$ TypeCustom [
								("offset",long),
								("image data",TypePointer False False
									(IntegerFunction $
										\vars -> vars 0 "width" * vars 0 "height" `div` 2^(vars 1 "$index"-3))
									(IntegerField "offset")
									u_char
								)
							])
						])
					)
				]
			)
		)
	]),
	("vertices",make_entry False True $ vec3 $ scalar),
	("visilist",make_entry False True $ u_char),
	("nodes", make_entry False True $ TypeCustom [
		("plane_id",long),
		("front",u_short),
		("back",u_short),
		("box",boundbox short),
		("face_id",u_short),
		("face_num",u_short)
		]),
	("texinfo", make_entry False True $ TypeCustom [
		("vectorS",vec3 scalar),
		("distS",scalar),
		("vectorT",vec3 scalar),
		("distT",scalar),
		("texture_id",u_long),
		("animated",u_long)
		]),
	("faces", make_entry False True $ TypeCustom [
		("plane_id",u_short),
		("side",u_short),
		("ledge_id",long),
		("ledge_num",u_short),
		("texinfo_id",u_short),
		("lights",TypeArray (IntegerConst 4) u_char),
		("lightmap",long)
		]),
	("lightmaps",make_entry False True $ u_char),
	("clipnodes",make_entry False True $ TypeCustom [
		("planenum",u_long),
		("front",short),
		("back",short)
		]),
	("leaves",make_entry False True $ TypeCustom [
		("type",long),
		("vislist",long),
		("bound",boundbox short),
		("lface_id",u_short),
		("lface_num",u_short),
		("sounds",TypeArray (IntegerConst 4) u_char)
		]),
	("lface",make_entry False True $ u_short),
	("edges",make_entry False True $ TypeCustom [
		("vertex0",long),
		("vertex1",long)
		]),
	("ledges",make_entry False True $ long),
	("models",make_entry False True $ TypeCustom [
		("bound",boundbox scalar),
		("origin",vec3 scalar),
		("node_id",TypeArray (IntegerConst 4) long),
		("numleafs",long),
		("face_id",long),
		("face_num",long)
		])
	]
) where {
	u_char = TypeInteger 1;
	short = TypeInteger 2;
	u_short = TypeInteger 2;
	u_long = TypeInteger 4;
	long = TypeInteger 4;
	scalar = TypeFloat 4;
	boundbox t = TypeCustom [
		("min",vec3 t),
		("max",vec3 t)];
	vec3 t = TypeCustom [
		("x",t),
		("y",t),
		("z",t)
	];
	make_entry do_frame_shift has_many definition = TypeCustom [
		("offset",long),
		("size",long),
		("data",TypePointer do_frame_shift
			(not has_many) 
			(if has_many then IntegerField "size" else IntegerConst 1)
			(IntegerField "offset") 
			definition
		)
	];
};


main = do {
	contents <- BL.readFile $ "map.bsp";
	--print $ snd $ (!!2) $ fromCustom $ snd $ (!!1) $ fromCustom $
	-- putStr $ (\(StoreCustom (_:(_,StoreCustom (_:_:(_,StorePointer (StoreString x:_)):_)):_)) -> x) 
	print
	$ (\(StoreCustom (_:_:_:(_,StoreCustom x):_)) -> x) 
	$ runGet (make_get "" bsp_definition `runReaderT` contents `evalStateT` ([],empty)) $ contents;
} where {
	fromCustom (StoreCustom x) = x;
};
}
