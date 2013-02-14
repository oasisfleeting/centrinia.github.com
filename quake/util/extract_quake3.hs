
module Main (main) where {
import BSPRead;
import Data.Binary.Get (runGet);
import qualified Data.ByteString.Lazy as BL;
import Data.Map (empty);
import Control.Monad.Reader;
import Control.Monad.State;

bsp_definition = TypeCustom (
	("magic", TypeString (Just 4)):
	("version",long):[
	("entities",make_entry False False $ TypeString Nothing),
	("textures",make_entry False True $ TypeCustom [
		("name",TypeString (Just 64)),
		("flags",long),
		("contents",long)
	]),
	("planes",make_entry False True $ TypeCustom [
		("normal",vec3 scalar),
		("dist",scalar)
	]),
	("nodes",make_entry False True $ TypeCustom [
		("plane",long),
		("children", TypeArray (IntegerConst 2) long),
		("mins", TypeArray (IntegerConst 3) long),
		("maxs", TypeArray (IntegerConst 3) long)
	]),
	("leafs",make_entry False True $ TypeCustom [
		("cluster",long),
		("area",long),
		("mins", TypeArray (IntegerConst 3) long),
		("maxs", TypeArray (IntegerConst 3) long),
		("leafface",long),
		("n_leaffaces",long),
		("leafbrush",long),
		("n_leafbrushes",long)
	]),
	("leaffaces",make_entry False True $ long),
	("leafbrushes",make_entry False True $ long),
	("models",make_entry False True $ TypeCustom [
		("mins", TypeArray (IntegerConst 3) scalar),
		("maxs", TypeArray (IntegerConst 3) scalar),
		("face",long),
		("n_faces",long),
		("brush",long),
		("n_brushes",long)
	]),
	("brushes",make_entry False True $ TypeCustom [
		("brushside",long),
		("n_brushsides",long),
		("texture",long)
	]),
	("brushsides",make_entry False True $ TypeCustom [
		("plane",long),
		("texture",long)
	])
	{-("vertexes",make_entry False True $ TypeCustom [
		("position",vec3 scalar),
		("surface texcoord",vec2 scalar),
		("lightmap texcoord",vec2 scalar),
		("normal",vec3 scalar),
		("color",vec4 u_char)
	])
-}
]) where {
	u_char = TypeInteger False 1;
	u_short = TypeInteger False 2;
	u_long = TypeInteger False 4;
	short = TypeInteger True 2;
	long = TypeInteger True 4;
	scalar = TypeFloat 4;
	boundbox t = TypeCustom [
		("min",vec3 t),
		("max",vec3 t)];
	vec4 t = TypeCustom [
		("r",t),
		("g",t),
		("b",t),
		("a",t)
	];
	vec2 t = TypeCustom [
		("s",t),
		("t",t)
	];
	vec3 t = TypeCustom [
		("x",t),
		("y",t),
		("z",t)
	];
	make_entry do_frame_shift has_many definition = TypeCustom [
		("offset",u_long),
		("size",u_long),
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
	print
	-- putStr $ (\(StoreCustom (_:(_,StoreCustom (_:_:(_,StorePointer (StoreString x:_)):_)):_)) -> x) 
	$ (\(_,StoreCustom x) -> x) 
	$ last 
	$ (\(StoreCustom x) -> x)
--	$ (\(StoreCustom (_:_:_:_:_:_:(_,StoreCustom x):_)) -> x) 
	$ runGet (make_get "" bsp_definition `runReaderT` ([],contents) `evalStateT` empty) $ contents;
} where {
	fromCustom (StoreCustom x) = x;
};
}
