

module BSPRead where {

import qualified Data.ByteString.Lazy as BL;
import qualified Data.Map as M;
import Data.List (tails);
import Data.Map ((!));
import Data.Binary (Get,getWord8);
import Data.Binary.Get (runGet,isEmpty,getRemainingLazyByteString,bytesRead);
import Data.Binary.IEEE754 (getFloat32le);
import Data.Char (chr);
import Data.Word (Word8);
import Data.Function (on);
import Control.Monad (forM,replicateM,liftM);
import Control.Monad.Fix (mfix,MonadFix);
import Control.Monad.State;
import Control.Monad.Reader;

data IntegerReference
 = IntegerConst Integer
 | IntegerField String
 | IntegerFunction ((Integer -> String -> Integer) -> Integer);


data TypeDefinition
 = TypeInteger Integer
 | TypeNamespace
 | TypeIntegerLiteral IntegerReference
 | TypeFloat Integer
 | TypeString (Maybe Integer)
 | TypeArray IntegerReference TypeDefinition
 | TypePointer Bool Bool IntegerReference IntegerReference TypeDefinition -- frame shift; size/count; offset; definition
 | TypeCustom [(String,TypeDefinition)]
 | TypeEmpty
 ;
data TypeStore
 = StoreInteger Integer
 | StoreFloat Float
 | StoreString String
 | StoreArray [TypeStore]
 | StorePointer [TypeStore]
 | StoreCustom [(String,TypeStore)]
 | StoreEmpty
 deriving (Show,Eq);

load_until :: Get a -> Get [a];
load_until f =
do {
	empty <- isEmpty;
	if empty then return []
	else do {
		v <- f;
		rest <- load_until f;
		return $ v:rest;
	};
};

get_bytes :: Integer -> Get [Word8];
get_bytes n = fromIntegral n `replicateM` getWord8;

make_integer :: [Word8] -> Integer;
make_integer = foldr (\x a -> a*256+x) 0 . map fromIntegral;

lookup_integer_var
	:: M.Map [String] TypeStore
	-> [String]
	-> IntegerReference
	-> Integer;

lookup_integer_var _ _ (IntegerConst val) = val;
lookup_integer_var vars namespace (IntegerField varname) =
let {
	Just (StoreInteger val) =
		head
		$ dropWhile (==Nothing)
		$ [(varname:suffix) `M.lookup` vars | suffix <- tails namespace]

	--vars ! (varname:namespace);

	--StoreInteger val = vars ! (varname:namespace);
} in val;
lookup_integer_var vars namespace (IntegerFunction f) =
let {
	varlookup level varname = let {
		Just (StoreInteger val) =
			head
			$ dropWhile (==Nothing)
			$ [(varname:suffix) `M.lookup` vars
			| suffix <- drop (fromIntegral level) $ tails namespace];
	} in val;
} in f varlookup;

type Varmap = M.Map [String] TypeStore;

make_get
	:: String
	-> TypeDefinition
	-> ReaderT BL.ByteString (StateT ([String],Varmap) Get) TypeStore;

make_get _ (TypeEmpty) =
	lift $ (const StoreEmpty) `liftM` lift getRemainingLazyByteString;
	--((const StoreEmpty) `liftM` getRemainingLazyByteString);

make_get _ (TypeInteger field_length) = 
	lift $ (StoreInteger . make_integer) `liftM` (lift $ get_bytes field_length);
make_get _ (TypeFloat 4) =
	lift $ StoreFloat `liftM` lift getFloat32le;

make_get _ (TypeIntegerLiteral x) = do {
	(namespace,vars) <- get;
	return $ StoreInteger $ lookup_integer_var vars namespace x;
};
make_get _ (TypeNamespace) = do {
	(namespace,_) <- get;
	return $ StoreArray $ map StoreString $ namespace;
};

make_get _ (TypeString field) =
	lift $
	(StoreString . map (chr . fromIntegral) . takeWhile (/=0)) `liftM`
	lift (case field of {
		Just field_length -> get_bytes field_length;
		Nothing -> load_until getWord8;
	});

make_get field_name (TypeArray n field_type) =
do {
	contents <- ask;
	(namespace,vars) <- get;
	put $ (namespace,update_index (field_name:namespace) (const 0) vars);
	array <-
		((fromIntegral $ lookup_integer_var vars (namespace) n)
		`replicateM` do {
		(namespace',vars) <- get;
		result <- make_get field_name field_type;
		put $ (namespace',update_index namespace' (+1) vars);
		return result;
		});
	put (namespace,vars);

	return $ StoreArray array;
};

make_get field_name (TypePointer do_frame_shift is_count number_field offset_field field_type) =
do {
	contents <- ask;
	(namespace,vars) <- get;
	let {
		number = lookup_integer_var vars namespace number_field;
		offset = lookup_integer_var vars namespace offset_field;
		getter = make_get field_name field_type `runReaderT` (BL.drop (fromIntegral offset) contents) `evalStateT` (namespace,vars);
		out =
			(if is_count
				then runGet (fromIntegral number `replicateM` getter)
				else runGet (load_until getter) . BL.take (fromIntegral number)
			) . BL.drop (fromIntegral $ offset);
		field_data =
			if 0<=number 
				then StorePointer (out contents)
				else StoreEmpty;
	};
	return field_data;
};

make_get field_name (TypeCustom fields) = do {
	contents <- ask;
	(namespace,vars) <- get;
	put (field_name:namespace,update_index (field_name:namespace) (const 0) vars);
	associations <- forM fields $ \(field_name,field_type) -> 
		do {
			(namespace',vars) <- get;
			field_data <- make_get field_name field_type;
			put (namespace',
				M.insert (field_name:namespace') field_data
				$ update_index namespace' (+1) vars);
			return (field_name,field_data);
		};
	put (namespace,vars);
	return $ StoreCustom $ associations;
};

update_index :: [String] -> (Integer -> Integer) -> Varmap -> Varmap;
update_index namespace f m =
let
{
	g Nothing = Just (StoreInteger $ f 0);
	g (Just (StoreInteger v)) = Just (StoreInteger $ f v);
} in M.alter g ("$index":namespace) m;

}
