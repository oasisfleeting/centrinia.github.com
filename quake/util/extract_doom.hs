
module Main (main) where {
import BSPRead;
import Data.Binary.Get (runGet);
import qualified Data.ByteString.Lazy as BL;
import Data.Map (empty);
import Control.Monad.Reader;
import Control.Monad.State;

wad_definition = TypeCustom [
("identification",TypeString $ Just 4),
("numlumps",TypeInteger 4),
("infotableofs",TypeInteger 4),
("lumps",TypePointer False True (IntegerField "numlumps") (IntegerField "infotableofs") $
	TypeCustom [
		("filepos",TypeInteger 4),
		("size",TypeInteger 4),
		("name",TypeString $ Just 8)
	]
)];

main = do {
	contents <- BL.readFile $ "doom.wad";
	--print $ snd $ (!!2) $ fromCustom $ snd $ (!!1) $ fromCustom $
	--print
	mapM putStrLn
	$ map (\(StoreCustom (_:_:(_,StoreString x):_)) -> x)
	$ (\(StoreCustom (_:_:_:(_,StorePointer x):_)) -> x) 
	$ runGet (make_get "" wad_definition `runReaderT` ([],contents) `evalStateT` empty) $ contents;
} where {
	fromCustom (StoreCustom x) = x;
};
}
