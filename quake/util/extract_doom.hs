
{-# LANGUAGE TemplateHaskell #-}

import Doom;
import Data.Binary.Get (runGet);
import qualified Data.ByteString.Lazy as BL;

main = do {
	contents <- BL.readFile $ "map.bsp";
	print$ runGet (make_get contents bsp_definition) $ contents;
}--print $ make_get 4 TypeInteger;
