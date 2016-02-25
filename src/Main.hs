{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Network.HTTP.Conduit as HC
import Text.HTML.TagSoup

main = simpleHttp baseURL >>= mapM_ BL.putStrLn . pickTexts . parseTags

baseURL = "http://darekagakaku.herokuapp.com"

pickTexts :: [Tag BL.ByteString] -> [BL.ByteString]
pickTexts [] = []
pickTexts ((TagOpen "p" []):TagText txt:_:rest) = txt:pickTexts rest
pickTexts (_:rest) = pickTexts rest
