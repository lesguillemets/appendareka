{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Control.Monad
import Data.Monoid
import Data.Maybe
import Data.List (uncons)
import System.Environment
import Network.HTTP.Conduit as HC
import Network.HTTP.Types.Status
import Text.HTML.TagSoup

main = do
    s <- fromMaybe "" . liftM (BL.pack . fst) . uncons <$> getArgs
    BL.getContents >>= appendWrite s

baseURL = "http://darekagakaku.herokuapp.com/"

pickTexts :: [Tag BL.ByteString] -> [BL.ByteString]
pickTexts [] = []
pickTexts ((TagOpen "p" []):TagText txt:_:rest) = txt:pickTexts rest
pickTexts (_:rest) = pickTexts rest

getCurrentTexts :: IO [BL.ByteString]
getCurrentTexts = pickTexts . parseTags <$> simpleHttp baseURL

appendWrite :: BL.ByteString -> BL.ByteString -> IO ()
appendWrite separator text = do
    current <- getCurrentTexts
    writeText . BL.unlines $ current ++ [separator , text]

writeText :: BL.ByteString -> IO ()
writeText text = do
    res <- httpLbs req =<< newManager tlsManagerSettings
    let sCode = statusCode . responseStatus $ res
        sMes = statusMessage . responseStatus $ res
    unless (responseStatus res == ok200) $ do
        print . ("Status : " <>) . show $ sCode
        B.putStrLn ("\t" <> sMes)
        mapM_ print $ responseHeaders res
        BL.putStrLn . responseBody $ res
    where
        req = [("text", BL.toStrict text)] `urlEncodedBody` baseRequest

baseRequest = (\c -> c
              {
                  method = "POST",
                  requestHeaders = [("user-agent", "haskell-conduit")]
              }) . fromJust . HC.parseUrl $ baseURL <> "w"
