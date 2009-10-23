module Network.HTTP.Easy (wget) where

import Control.Exception
import Control.Monad
import Network.HTTP
import Network.URI

wget :: Monad m => String -> [(String, String)] -> [(String, String)] -> IO (m String)
wget uri get [] =
  doWget (uri ++ toQuery get) "" GET

wget uri get post =
  doWget (uri ++ toQuery get) (drop 1 $ toQuery post) POST

toQuery :: [(String, String)] -> String
toQuery ss = g $ concatMap f ss where
  f (k, v) = "&" ++ esc k ++ "=" ++ esc v
  g "" = ""
  g (_:rs) = '?':rs

esc :: String -> String
esc = escapeURIString isAllowedInURI

doWget :: Monad m => String -> String -> RequestMethod -> IO (m String)
doWget uri body method =
  case parseURI uri of
    Nothing -> return $ fail "cannot parse URI"
    Just u -> do
      er <- try $ simpleHTTP (Request u method [] body)
      case er of
        Left e -> return $ fail $ show (e :: IOException)
        Right r -> return $ do
          let Right rsp = r
          when (rspCode rsp /= (2,0,0)) $ fail $ "Response is not 200 OK: " ++ show (rspCode rsp)
          return $ rspBody rsp
