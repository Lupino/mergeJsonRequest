{-# LANGUAGE OverloadedStrings #-}
module GraphQL
  (
    schema
  ) where

import           Control.Lens          ((&), (.~))
import           Data.CaseInsensitive  (mk)
import           Data.GraphQL.AST.Core (ObjectField (..), Value (ValueString))
import           Data.GraphQL.Schema   (Resolver, Schema, objectA', scalar)
import           Data.List.NonEmpty    (NonEmpty ((:|)))
import           Data.Maybe            (fromMaybe)
import           Data.Text             (toUpper, unpack)
import           Data.Text.Encoding    (encodeUtf8)

import           Yuntan.Utils.GraphQL  (getObjectValue, getTextValue, value')

import qualified Data.Aeson            as A (Value)
import           Network.HTTP.Client   (Manager)
import           Network.Wreq          (Options, customPayloadMethodWith,
                                        defaults, header, manager)
import           Yuntan.Types.Result   (ErrResult (errMsg))
import           Yuntan.Utils.Wreq     (responseEitherJSON)

-- type Query {
--   request(uri: String!, method: String, headers: Object, body: String): Value
-- }

schema :: String -> Manager -> Schema IO
schema root mgr = request root mgr :| []

request :: String -> Manager -> Resolver IO
request root mgr = objectA' "request" $ \argv -> do
  let uri = (root ++) . fix . unpack . fromMaybe "" $ getTextValue "uri" argv
      method = unpack . toUpper . fromMaybe "GET" $ getTextValue "method" argv
      headers = fromMaybe [] $ getObjectValue "headers" argv
      body = encodeUtf8 . fromMaybe "" $ getTextValue "body" argv
      opts = mergeHeaders (defaults & manager .~ Right mgr) headers

  toGraphQL <$> responseEitherJSON (customPayloadMethodWith method opts uri body)

fix :: String -> String
fix ('/':xs) = '/' : xs
fix xs       = '/' : xs


toGraphQL :: Either ErrResult A.Value -> [Resolver IO]
toGraphQL (Left e)  = [scalar "err" (errMsg e)]
toGraphQL (Right v) = value' v


mergeHeaders :: Options -> [ObjectField] -> Options
mergeHeaders = foldl mergeHeader

mergeHeader :: Options -> ObjectField -> Options
mergeHeader opts (ObjectField k (ValueString v)) = opts
                                                 & header (mk $ encodeUtf8 k)
                                                 .~ [encodeUtf8 v]
mergeHeader opts _ = opts
