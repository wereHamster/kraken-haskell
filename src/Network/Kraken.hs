{-# LANGUAGE OverloadedStrings #-}

module Network.Kraken
    ( Config(..)

    , Handle
    , newHandle

    , Quality(..), Resize(..), Format(..), Convert(..), Options(..)

    , compressImage
    ) where


import Control.Monad.Except
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Aeson hiding (Success, Error)
import Data.Aeson.Types hiding (Success, Error, Options)
import Data.Maybe

import Network.HTTP.Client hiding (Response)
import Network.HTTP.Client.MultipartFormData
import Network.HTTP.Client.TLS (tlsManagerSettings)



data Config = Config
    { cApiKey    :: !String
    , cApiSecret :: !String
    }


data Handle = Handle
    { hConfig  :: Config
    , hManager :: Manager
    }


data Quality = Lossless | Lossy !Int

data Resize
    = Exact !Int !Int
    | Portrait !Int
    | Landscape !Int
    | Auto !Int !Int
    | Fit !Int !Int
    | Crop !Int !Int
    | Square !Int
    | Fill !Int !Int !String

instance ToJSON Resize where
    toJSON (Exact w h) = strategy "exact" [ "width" .= w, "height" .= h ]
    toJSON (Portrait h) = strategy "portrait" [ "height" .= h ]
    toJSON (Landscape w) = strategy "landscape" [ "width" .= w ]
    toJSON (Auto w h) = strategy "auto" [ "width" .= w, "height" .= h ]
    toJSON (Fit w h) = strategy "fit" [ "width" .= w, "height" .= h ]
    toJSON (Crop w h) = strategy "crop" [ "width" .= w, "height" .= h ]
    toJSON (Square s) = strategy "square" [ "size" .= s ]
    toJSON (Fill w h bg) = strategy "fill" [ "width" .= w, "height" .= h, "background" .= bg ]

strategy :: String -> [Pair] -> Value
strategy s r = object $ [ "strategy" .= s ] ++ r

data Format = JPEG | PNG | GIF
instance ToJSON Format where
    toJSON JPEG = String "jpeg"
    toJSON PNG  = String "png"
    toJSON GIF  = String "gif"

data Convert = Convert
    { format :: Format
    , background :: !(Maybe String)
    , keepExtension :: !(Maybe Bool)
    }

data Options = Options
    { quality :: !(Maybe Quality)
    , resize :: !(Maybe Resize)
    , convert :: !(Maybe Convert)
    }

renderOptions :: Options -> [Pair]
renderOptions o = concat $ catMaybes
    [ fmap qualityPairs (quality o)
    , (\r -> ["resize" .= r]) <$> (resize o)
    , fmap convertPairs (convert o)
    ]

  where

    qualityPairs Lossless  = [ "lossy" .= False ]
    qualityPairs (Lossy q) = [ "lossy" .= True, "quality" .= q ]

    convertPairs x =
        [ "convert" .= object (catMaybes
            [ Just $ "format" .= format x
            , ("background" .=) <$> (background x)
            , ("keep_extension" .=) <$> (keepExtension x)
            ])
        ]


-------------------------------------------------------------------------------
-- JSON objects used in the API

data Success = Success
    { krakedUrl :: !String
    } deriving (Show)


data Error = Error !String
    deriving (Show)


responseParser :: Value -> Parser (Either Error Success)
responseParser (Object o) = o .: "success" >>= \success ->
    if success
        then Right . Success <$> o .: "kraked_url"
        else Left . Error <$> o .: "error"
responseParser _ = fail "Kraken API Response"


newHandle :: Config -> IO Handle
newHandle c = do
    manager <- newManager tlsManagerSettings
    return $ Handle c manager


compressImage :: Handle -> Options -> ByteString -> IO (Either Error ByteString)
compressImage h opt img = do
    let Just req0 = parseUrl "https://api.kraken.io/v1/upload"
    req <- insertRequestBody req0
    res <- httpLbs (req { checkStatus = \_ _ _ -> Nothing }) (hManager h)

    runExceptT $ do
        v <- case eitherDecode' (responseBody res) of
            Left e -> throwError $ Error e
            Right x -> return x

        r <- case parseEither responseParser v of
            Left e -> throwError $ Error e
            Right x -> ExceptT $ return x

        ExceptT $ downloadBinary (hManager h) (krakedUrl r)

  where
    config = hConfig h

    options = object $
        [ "auth" .= object
            [ "api_key"    .= cApiKey config
            , "api_secret" .= cApiSecret config
            ]
        , "wait" .= True
        ] ++ renderOptions opt

    insertRequestBody :: Request -> IO Request
    insertRequestBody = formDataBody
        [ partLBS "options" $ encode options
        , (partBS "file" img) { partFilename = Just "kraken" }
        ]

downloadBinary :: Manager -> String -> IO (Either Error ByteString)
downloadBinary m url = runExceptT $ do
    req <- ExceptT $ return $ case parseUrl url of
        Nothing -> throwError $ Error $ "Failed to parse download URL: " ++ url
        Just x -> return x

    res <- ExceptT $ Right <$> httpLbs req m
    ExceptT $ return $ Right $ toStrict $ responseBody res
