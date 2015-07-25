Haskell API client library for the Kraken.io image compressor

```Haskell
import Data.ByteString as BS
import Network.Kraken

main :: IO ()
main = do
    let config = Config "<api key>" "<api secret>"
    h <- newHandle config

    img <- BS.readFile "image.png"

    let opt = Options (Just $ Lossy 80) (Just $ Crop 200 200) Nothing
    res <- compressImage h opt img
    case res of
        Left e -> error $ show e
        Right compressedImage -> do
            BS.writeFile "smaller-image.png" compressedImage
```
