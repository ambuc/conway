import Codec.Picture( PixelRGBA8( .. ), writePng )
import Graphics.Rasterific                 
import Graphics.Rasterific.Linear          
import Graphics.Rasterific.Texture         
import Graphics.Rasterific.Transformations 
import System.Environment

size = 5
white = PixelRGBA8 255 255 255 255
black = PixelRGBA8   0   0   0 255

main = do
  [inPath, outPath] <- getArgs
  grid <- fmap words $ readFile inPath
  let height = length $ grid
  let width  = length $ head $ grid
  let datum = id
          $ map snd
          $ filter ((=='1').fst)
          $ zip (concat $ grid)
                [(x,y) | y <- [0..height-1], x <- [0..width-1]]
  let img = renderDrawing (width*size) (height*size) white 
          $ mapM_ (uncurry pixelAt) datum
  writePng outPath img

pixelAt :: Int -> Int -> Drawing PixelRGBA8 ()
pixelAt x y = withTexture (uniformTexture $ PixelRGBA8 0 0 0 255) 
            $ fill 
            $ rectangle (V2 (fromIntegral $ x*size) (fromIntegral $ y*size))
                        (fromIntegral size) 
                        (fromIntegral size)
