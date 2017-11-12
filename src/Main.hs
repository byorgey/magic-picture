{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Function (on)
import Data.List (minimumBy, group, sortBy, groupBy, intercalate)
import Data.List.Split
import Data.Char
import Data.Ord (comparing)
import Data.Word
import Control.Arrow (first, second)
import System.Environment (getArgs)
import Control.Monad (forM_)
import qualified Data.Map as M
import System.Random.Shuffle (shuffleM)

import Codec.Picture
import qualified Vision.Image as V
import Vision.Image.JuicyPixels
import Vision.Primitive (ix2)

import qualified Data.Colour as C
import qualified Data.Colour.SRGB as C
import qualified Diagrams.Color.XKCD as X
import Diagrams.Prelude
import Diagrams.Backend.PGF.CmdLine

import qualified Options.Applicative as O

------------------------------------------------------------

data MPOptions = MPOptions
  { inputImg   :: FilePath
  , outputImg  :: Maybe FilePath
  , outputPDF  :: FilePath
  , outputSize :: Int
  , numColors  :: Int
  , oneIndex   :: Bool
  , shuffle    :: Bool
  }

mpOptions :: O.Parser MPOptions
mpOptions = MPOptions
  <$> O.strArgument
        ( O.metavar "FILE"
       <> O.help "Image to be converted to a magic picture puzzle."
        )
  <*> O.optional (O.strOption
        ( O.long "output-img"
       <> O.metavar "FILE"
       <> O.help "File in which to save the converted output as a PNG image."
        ))
  <*> O.strOption
        ( O.long "output"
       <> O.short 'o'
       <> O.metavar "FILE"
       <> O.help "Name of the file in which to save a PDF of the generated magic picture puzzle."
        )
  <*> O.option O.auto
        ( O.long "size"
       <> O.short 's'
       <> O.metavar "INT"
       <> O.value 10
       <> O.help "Size in pixels of the output puzzle (default = 20)."
        )
  <*> O.option O.auto
        ( O.long "colors"
       <> O.short 'c'
       <> O.metavar "INT"
       <> O.value 6
       <> O.help "Number of colors to use in the output puzzle (default = 6)."
        )
  <*> O.switch
        ( O.long "one-index"
       <> O.help "Label grid squares starting from 1 instead of 0 (the default)."
        )
  <*> O.switch
        ( O.long "shuffle"
       <> O.help "Present pixel numbers in random order (default is sorted)."
        )

magicPicture :: O.ParserInfo MPOptions
magicPicture = O.info (mpOptions O.<**> O.helper)
  ( O.fullDesc
  <> O.progDesc "Create a 'magic picture' puzzle from a reference image."
  <> O.header "Magic picture maker!"
  )

-------------------------------------------------------------

main :: IO ()
main = do
  opts <- O.execParser magicPicture

  res <- readImage (inputImg opts)

  case res of
    Left err           -> do
      putStrLn "Unable to load the image:"
      print err
    Right dimg         -> do
      let jpImg = convertRGB8 dimg
          rgb   = toFridayRGB jpImg
          resized = V.resize V.Bilinear (ix2 (outputSize opts) (outputSize opts)) rgb :: V.RGB

      let jpImgSmall = toJuicyRGB resized

          (ixImg, pal) = palettize (PaletteOptions MedianMeanCut False (numColors opts))
                          jpImgSmall

      -- pal has a color at each of (x,0) for x in [0..numColors-1].
      -- ixImg has "pixels" which are just an index into pal.

      -- We can reconstruct the image with quantized colors using pixelMap.
      let pcImg = pixelMap (\w -> pixelAt pal (fromIntegral w) 0) ixImg

      case outputImg opts of
        Nothing  -> return ()
        Just out -> savePngImage out (ImageRGB8 pcImg)

      -- forM_ [0 .. numColors opts - 1] $ \x ->
      --   print (findColorName (pixelAt pal x 0))

      let mp = mkMagicPicture opts pal ixImg
      mp' <- if shuffle opts then shufflePixels mp else return mp

      renderMagicPicture opts (outputPDF opts) mp'

colors :: [(String, C.RGB Word8)]
colors = map (second (C.toSRGB24 . (`C.over` C.black))) (M.assocs X.xkcdColorMap)

findColorName :: C.RGB Word8 -> String
findColorName (C.RGB r g b)
  | distSq (C.RGB 255 255 255) < 25 = "white"
  | otherwise = fst $ minimumBy (comparing (distSq.snd)) colors
  where
    ri, gi, bi :: Int
    ri = fromIntegral r
    gi = fromIntegral g
    bi = fromIntegral b
    distSq (C.RGB r' g' b') = (fromIntegral r' - ri)^2 + (fromIntegral g' - gi)^2 + (fromIntegral b' - bi)^2

------------------------------------------------------------

data ColorList = ColorList
  { colorName   :: String
  , colorSample :: C.RGB Word8
  , pixelList   :: [Int]
  }
  deriving Show

data MagicPicture = MagicPicture
  { pictureSize :: Int
  , colorLists  :: [ColorList]
  }
  deriving Show

mkMagicPicture :: MPOptions -> Palette -> Image Pixel8 -> MagicPicture
mkMagicPicture opts pal ixImg = MagicPicture
  { pictureSize = imageWidth ixImg
  , colorLists  = extractColors
  }
  where
    extractColors = filter ((/="white") . colorName) $ map (uncurry mkColorList) pixelGroups
    pixelGroups :: [(C.RGB Word8, [Int])]
    pixelGroups
      = map (first (\ix -> conv $ pixelAt pal (fromIntegral ix) 0))
      . map (\ps -> (fst . head $ ps, map snd ps))
      . groupBy ((==) `on` fst) . sortBy (comparing fst)
      $ pixels
    pixels :: [(Word8, Int)]
    pixels = zip
      [pixelAt ixImg x y | y <- [0 .. imageHeight ixImg - 1], x <- [0 .. imageWidth ixImg - 1]]
      (if oneIndex opts then [1 ..] else [0 ..])
    mkColorList color pxs = ColorList (findColorName color) color pxs
    conv (PixelRGB8 r g b) = C.RGB r g b

shufflePixels :: MagicPicture -> IO MagicPicture
shufflePixels (MagicPicture sz lists) = do
  lists' <- mapM shuffleColorList lists
  return $ MagicPicture sz lists'
  where
    shuffleColorList (ColorList nm clr pxs) = ColorList nm clr <$> shuffleM pxs

------------------------------------------------------------

letterSize :: SizeSpec V2 Double
letterSize = mkSizeSpec2D (Just 612) (Just 792)

renderMagicPicture :: MPOptions -> FilePath -> MagicPicture -> IO ()
renderMagicPicture opts outFile (MagicPicture sz lists)
  = renderOnlinePGF' outFile (with & standalone .~ True & sizeSpec .~ letterSize) $ do
      plists <- mapM renderPixelList lists
      let plistsDia = vsep 10 plists
          gridSz    = width plistsDia / fromIntegral sz
          grid = mconcat [ vert, horiz, nums ] # lw thin
          vert = replicate (sz + 1) (vrule (width plistsDia) # alignT)
               # hcat' (with & catMethod .~ Distrib & sep .~ gridSz)
          horiz = replicate (sz + 1) (hrule (width plistsDia) # alignL)
               # vcat' (with & catMethod .~ Distrib & sep .~ gridSz)
          nums = [ 0 .. ]
               # take (sz*sz)
               # map (\n -> text (show (if oneIndex opts then succ n else n))
                            # fontSizeO 10
                            # fc grey
                            # translate ( (( fromIntegral (n `mod` sz) + 0.5) * gridSz)
                                       ^& ((-fromIntegral (n `div` sz) - 0.5) * gridSz)
                                        )
                     )
               # mconcat
      return $ vsep 10
        [ plistsDia # centerX
        , grid      # centerX
        ]
        # frame 30

renderPixelList :: ColorList -> OnlineTex (QDiagram PGF V2 Double Any)
renderPixelList pl = do
  let showPxs = intercalate ", " . map show . pixelList $ pl
  cnameHbox <- hboxOnline $ forHuman $ colorName pl
  pxsHbox <- hboxOnline $ "\\parbox{0.5\\linewidth}{\\scriptsize " ++ showPxs ++ "}"
  let d :: QDiagram PGF V2 Double Any
      d =
        [ beside ((-1) ^& 0)
            (square 15 # fc (conv (colorSample pl)) # centerY # frame 5)
            (cnameHbox # centerY)
        , pxsHbox
        ]
        # map centerY
        # hsep 10
  return d

  where
    conv (C.RGB r g b) = sRGB24 r g b
    forHuman = unwords . fixCase . split (startsWithOneOf ['A'..'Z'])
    fixCase (w:ws) = onHead toUpper w : map (map toLower) ws
    onHead f [] = []
    onHead f (x:xs) = f x : xs
