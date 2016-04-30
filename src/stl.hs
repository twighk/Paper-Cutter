import Control.Arrow
import Control.Monad
import qualified Data.ByteString as BS
import Data.List
import Data.Maybe
import Data.Serialize
import Graphics.Formats.STL
import Prelude

xslices :: Int
xslices = 7
yslices :: Int
yslices = 7
ysize :: Float -- mm
ysize = 55
xsize :: Float -- mm
xsize = 55
zsize :: Float -- mm
zsize = 55

pagewidth :: Float
pagewidth  = 297 --mm
pageheight :: Float
pageheight = 210 --mm

file :: String
file = "Tetrahedron.stl"

main :: IO()
main = do
    file <- BS.readFile file

    case runGet (get :: Get STL) file of
        Left err -> do
            putStrLn "Encountered error reading."
            putStrLn err
        Right stl -> do
            drawStl stl
            return ()
    putStrLn "Done"

drawStl :: STL -> IO String
drawStl stl = do
    let ts = triangles stl
    let (xs,ys,zs) = unzip3 . concatMap triangleToPoints $ ts
    let (minx, maxx) = minimum &&& maximum $ xs
    let (miny, maxy) = minimum &&& maximum $ ys
    let (minz, maxz) = minimum &&& maximum $ zs

    let tris = map (scale minx maxx miny maxy minz maxz . vertices) ts
   
    let xcuts = init . tail $ linspacelist 0 xsize (xslices+2)
    let ycuts = init . tail $ linspacelist 0 ysize (yslices+2)
    
    let planexs = map (\x->Plane{pnormal = (1,0,0), ppoint = (x,0,0)}) xcuts
    let planeys = map (\y->Plane{pnormal = (0,1,0), ppoint = (0,y,0)}) ycuts
   
    let tripoints =  tupleApply (map (map fromJust . filter isJust . (map crossPlane tris <*>)) . tacnoc) (planexs, planeys)

    let (linesx, linesy) = tuplezip (tupleApply (zipWith map . map lineOnPlane) (planexs, planeys))  tripoints
    let drawx = map ((\x -> (x,xsize,zsize)) . drawPlaneLines cutx xcuts . map (tupleApply rmx)) linesx 
    let drawy = map ((\y -> (y,ysize,zsize)) . drawPlaneLines cuty ycuts . map (tupleApply rmy)) linesy 
    
    -- Convert graphs into pages of svgs 
    let svgs = map (
         \(a,b,c)-> unlines 
            [ svgheader 
            , "<g transform=\"translate("++ show ((pagewidth-b)/2)++","++ show ((pageheight-c)/2)++")\">" 
            , a 
            , "</g>" 
            , svgfooter
            ]
         ) $ spacegraphs (drawx ++ drawy)
    
    -- Save pages by number
    zipWithM_ (\f svg -> writeFile (file ++ "." ++ show f ++ ".svg") svg) [0..] $ reverse svgs

    return "Done"

scale :: Float ->  Float -> Float -> Float -> Float -> Float -> (Vector, Vector, Vector) -> (Vector, Vector, Vector)
scale xi xa yi ya zi za (a,b,c)  = (s a, s b, s c)
    where
        s (i,j,k) = (sx i , sy j, sz k)
        sx p = xsize * (p-xi)/(xa-xi)
        sy p = ysize * (p-yi)/(ya-yi)
        sz p = zsize * (p-zi)/(za-zi)

rmx (_,b,c) = (b,c)
rmy (a,_,c) = (a,c)

tuplezip :: (a -> a', b -> b') -> (a,b) -> (a',b')
tuplezip (fa,fb) (a,b) = (fa a, fb b)

spacegraphs :: [(String, Float, Float)] -> [(String, Float, Float)]
spacegraphs = joiny . joinx 
    where
        joinx :: [(String, Float, Float)] -> [(String, Float, Float)]
        joinx = foldl 
                    (\((i,j,k):ls) (a,b,c) -> 
                        if j+b < pagewidth
                            then (  "<g transform=\"translate("++show j++",0)\"\n>"
                                ++ a
                                ++ "\n</g>\n"
                                ++ i
                                , j+b
                                , max c k
                                ):ls
                            else (a,b,c):(i,j,k):ls
                    )
                    [("",0.0,0.0)]

        joiny :: [(String, Float, Float)] -> [(String, Float, Float)]
        joiny = foldl 
                    (\((i,j,k):ls) (a,b,c) ->
                        if c+k < pageheight
                            then (  "<g transform=\"translate(0,"++show k++")\">\n"
                                ++ a
                                ++ "\n</g>\n"
                                ++ i
                                , max j b
                                , c+k
                                ):ls
                            else (a,b,c):(i,j,k):ls
                    )
                    [("",0.0,0.0)]

drawPlaneLines :: (Float -> [((Float, Float),(Float, Float))] -> String) ->[Float] -> [((Float, Float),(Float, Float))] -> String
drawPlaneLines cuttype cutlines lines
    =  "<g>"
    ++ concatMap (\((x1,y1),(x2,y2))
        -> "<line x1=\""++show x1++"\" y1=\""++show y1++"\" x2=\""++show x2++"\" y2=\""++show y2++"\" "
        ++ style
        ++ "/>\n"
        ) lines
    ++ concatMap (`cuttype` lines) cutlines
    ++ "</g>"

cutx :: Float -> [((Float, Float),(Float, Float))] -> String
cutx = cut (\(a,b) -> (a, a + (b-a)/2))   
cuty :: Float -> [((Float, Float),(Float, Float))] -> String
cuty = cut (\(a,b) -> (a + (b-a)/2, b))   

cut :: ((Float, Float) -> (Float,Float)) ->Float -> [((Float, Float),(Float, Float))] -> String
cut cutxy x lines = concatMap (\ (y1,y2)
        -> "<line x1=\""++show x++"\" y1=\""++show y1++"\" x2=\""++show x++"\" y2=\""++show y2++"\" "
        ++ style
        ++ "/>\n"
        ) points
    where
        points :: [(Float,Float)]
        points = map cutxy . pair . nub . sort . map crossingPoint . filter crossLine $ lines
        
        crossingPoint :: ((Float, Float),(Float, Float)) -> Float
        crossingPoint ((x1,y1),(x2,y2))= y1 + (x-x1)*(y2-y1)/(x2-x1)  

        crossLine :: ((Float, Float),(Float, Float)) -> Bool
        crossLine ((x1,_),(x2,_))
            | x1 <= x && x <= x2 = True
            | x1 >= x && x >= x2 = True
            | otherwise = False

pair :: [a] -> [(a,a)]
pair [] = []
pair (a:b:xs) = (a,b):pair xs

lineOnPlane :: Plane -> (Vector, (Vector,Vector)) -> (Vector, Vector)
lineOnPlane t (b,(a,c)) = tupleApply f (a,c) 
    where 
        f x = ((((pp `minus` b) `dot` pn)/((x `minus` b) `dot` pn)) `mult` (x `minus` b)) `plus` b
        pn = pnormal t
        pp = ppoint  t

tupleApply :: (a->b) -> (a,a) -> (b,b)
tupleApply x = first x . second x

crossPlane :: (Vector,Vector,Vector)-> Plane -> Maybe (Vector, (Vector,Vector))
crossPlane tri plane  
    | sign ta == sign tb && sign tb == sign tc = Nothing
    | sign ta == sign tb = Just (tc,(ta,tb))
    | sign ta == sign tc = Just (tb, (ta,tc))
    | otherwise = Just (ta,(tb,tc))
    where 
        pn = pnormal plane
        pp = ppoint  plane
        (ta,tb,tc) = tri
        sign tx = signum (pn `dot` (pp `minus` tx))

data Plane = Plane 
    { pnormal :: Vector
    , ppoint  :: Vector
    } deriving Show

dot :: Vector -> Vector -> Float
dot (a,b,c) (i,j,k) = a*i + b*j + c*k

plus :: Vector -> Vector -> Vector
plus (a,b,c) (i,j,k) = (a+i,b+j,c+k)

minus :: Vector -> Vector -> Vector
minus (a,b,c) (i,j,k) = (a-i, b-j, c-k)

mult :: Float -> Vector -> Vector
mult x(a,b,c) = (x*a, x*b, x*c)

triangleToPoints :: Triangle -> [Vector]
triangleToPoints t = [a,b,c]
    where (a,b,c) = vertices t

linspacelist :: Float -> Float -> Int -> [Float]
linspacelist min max n = map (\x -> min + (max-min) * fromIntegral x / nf) [0..n-1]
    where nf = fromIntegral (n-1)

tacnoc :: [a] -> [[a]]
tacnoc = map (: [])

svgheader :: String
svgheader = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n"
         ++ "<svg "
         ++ "width=\""++ show pagewidth ++"mm\" "
         ++ "height=\""++ show pageheight ++"mm\" "
         ++ "viewBox=\"0 0 "++ show pagewidth ++" "++ show pageheight ++"\" "
         ++ ">"

svgfooter :: String
svgfooter = "</svg>"

style :: String
style = "style=\"fill:none;stroke:#ff0000;stroke-width:0.1\""
