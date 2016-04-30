import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.List
import Prelude

pointsperslice :: Int
pointsperslice = 10
xslices :: Int
xslices = 7
yslices :: Int
yslices = 7

xmin :: Double
xmin = -10
xmax :: Double
xmax =  10
ymin :: Double
ymin = -10
ymax :: Double
ymax =  10

ysize :: Double -- mm
ysize = 55
xsize :: Double	-- mm
xsize = 55
zsize :: Double -- mm
zsize = 55
zoffset :: Double -- mm
zoffset = 5

pagewidth :: Double
pagewidth  = 297 --mm
pageheight :: Double
pageheight = 210 --mm

function :: (Double,Double) -> Double
function (x,y)
	| sqrt (x^^2 + y^^2) == 0 = 1
	| otherwise = sin (sqrt(x^^2 + y^^2)) / sqrt (x^^2 + y^^2)

main :: IO()
main = do
	-- Find maximum and minium z
	let xpoints = linspacelist xmin xmax ((xslices+1)*pointsperslice+1)
	let ypoints = linspacelist ymin ymax ((yslices+1)*pointsperslice+1)
	let points  = liftM2 (,) xpoints ypoints	 
	let (zmin,zmax) = (minimum &&& maximum) . map function $ points
	-- Get Slices' x's and y's
	let xs = tail . init $ linspacelist xmin xmax (2+xslices)	
	let ys = tail . init $ linspacelist ymin ymax (2+yslices)

	-- Get graphs and attach dimentions
	let xgraphs  
		= zip3 
			(map (\a -> drawfunction (\x->function (a,x)) xmin xmax xsize zmin zmax zsize zoffset cutdown ypoints) xs) 
			(repeat xsize) 
			(repeat (zsize + zoffset))

	let ygraphs
		= zip3
			(map (\a -> drawfunction (\x->function (x,a)) ymin ymax ysize zmin zmax zsize zoffset cutup   xpoints) ys)
			(repeat ysize)
			(repeat (zsize + zoffset))
	
	-- Convert graphs into pages of svgs 
	let svgs = map 
		(\(a,b,c)-> unlines  
			[ svgheader
			, "<g transform=\"translate("++ show ((pagewidth-b)/2)++","++ show ((pageheight-c)/2)++")\">"
			, a
			, "</g>"
			, svgfooter
			]
		)
		$ spacegraphs (xgraphs++ygraphs)

	-- Save pages by number
	zipWithM_ (\file svg -> writeFile (show file ++ ".svg") svg) [0..] $ reverse svgs

	putStrLn $ "Saved "++ (show . length $ svgs) ++ " pages"

spacegraphs :: [(String, Double, Double)] -> [(String, Double, Double)]
spacegraphs = joiny . joinx 
	where
		joinx :: [(String, Double, Double)] -> [(String, Double, Double)]
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

		joiny :: [(String, Double, Double)] -> [(String, Double, Double)]
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


drawfunction 
	:: (Double -> Double) 
	-> Double -> Double -> Double 
	-> Double -> Double -> Double
	-> Double
	-> (Double -> (Double,Double) -> String) 
	-> [Double]
	-> String
drawfunction f xmin xmax xscale zmin zmax zscale zoff cut xs 
	=  "<g>"
	++ "<polygon points=\""
	++ show (fst . head $ points) ++ ",0 "
	++ concatMap (\(a,b) -> show a ++ "," ++ show (zoff + b) ++ " ") points
	++ show (fst . last $ points) ++ ",0 "
	++ "\" "
	++ style
	++ "/>\n"
	++ concatMap (cut zoff) cuts
	++ "</g>"
	where
		points = zip 
					(map (*xscale) $ scalepoints xmin xmax xs)
					(map (*zscale) $ scalepoints zmin zmax (map f xs))
		every n [] = []
		every n xs = take 1 (drop n xs) ++ every n (drop n xs)
		cuts = every pointsperslice points

cutup :: Double -> (Double,Double) -> String
cutup zoff (a,b) 
	= "<polyline points=\""
	++ show a ++ ",0 "
	++ show a ++ "," ++ show ((zoff + b) /2.0)  ++ " "
	++ "\" "
	++ style
	++ "/>\n"

cutdown :: Double -> (Double,Double) -> String
cutdown zoff (a,b) 
	= "<polyline points=\""
	++ show a ++ "," ++ show (zoff + b) ++ " "
	++ show a ++ "," ++ show ((zoff + b)/2.0)  ++ " "
	++ "\" "
	++ style
	++ "/>\n"


scalepoints :: Double -> Double -> [Double] -> [Double] -- scale points [0,1]
scalepoints min max = map (\z -> (z-min)/(max-min)) 

linspacelist :: Double -> Double -> Int -> [Double]
linspacelist min max n = map (\x -> min + (max-min) * fromIntegral x / nf) [0..n-1]
	where nf = fromIntegral (n-1)


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
