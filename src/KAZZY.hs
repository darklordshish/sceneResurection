module KAZZY
    ( Conductivity(..)
    , getPixel
    , getImage
    , baseImage
--    , numOctaves
--    , conductivity
--    , conductImage
--    , diffusionOperator
--    , sigmas
--    , timesScales
--    , semiImplictNonlinearDifusion
--    , eyeImage
--    , threeDiagonalImageToLists
--    , computeThomas
--    , unflatten
    , nonlinearDiffusion
    ) where

import Prelude as P
import Graphics.Image as I
import Graphics.Image.Interface
import Graphics.Image.Processing.Filter (Direction
                                        ,applyFilter
                                        ,gaussianLowPass
                                        ,gaussianBlur
                                        ,sobelFilter
                                        ,sobelOperator
                                        ,logFilter
                                        )
import System.Environment( getArgs )
import GHC.Real (fromIntegral)
import Foreign (Int)
import GHC.Err (undefined)
import Data.List.NonEmpty (length)
import Text.Read.Lex (numberToInteger)



data Conductivity = HighContrastEdges | WideRegions | SelectiveSmoothing deriving(Show,Eq)


getPixel :: (Int, Int) -> Pixel RGB Word8
getPixel (i, j) = PixelRGB (fromIntegral i) (fromIntegral j) (fromIntegral (i + j))

getImage :: (Int, Int) -> Image VS RGB Word8
getImage (w, h) = makeImageR VS (w, h) getPixel

baseImage:: Image VS RGB Double -- image
  -> Double -- sigma 
  -> Double -- assumedBlur
  -> Image VS RGB Double
baseImage img sigma assumedBlur = applyFilter filter resizedImg
  where (width,height) = dims img
        sigmaDiff = sqrt $ max ( sigma ^ 2 - (2 * assumedBlur) ^ 2)  0.01
        resizedImg = resize Bilinear Edge (2*width,2*height) img
        filter = gaussianBlur sigma

thomas :: Fractional g => [g] -> [g] -> [g] -> [g] -> [g]
thomas as bs cs ds = xs
  where
    n = P.length bs
    bs' = b(0) : [b(i) - a(i)/b'(i-1) * c(i-1) | i <- [1..n-1]]
    ds' = d(0) : [d(i) - a(i)/b'(i-1) * d'(i-1) | i <- [1..n-1]]
    xs = P.reverse $ d'(n-1) / b'(n-1) : [(d'(i) - c(i) * x(i+1)) / b'(i) | i <- [n-2, n-3..0]]
    -- convenience accessors (because otherwise it's hard to read)
    a i = as !! (i-1) --  because the list's first item is equivalent to a_1
    b i = bs !! i
    c i = cs !! i
    d i = ds !! i
    x i = xs !! i
    b' i = bs' !! i
    d' i = ds' !! i

numOctaves :: (Int,Int) -> Int
numOctaves (width,height) = round $ logBase 2 (fromIntegral (min width height)) - 1

conductivity :: (Floating a, Eq a) => Conductivity -- type of isotropic diffusion
  -> a -- scale factor
  -> a -- gradient
  -> a
conductivity HighContrastEdges k l = exp (- l^2 / k^2 )
conductivity WideRegions k l = 1 / ( 1 + l^2 / k^2 )
conductivity SelectiveSmoothing k l
  | l^2 == 0 = 1
  | otherwise =  1 - exp ( - 3.315/( abs l / k )^8)


conductImage :: Conductivity
  -> Double -- scale factor
  -> Double -- sigma
  -> Image VS RGB Double
  -> Image VS RGB Double
conductImage cond k sg img  = resultImg
  where resultImg = I.map condFuncion smoothImg
        condFuncion = liftPx $ conductivity cond k
        smoothImg = sobelOperator $ applyFilter blureFilter img
        blureFilter = gaussianLowPass (ceiling (2*sg)) sg Reflect

diffusionOperator :: Image VS RGB Double -- conductImage 
  -> Image VS RGB Double -- ImageOperator
diffusionOperator  img = resultImg
    where (w,h) = dims img
          flattedImg = toLists img
          indexes = [(i,j)| i<-[0..w], j<-[0..h]]
          indexesFlat = P.map (fromIx w) indexes
          aH (i,j)= aHorizontal img (toIx w i) (toIx w j)
          aV (i,j)= aVertical img (toIx w i) (toIx w j)
          aHorImg = makeImage (w*h,w*h) aH
          aVerImg = makeImage (w*h,w*h) aV
          resultImg = aHorImg + aVerImg

aHorizontal :: Image VS RGB Double -- conductImage 
  -> (Int,Int) -- i
  -> (Int,Int) -- j 
  -> Pixel RGB Double -- pixel result
aHorizontal img (i,j) (l,k)
  | i==l && j==k      = liftPx  ((-1/18)*) $ previj + lk + ij + lk + succij + lk
  | (l+1)==i && j==k  = liftPx (1/18*) $ previj + ij
  | (l-1)==i && j==k  = liftPx (1/18*) $ succij + ij
  | otherwise            = liftPx (0*) ij
  where (w,h) = dims img
        ij = index img (i,j)
        lk = index img (l,k)
        succij = index img ( max (i+1) w, j)
        previj = index img ( min (i-1) 0, j)

aVertical :: Image VS RGB Double -- conductImage 
  -> (Int,Int) -- i
  -> (Int,Int) -- j 
  -> Pixel RGB Double -- pixel result
aVertical img (i,j) (l,k)
  | i==l && j==k      = liftPx  ((-1/18)*) $ previj + lk + ij + lk + succij + lk
  | l==i && (j+1)==k  = liftPx (1/18*) $ previj + ij
  | l==i && (j-1)==k  = liftPx (1/18*) $ succij + ij
  | otherwise            = liftPx (0*) ij
  where (w,h) = dims img
        ij = index img (i,j)
        lk = index img (l,k)
        succij = index img ( i, max (j+1) h)
        previj = index img ( i, min (j-1) 0)

sigmas:: Double -- sigmas
  -> Int --octaves
  -> Int -- intervals
  -> [Double]
sigmas sg o s = [sg*2**(fromIntegral i + fromIntegral j / fromIntegral s)| i <- [0..o-1], j <- [0..s-1] ]

timesScales:: [Double] -- sigmas
  -> [Double] -- times
timesScales = P.map (\ x -> 0.5*x^2)

semiImplictNonlinearDifusion :: Conductivity
  -> Double -- scale factor
  -> Double -- sigma
  -> Double -- time interval
  -> Image VS RGB Double -- image 
  -> Image VS RGB Double
semiImplictNonlinearDifusion cond k sigma tau img = equationMatrix
  where (w,h) = dims img
        condImg = conductImage cond k sigma img
        eye = eyeImage (w*h,w*h)
        aOp = diffusionOperator condImg
        equationMatrix = eye - I.map (liftPx (tau *)) aOp

eyeImage :: (Int,Int) -> Image VS RGB Double
eyeImage (w,h) = makeImage (w,h) eyeij
  where eyeij (i,j)
          | i==j = promote 1
          | otherwise = promote 0

threeDiagonalImageToLists :: Image VS RGB Double
  -> ([Pixel RGB Double],[Pixel RGB Double],[Pixel RGB Double])
threeDiagonalImageToLists img = (top,diag,bottom)
  where (w,h) = dims img
        top = [indexImg (i,j) | i<-[1..w-1],j<-[0..h-2], i==(j+1) ]
        diag = [indexImg (i,j) | i<-[0..w-1],j<-[0..h-1], i==j ]
        bottom = [indexImg (i,j) | i<-[0..w-2],j<-[1..h-1], i==(j-1) ]
        indexImg = index img

computeThomas ::  Image VS RGB Double -- оператор диффузии
  -> Image VS RGB Double -- картинка предыдущего шага
  -> Image VS RGB Double -- картинка нового шага 
computeThomas matrixA img = imgResult
  where (w,h) = dims img
        (ts,ds,bs) = threeDiagonalImageToLists matrixA
        imgFlattenVector = (fromVector (1,w*h) $ toVector img ):: Image VS RGB Double
        imgFlatten = concat $ toLists imgFlattenVector
        imgResultFlatten = thomas ts ds bs imgFlatten
        imgResult = fromLists $ unflatten (w,h) imgResultFlatten

computeNextImage :: Conductivity
  -> Double --scale factor
  -> Double -- sigma 
  -> Double -- time's interval
  -> Image VS RGB Double -- image t
  -> Image VS RGB Double -- image t+1
computeNextImage cond k sigma tau img = imgNext
  where imgNext = computeThomas diffOperator img
        diffOperator = semiImplictNonlinearDifusion cond k sigma tau img

nonlinearDiffusion :: Conductivity
  -> Double --scale factor
  -> Double -- sigma 
  -> Int -- num intervals
  -> Image VS RGB Double -- base image
  -> [Image VS RGB Double]
nonlinearDiffusion cond k sigma numInt img = imgs
  where (w,h) = dims img
        numOct = numOctaves (w,h)
        sgms = sigmas sigma numOct numInt
        times = timesScales sgms
        taus = P.zipWith (-) times (0 : times)
        sgmTau = zip sgms taus
        cNI = computeNextImage cond k
        imgs = computeImgs sgmTau img cNI
        
computeImgs :: [(Double,Double)]
  -> Image VS RGB Double -- image start
  -> (  Double -- sigma 
      -> Double -- time's interval
      -> Image VS RGB Double -- image t
      -> Image VS RGB Double) -- image t+1
  -> [Image VS RGB Double] -- image list
computeImgs [] img func  = []
computeImgs ((s,t):simtaus) img func = imgNext: computeImgs simtaus imgNext func 
  where imgNext =  func s t img


unflatten :: (Int,Int) -> [a] -> [[a]]
unflatten _ [] = [[]]
unflatten (0,_) xs = [xs]
unflatten (_,0) xs = [xs]
unflatten (w,h) xs
  | w*h == P.length xs = [[xs!!(j*w+i)| i<-[0..w-1]] | j <- [0..h-1] ]
  | otherwise = [xs]

{-
image, sigma=1.6, num_intervals=3, assumed_blur=0.5, image_border_width=5

def generateGaussianKernels(sigma, num_intervals):
    """Generate list of gaussian kernels at which to blur the input image. Default values of sigma, intervals, and octaves follow section 3 of Lowe's paper.
    """
    logger.debug('Generating scales...')
    num_images_per_octave = num_intervals + 3
    k = 2 ** (1. / num_intervals)
    gaussian_kernels = zeros(num_images_per_octave)  # scale of gaussian blur necessary to go from one blur scale to the next within an octave
    gaussian_kernels[0] = sigma

    for image_index in range(1, num_images_per_octave):
        sigma_previous = (k ** (image_index - 1)) * sigma
        sigma_total = k * sigma_previous
        gaussian_kernels[image_index] = sqrt(sigma_total ** 2 - sigma_previous ** 2)
    return gaussian_kernels

def generateGaussianImages(image, num_octaves, gaussian_kernels):
    """Generate scale-space pyramid of Gaussian images
    """
    logger.debug('Generating Gaussian images...')
    gaussian_images = []

    for octave_index in range(num_octaves):
        gaussian_images_in_octave = []
        gaussian_images_in_octave.append(image)  # first image in octave already has the correct blur
        for gaussian_kernel in gaussian_kernels[1:]:
            image = GaussianBlur(image, (0, 0), sigmaX=gaussian_kernel, sigmaY=gaussian_kernel)
            gaussian_images_in_octave.append(image)
        gaussian_images.append(gaussian_images_in_octave)
        octave_base = gaussian_images_in_octave[-3]
        image = resize(octave_base, (int(octave_base.shape[1] / 2), int(octave_base.shape[0] / 2)), interpolation=INTER_NEAREST)
    return array(gaussian_images)

def generateDoGImages(gaussian_images):
    """Generate Difference-of-Gaussians image pyramid
    """
    logger.debug('Generating Difference-of-Gaussian images...')
    dog_images = []

    for gaussian_images_in_octave in gaussian_images:
        dog_images_in_octave = []
        for first_image, second_image in zip(gaussian_images_in_octave, gaussian_images_in_octave[1:]):
            dog_images_in_octave.append(subtract(second_image, first_image))  # ordinary subtraction will not work because the images are unsigned integers
        dog_images.append(dog_images_in_octave)
    return array(dog_images) 

 def generateBaseImage(image, sigma, assumed_blur):
    """Generate base image from input image by upsampling by 2 in both directions and blurring
    """
    logger.debug('Generating base image...')
    image = resize(image, (0, 0), fx=2, fy=2, interpolation=INTER_LINEAR)
    sigma_diff = sqrt(max((sigma ** 2) - ((2 * assumed_blur) ** 2), 0.01))
    return GaussianBlur(image, (0, 0), sigmaX=sigma_diff, sigmaY=sigma_diff)  # the image blur is now sigma instead of assumed_blur

def computeNumberOfOctaves(image_shape):
    """Compute number of octaves in image pyramid as function of base image shape (OpenCV default)
    """
    return int(round(log(min(image_shape)) / log(2) - 1))
-}
