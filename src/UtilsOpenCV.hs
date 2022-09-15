{-# LANGUAGE PackageImports #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# language DataKinds #-}
{-# language KindSignatures #-}
{-# language FlexibleInstances #-}
--{-# language ScopedTypeVariables #-}
{-# language FlexibleContexts #-}

module UtilsOpenCV where

import Control.Monad ( forM_, forM , void)
import Control.Monad.Trans.Class

import qualified "bytestring" Data.ByteString as B
import "data-default" Data.Default
import Data.Int (Int32, Int)
import Data.Maybe (fromJust)
import Data.List (sort)
import Data.Word
import qualified Data.Vector as DV
import Data.Proxy
import Data.Typeable
import GHC.TypeLits
import Foreign.C.Types
import "opencv" OpenCV
import OpenCV.Calib3d
import OpenCV.Core.Types
import OpenCV.Core.Types.Mat
import OpenCV.Features2d
import OpenCV.Exception
import OpenCV.TypeLevel

import Linear.Vector ( (^+^), (^-^) )
import Linear.V2
import Linear.V4
import GHC.Float (maxExpt10)


loadImg :: ImreadMode -> FilePath -> IO (Mat ('S ['D, 'D]) 'D 'D)
loadImg readMode fp = imdecode readMode <$> B.readFile ("img/" <> fp)

blue,green,red,white,black:: Scalar
blue   = toScalar (V4 255   0   0 255 :: V4 Double)
green  = toScalar (V4   0 255   0 255 :: V4 Double)
red    = toScalar (V4   0   0 255 255 :: V4 Double)
white  = toScalar (V4 255 255 255 255 :: V4 Double)
black  = toScalar (V4   0   0   0 255 :: V4 Double)

slicePt :: DV.Vector (DV.Vector (V2 CDouble))
  -> Int
  -> Maybe (DV.Vector (V2 CDouble))
slicePt  twoPoints 0 = Just $ fmap ( DV.! 0) twoPoints
slicePt  twoPoints 1 = Just $ fmap ( DV.! 1) twoPoints
slicePt  twoPoints _ = Nothing

matchPoints :: DV.Vector DMatch -- matches
    -> DV.Vector KeyPoint -- keyPoints 1
    -> DV.Vector KeyPoint -- keyPoints 2
    -> DV.Vector (DV.Vector (V2 CDouble)) -- matched Points at images
matchPoints mtchs kpts1 kpts2 = forM  mtchs matcher
    where matcher = matchPt kpts1 kpts2

matchPt :: DV.Vector KeyPoint
  -> DV.Vector KeyPoint
  -> DMatch
  -> DV.Vector (V2 CDouble)
matchPt kpts1 kpts2 mtch = (DV.empty `DV.snoc` query)  `DV.snoc` train
    where matchRec = dmatchAsRec mtch
          f2cd = CDouble . realToFrac
          queryPt = kpts1 DV.! fromIntegral (dmatchQueryIdx matchRec)
          trainPt = kpts2 DV.! fromIntegral (dmatchTrainIdx matchRec)
          V2 x1 y1 = kptPoint $ keyPointAsRec queryPt
          V2 x2 y2= kptPoint $ keyPointAsRec trainPt
          query = V2 (f2cd x1) (f2cd y1)
          train = V2 (f2cd x2) (f2cd y2)

computeFundMat :: FundamentalMatMethod
    -> Int
    -> DV.Vector DMatch -- matches
    -> DV.Vector KeyPoint -- keyPoints 1
    -> DV.Vector KeyPoint -- keyPoints 2
    -> Maybe (Mat (S '[D, S 3]) (S 1) (S Double), Mat (S '[D, D]) (S 1) (S Word8))
computeFundMat method n mtchs kpts1 kpts2 = fundMat
    where fundMat = exceptError $ findFundamentalMat pts1 pts2 method
          srtMtch = sortMatches mtchs n
          ptsExtract = matchPoints srtMtch kpts1 kpts2
          Just pts1 = slicePt ptsExtract 0
          Just pts2 = slicePt ptsExtract 1

sortMatches :: DV.Vector DMatch
  -> Int
  -> DV.Vector DMatch
sortMatches matches n = DV.take n $ sortMatchesDist matches

sortMatchesDist :: DV.Vector DMatch -> DV.Vector DMatch
sortMatchesDist matches = DV.fromList sortMatches
    where sortMatches = reverse $ (\(_,i)-> matches DV.! i ) <$> sort matchesDist
          matchesDist = zip (DV.toList $ fmap (dmatchDistance.dmatchAsRec) matches) [1..]

mainOpenCV :: IO ()
mainOpenCV = do
    -- lenna <- loadImg ImreadUnchanged "Lenna.png"
    img1 <- loadImg ImreadUnchanged "cat1.jpg"

    let info = matInfo img1
        shape@height:width:_ = miShape info
        width2 = 2 * width
        channels = miChannels info
        depth = miDepth info

    let orb = mkOrb defaultOrbParams {orb_nfeatures = 150}

    img2 <- loadImg ImreadUnchanged "cat2.jpg"
    print "first image:"
    let (kpts1, descs1) = exceptError $ orbDetectAndCompute orb img1 Nothing
    -- forM_ kpts1 $ \kpt -> do
    --     let kptRec = keyPointAsRec kpt
    --     putStrLn $ "KeyPoint: " <> show (kptPoint kptRec)
    print "second image:"
    let (kpts2, descs2) = exceptError $ orbDetectAndCompute orb img2 Nothing
    -- forM_ kpts2 $ \kpt -> do
    --     let kptRec = keyPointAsRec kpt
    --     putStrLn $ "KeyPoint: " <> show (kptPoint kptRec)
    fbmatcher <- newFlannBasedMatcher (def { indexParams = FlannLshIndexParams 20 10 2 })
    print "matches: "
    matches <- match fbmatcher
                     descs1 -- Query descriptors
                     descs2 -- Train descriptors
                     Nothing
    forM_ matches $ \mtch -> do
        let mtchRec = dmatchAsRec mtch
        putStrLn $ "Match: " <> show  mtchRec
    print "fundamental matrix:"
    let fundMat = computeFundMat method 10 matches kpts1 kpts2
        method = FM_8Point --FM_Lmeds (Just 0.50)--FM_7Point -- FM_Ransac (Just 0.999) (Just 1.0) --
        ptsExtract = matchPoints matches kpts1 kpts2
        Just pts1 = slicePt ptsExtract 0
        Just pts2 = slicePt ptsExtract 1
    print $ case fundMat of Nothing             -> "computation of Fundamental matrix is failed"
                            Just (mat1,mat2)    -> show (matInfo mat1) ++ " "
                                                     ++ show (matInfo mat2)
    print "matches image:"
    img <-exceptErrorIO $
        withMatM (height:::width2:::Z) channels depth
            white $ \imgM -> do
                matCopyToM imgM (V2 0     0)  img1 Nothing
                matCopyToM imgM (V2 width 0)  img2 Nothing--eyeMat height width2 channels depth
                -- Draw the matches as lines from the query image to the train image.
                forM_ matches $ \dmatch -> do
                    let matchRec = dmatchAsRec dmatch
                        queryPt = kpts1 DV.! fromIntegral (dmatchQueryIdx matchRec)
                        trainPt = kpts2 DV.! fromIntegral (dmatchTrainIdx matchRec)
                        queryPtRec = keyPointAsRec queryPt
                        trainPtRec = keyPointAsRec trainPt
                -- We translate the train point one width to the right in order to
                -- match the position of rotatedFrog in imgM.
                    line imgM
                        (round <$> kptPoint queryPtRec :: V2 Int32)
                        ((round <$> kptPoint trainPtRec :: V2 Int32) ^+^ V2 width 0)
                        blue 1 LineType_AA 0
    --print $ typeOf img
    window1 <- makeWindow "testMatches"
    imshow window1 img
    let correctF = exceptError $ coerceMat fMat
        fMat = fst (fromJust fundMat)
        
    let img3 = exceptError $ computeCorrespondEpilines pts1 Image1 $ toMat correctF
        img4 = exceptError $ computeCorrespondEpilines pts2 Image2 $ toMat correctF
    -- imgEp <-exceptErrorIO $
    --     withMatM (height:::width2:::Z) channels depth -- (toShape [height,width2]) (toChannels channels) (toDepth depth)
    --         white $ \imgM -> do 
    --             matCopyToM imgM (V2 0     0)  img3 Nothing
    --             matCopyToM imgM (V2 width 0)  img4 Nothing
    window2 <- makeWindow "testEpilines"
    imshow window2 correctF
    void $ waitKey 10000
