import Reflex.Dom 

import Data.Map 
import Data.List 
import Data.Matrix (Matrix, fromLists, toLists, multStd2)
import Data.Monoid 

identityMatrix :: Matrix Float
identityMatrix = 
    fromLists [[ 1,  0,  0,  0 ]
              ,[ 0,  1,  0,  0 ]
              ,[ 0,  0,  1,  0 ]
              ,[ 0,  0,  0,  1 ]
              ]

xyRotationMatrix :: Float -> Matrix Float
xyRotationMatrix rotation = 
    let c = cos rotation
        s = sin rotation
    in fromLists [[ c,  s,  0,  0 ]
                 ,[-s,  c,  0,  0 ]
                 ,[ 0,  0,  1,  0 ]
                 ,[ 0,  0,  0,  1 ]
                 ]

yzRotationMatrix :: Float -> Matrix Float
yzRotationMatrix rotation = 
    let c = cos rotation
        s = sin rotation
    in fromLists [[ 1,  0,  0,  0 ]
                 ,[ 0,  c,  s,  0 ]
                 ,[ 0, -s,  c,  0 ]
                 ,[ 0,  0,  0,  1 ]
                 ]

zxRotationMatrix :: Float -> Matrix Float
zxRotationMatrix rotation = 
    let c = cos rotation
        s = sin rotation
    in fromLists [[ c,  0,  s,  0 ]
                 ,[ 0,  1,  0,  0 ]
                 ,[-s,  0,  c,  0 ]
                 ,[ 0,  0,  0,  1 ]
                 ]

translationMatrix :: (Float,Float,Float) -> Matrix Float
translationMatrix (x,y,z) =
    fromLists  [[ 1,  0,  0,  0 ]
               ,[ 0,  1,  0,  0 ]
               ,[ 0,  0,  1,  0 ]
               ,[ x,  y,  z,  1 ]
               ]

scaleMatrix :: Float -> Matrix Float
scaleMatrix s =
    fromLists  [[ s,  0,  0,  0 ]
               ,[ 0,  s,  0,  0 ]
               ,[ 0,  0,  s,  0 ]
               ,[ 0,  0,  0,  1 ]
               ]

-- translate model to (0,0,1) for perspective viewing
perspectivePrepMatrix :: Matrix Float
perspectivePrepMatrix = translationMatrix (0,0,1)

-- perspective transformation 
perspectiveMatrix :: Matrix Float
perspectiveMatrix = 
    fromLists  [[ 1,  0,  0,  0 ]
               ,[ 0,  1,  0,  0 ]
               ,[ 0,  0,  1,  1 ]
               ,[ 0,  0,  0,  0 ] ]

viewScale = 200

-- | Namespace needed for svg elements.
svgNamespace = Just "http://www.w3.org/2000/svg"

transformPoints :: Matrix Float -> Matrix Float -> Matrix Float
transformPoints transform points = points `multStd2` transform

fromMatrix :: Matrix Float -> [(Float,Float)]
fromMatrix mat = (\[x,y,z,w] -> (x/w,y/w)) <$> toLists mat

toMatrix :: [(Float,Float)] -> Matrix Float 
toMatrix points = fromLists $ (\(x,y) -> [x,y,0,1]) <$> points

pointsToString :: [(Float,Float)] -> String
pointsToString = concatMap (\(x,y) -> show x ++ ", " ++ show y ++ " ") 

showPolygon :: MonadWidget t m => String -> [(Float,Float)] -> m ()
showPolygon color pts = do
    elDynAttrNS' svgNamespace "polygon" 
                 (constDyn $  "points" =: pointsToString pts
                           <> "fill" =: color
                           ) $ return ()
    return ()

showAsGroup :: MonadWidget t m => Matrix Float -> [(Float,Float)]  -> m ()
showAsGroup transform points = do
    let transformToString  = intercalate ", " . fmap show . concat . toLists
        attrs = fromList [("style",   
                                      "transform: matrix3d(" 
                                   ++ transformToString transform 
                                   ++ ");" 
                                   ) ] 

    elDynAttrNS' svgNamespace "g" (constDyn attrs) $ showPolygon "Green" points
    return ()

view :: MonadWidget t m => Matrix Float -> m ()
view transform = do 
    let points = [(0,0), (0,0.5), (1,0.5), (1,0)]

        transform2d transform points 
            = fromMatrix $ transformPoints transform $ toMatrix points

    el "div" $ do
        let dAttr = constDyn $  "width" =: show viewScale
                             <> "height" =: show viewScale

        -- show with Red on top
        elDynAttrNS' svgNamespace "svg" dAttr $ do
                         showPolygon "Red" $ transform2d transform points
                         showAsGroup transform points

        -- show with Green on top
        elDynAttrNS' svgNamespace "svg" dAttr $ do
                         showAsGroup transform points
                         showPolygon "Red" $ transform2d transform points
        return ()


main = mainWidget $ do

    -- simplest example ; just show effect of identity matrix
    -- (plus scale and translation for viewing)
    (view $            identityMatrix
            `multStd2` scaleMatrix (viewScale/4)
            `multStd2` translationMatrix (viewScale/2,viewScale/2,0) )

    el "br" $ return ()

    -- Simplest example with perspective viewing trasformations; 
    -- Things are working fine in this case with all z coords zero.
    (view $            identityMatrix
            `multStd2` perspectivePrepMatrix
            `multStd2` perspectiveMatrix
            `multStd2` scaleMatrix (viewScale/4)
            `multStd2` translationMatrix (viewScale/2,viewScale/2,0) )

    el "br" $ return ()

    -- This time rotate rectangle around y axis resulting in 
    -- some z coords greater than zero.  Now Red rectangle looks
    -- as expected (right side shorter due to perspective) 
    -- but Green does not.
    (view $            identityMatrix
            `multStd2` zxRotationMatrix (pi / 4)
            `multStd2` perspectivePrepMatrix
            `multStd2` perspectiveMatrix
            `multStd2` scaleMatrix (viewScale/4)
            `multStd2` translationMatrix (viewScale/2,viewScale/2,0) )

    el "br" $ return ()

    -- More complicated example with composite translations, rotations
    -- but no perspective projection.  Red and Green display 
    -- identically ( as expected ).
    (view $            identityMatrix
            `multStd2` translationMatrix (1.15,1.2,1.0) 
            `multStd2` xyRotationMatrix (pi / 3)
            `multStd2` zxRotationMatrix (pi / 4)
            `multStd2` yzRotationMatrix (pi / 4)
            `multStd2` perspectivePrepMatrix
            -- `multStd2` perspectiveMatrix
            `multStd2` scaleMatrix (viewScale/4)
            `multStd2` translationMatrix (viewScale/2,viewScale/2,0) )

    el "br" $ return ()

    -- Add perspective to example above and Red and Green
    -- no longer display to same location (Green goes off screen 
    -- entirely).
    (view $            identityMatrix
            `multStd2` translationMatrix (1.15,1.2,1.0) 
            `multStd2` xyRotationMatrix (pi / 3)
            `multStd2` zxRotationMatrix (pi / 4)
            `multStd2` yzRotationMatrix (pi / 4)
            `multStd2` perspectivePrepMatrix
            `multStd2` perspectiveMatrix
            `multStd2` scaleMatrix (viewScale/4)
            `multStd2` translationMatrix (viewScale/2,viewScale/2,0) )
