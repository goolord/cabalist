-- | The app's palette and nano-ui theme, and the few pieces the view is built
-- from: sized buttons, a surface, icons, and the release track.
--
-- Neutral graphite, with Hackage purple kept to the button that moves a
-- release on and a few small marks. The other colours carry state: sage for
-- released or done, amber for what needs attention, coral for what failed.
module Gui.Style
  ( Palette (..)
  , palette
  , appTheme
  , ink
    -- * Type scale
  , sizeSmall
  , sizeBody
  , sizeTitle
  , sizeDisplay
    -- * Buttons
  , quiet
  , actionButton
  , actionButton'
  , compactButton
  , disclosure
    -- * Surfaces
  , surface
    -- * Icons
  , icon
  , iconCheck
  , iconCross
  , iconDot
    -- * Release track
  , releaseTrack
  , drawKey
  )
where

import Data.Char (ord)
import Data.Text (Text)
import Data.Text qualified as T
import NanoUI
import NanoUI.Testing (UiCursorKind (..))

data Palette = Palette
  { palGround :: !Color
  -- ^ The window.
  , palWell :: !Color
  -- ^ Lists and the log: content sits a step below the ground.
  , palRaised :: !Color
  -- ^ Dialogs and the activity bar.
  , palControl :: !Color
  , palHover :: !Color
  , palPressed :: !Color
  , palLine :: !Color
  -- ^ Separators, and the unreached part of the track.
  , palBorder :: !Color
  , palSelect :: !Color
  -- ^ The selected row.
  , palText :: !Color
  , palMuted :: !Color
  , palQuiet :: !Color
  , palPurple :: !Color
  -- ^ Hackage purple, lifted a little for contrast: the fill of the one
  -- button that moves a release on.
  , palPurpleInk :: !Color
  -- ^ A light tint of it, for small marks on dark: the current stop, the
  -- selected row's edge, packages mid-release.
  , palSage :: !Color
  , palAmber :: !Color
  , palCoral :: !Color
  }

-- | Neutral graphite, with no hue in the greys, so purple appears only where
-- it means something.
palette :: Palette
palette =
  Palette
    { palGround = rgb 30 30 30
    , palWell = rgb 25 25 25
    , palRaised = rgb 38 38 38
    , palControl = rgb 52 52 52
    , palHover = rgb 64 64 64
    , palPressed = rgb 44 44 44
    , palLine = rgb 56 56 56
    , palBorder = rgb 76 76 76
    , palSelect = rgb 46 46 46
    , palText = rgb 236 236 234
    , palMuted = rgb 184 184 180
    , palQuiet = rgb 150 150 146
    , palPurple = rgb 111 95 160
    , palPurpleInk = rgb 168 152 216
    , palSage = rgb 150 190 150
    , palAmber = rgb 222 180 110
    , palCoral = rgb 228 132 120
    }
  where
    rgb r g b = colorRGBA r g b 255

-- | Sizes of the type scale: notes, body, section titles, the package name.
sizeSmall, sizeBody, sizeTitle, sizeDisplay :: Float
sizeSmall = 14
sizeBody = 16
sizeTitle = 20
sizeDisplay = 30

appTheme :: Theme
appTheme =
  Theme
    { themeWindow = palGround p
    , themePanel = style (palRaised p) (palLine p) (palHover p) (palPressed p) 8
    , -- Dialogs and drop-downs share the raised surface: nano-ui fills padded
      -- containers with the panel colour, which would otherwise show as a box
      -- inside the dialog.
      themeFloatingWindow = style (palRaised p) (palBorder p) (palRaised p) (palRaised p) 10
    , themeButton = style (palControl p) (palControl p) (palHover p) (palPressed p) 6
    , themeInput = style (palWell p) (palBorder p) (palWell p) (palWell p) 6
    , themeSeparator = palLine p
    , themeAccent = palPurple p
    , themeMuted = palMuted p
    , themeRed = palCoral p
    , themeOrange = palAmber p
    , themeYellow = palAmber p
    , themeGreen = palSage p
    , themePurple = palPurple p
    , themeOverlayDim = colorRGBA 0 0 0 150
    , themeOnAccent = colorRGBA 255 255 255 255
    , themeSelection = let c = palPurpleInk p in colorRGBA (colorR c) (colorG c) (colorB c) 70
    , themeFocusRing = palPurpleInk p
    , -- Links read as text: bright beside their quiet labels, underlined when
      -- hovered.
      themeLink = palText p
    , themeShadow = colorRGBA 0 0 0 120
    , themeDisabledFade = 0.6
    }
  where
    p = palette
    style bg border hover active radius =
      Style
        { styleBg = bg
        , styleFg = palText p
        , styleBorder = border
        , styleBorderWidth = 1
        , styleCornerRadius = radius
        , styleHoverBg = hover
        , styleActiveBg = active
        }

ink :: (Palette -> Color) -> Layout -> Layout
ink colour = fontColor (colour palette)

-- | Buttons with no fill until hovered, in the muted text colour, for the
-- actions beside a primary one.
quiet :: Theme -> Theme
quiet = subtle . buttonStyle (foreground (palMuted palette))

-- | A button with room around its label. nano-ui sizes a button from its
-- text alone, so the size is set here.
actionButton :: Text -> NanoUI Bool
actionButton = sizedButton 36 36

-- | A shorter button, for toolbars and rows.
compactButton :: Text -> NanoUI Bool
compactButton = sizedButton 24 30

sizedButton :: Float -> Float -> Text -> NanoUI Bool
sizedButton sidePad h txt = respClicked <$> sizedButton' sidePad h txt

sizedButton' :: Float -> Float -> Text -> NanoUI Response
sizedButton' sidePad h txt = do
  fm <- uiFontMetrics
  buttonWith' (fixedWH (fromIntegral (ceiling (lineWidth fm txt + sidePad) :: Int)) h . alignMid) txt

-- | 'actionButton' with its response, for a button that anchors a menu.
actionButton' :: Text -> NanoUI Response
actionButton' = sizedButton' 36 36

-- | A line of text that opens and closes what follows it, with a chevron
-- that points down while it is open. Its text starts at the column's edge,
-- where a button's would be inset.
disclosure :: Bool -> Text -> NanoUI Bool
disclosure open txt = do
  fm <- uiFontMetrics
  let w = lineWidth fm txt + 22
  (resp, ()) <-
    customWidget
      defaultCustomWidgetSpec
        { widgetLayout = (fixedWH w 30 . alignMid) defaultLayout
        , widgetContent = drawKey txt [] [if open then 1 else 0]
        , widgetDraw = \dc r -> runCanvas $ do
            let c = if cdcHovered dc then palText palette else palMuted palette
                cy = rectY r + rectH r / 2
                tx = rectX r + lineWidth (cdcFont dc) txt + 8
                chevron (V2 ax ay) (V2 bx by) = drawStroke (V2 ax ay) (V2 bx by) 1.5 c
            drawText (V2 (rectX r) cy) AlignStart AlignMiddle txt c
            if open
              then chevron (V2 tx (cy - 2)) (V2 (tx + 4) (cy + 2)) >> chevron (V2 (tx + 4) (cy + 2)) (V2 (tx + 8) (cy - 2))
              else chevron (V2 (tx + 2) (cy - 4)) (V2 (tx + 6) cy) >> chevron (V2 (tx + 6) cy) (V2 (tx + 2) (cy + 4))
        , widgetCursor = Just (const UiCursorPointer)
        }
  pure (respClicked resp)

-- | A panel in its own colours.
surface :: Color -> Color -> Float -> (Layout -> Layout) -> NanoUI a -> NanoUI a
surface bg border radius f =
  styled (panelStyle (background bg . borderColor border . cornerRadius radius)) . panelWith f

-- | A one-colour icon, @size@ px square, centred in its row.
icon :: Float -> Color -> Svg -> NanoUI ()
icon size color = svgIconWith (fixedWH size size . alignMid . fontColor color)

svg :: Text -> Svg
svg body =
  either (error . ("Gui.Style: bad icon: " <>)) id . parseSvg $
    "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\">"
      <> body
      <> "</svg>"

iconCheck, iconCross, iconDot :: Svg
iconCheck = svg "<path d=\"M5 12.5l4.5 4.5L19 7.5\" stroke-width=\"2.5\"/>"
iconCross = svg "<path d=\"M7 7l10 10M17 7L7 17\" stroke-width=\"2.5\"/>"
iconDot = svg "<circle cx=\"12\" cy=\"12\" r=\"4.5\" fill=\"currentColor\" stroke=\"none\"/>"

-- | The stops of a release, with the one the package is at. Stops before it
-- are done (sage), it is drawn in @current@'s colour, and later ones are
-- open circles on an unlit line. Past the last stop, every stop is done.
releaseTrack :: [Text] -> Int -> Color -> NanoUI ()
releaseTrack stops at current = do
  _ <-
    customWidget
      defaultCustomWidgetSpec
        { widgetLayout = (fillW . maxW 720 . fixedH 56) defaultLayout
        , widgetContent = drawKey (T.intercalate "\0" stops) [current] [fromIntegral at]
        , widgetDraw = \_ r -> runCanvas $ do
            let n = length stops
                inset = 9
                x0 = rectX r + inset
                x1 = rectX r + rectW r - inset - 90
                cy = rectY r + 12
                xAt i = if n <= 1 then x0 else x0 + (x1 - x0) * fromIntegral i / fromIntegral (n - 1)
                done i = i < at
                colourAt i
                  | done i = palSage palette
                  | i == at = current
                  | otherwise = palBorder palette
            -- The line between stops, lit up to the current one.
            mapM_
              ( \i -> do
                  let lit = done (i + 1) || (i + 1 == at)
                  drawRect (Rect (xAt i + 8) (cy - 1) (xAt (i + 1) - xAt i - 16) 2) (if lit then palSage palette else palLine palette)
              )
              [0 .. n - 2]
            mapM_
              ( \(i, name) -> do
                  let x = xAt i
                      c = colourAt i
                  if done i || i == at
                    then drawCircle (V2 x cy) 6 c
                    else drawStrokeCircle (V2 x cy) 6 1.5 (palQuiet palette)
                  drawText (V2 (x - 6) (cy + 26)) AlignStart AlignMiddle name (if i == at then palText palette else palQuiet palette)
              )
              (zip [0 ..] stops)
        }
  pure ()

-- | A 'widgetContent' key covering text, colours and numbers: a custom
-- widget whose key is unchanged is not redrawn.
drawKey :: Text -> [Color] -> [Float] -> Int
drawKey txt colors numbers =
  let seed = fromIntegral (contentKey numbers)
      withColors = foldl' (\acc c -> mix64 acc (fromIntegral (colorToWord32 c))) seed colors
      key = fromIntegral (T.foldl' (\acc ch -> mix64 acc (fromIntegral (ord ch))) withColors txt)
   in if key == 0 then 1 else key
