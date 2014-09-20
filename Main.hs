import FRP.Helm
import qualified FRP.Helm.Keyboard as Keyboard
import qualified FRP.Helm.Window as Window

data State = State { mx :: Double, my :: Double }

step :: (Int, Int) -> State -> State
step (dx, dy) state = state { mx = realToFrac dx + mx state,
                              my = realToFrac dy + my state }

render :: (Int, Int) -> State -> Element
render (w, h) (State { mx = mx, my = my }) =
  centeredCollage w h [move (mx, my) $ filled red $ square 100]

main :: IO ()
main =
  do
    engine <- startup defaultConfig
    run engine $ render <~ Window.dimensions engine ~~ stepper
  where
    state = State { mx = 0, my = 0 }
    stepper = foldp step state Keyboard.arrows
