import Debug.Trace
import FRP.Helm
import qualified FRP.Helm.Keyboard as Keyboard
import qualified FRP.Helm.Window as Window
import qualified FRP.Helm.Time as Time

data State = State { xpos :: Double, ypos :: Double,
                     xvel :: Double, yvel :: Double } deriving (Show)

data Impulse = Impulse { dx :: Int, dy :: Int, dt :: Time.Time } deriving (Show)

{-| Given an impulse and a state, returns a new state with updated velocities and positions |-}
update :: Impulse -> State -> State
update (Impulse { dx = dx, dy = dy, dt = dt}) state = trace (show newstate) newstate
  where
    newstate = state { xvel = realToFrac dx / 100.0 + xvel state,
                       yvel = realToFrac dy / 100.0 + yvel state,
                       xpos = xpos state + dt * xvel state,
                       ypos = ypos state + dt * yvel state }

{-| Combine arrow inputs and time delta to an impulse |-}
impulse :: (Int, Int) -> Time.Time -> Impulse
impulse (dx, dy) dt = trace (show i) i
  where
    i = Impulse { dx = dx, dy = dy, dt = dt }

{-| Given a viewport size, returns rendering functions (State -> Element) |-}
render :: (Int, Int) -> State -> Element
render (w, h) (State { xpos = xpos, ypos = ypos }) =
  centeredCollage w h [move (xpos, ypos) $ filled red $ square 100]

main :: IO ()
main =
  do
    engine <- startup defaultConfig
    -- read as "run(engine((render <~ (Window.dimensions engine)) ~~ stepper))"
    -- where "Window.dimensions engine" is a signal generator for viewport sizes,
    -- "render <~" that is a rendering function signal generator, and
    -- that "~~ stepper" applies those functions to states generated by the
    -- updater signal generator
    run engine $ render <~ Window.dimensions engine ~~ updater
  where
    state = State { xpos = 0, ypos = 0, xvel = 0, yvel = 0 }
    updater = foldp update state (lift2 impulse Keyboard.arrows (Time.delay (Time.fps 60)))
