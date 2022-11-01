module Multi exposing (main)
import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Array
import Http
import Random
import WalkCycle exposing (..)
import ClipData exposing (getClip)

-- CONSTANTS

pageWidth = 1920 / 5
pageHeight = 1080 / 5
ip = "elm-amongus-backend-production.up.railway.app"

-- TYPE DEFs

type Msg = Tick Float GetKeyState 
        | PostS (Result Http.Error String) 
        | LogIn (Result Http.Error String) 
        | OtherP (Result Http.Error String)

type Page = Load | Game | Crash

type alias Mate = {
                    id : String,
                    posX : Float, 
                    posY : Float, 
                    flipped : Bool, 
                    animating : Bool,
                    sprites : Array.Array String
                    }

type alias Model = { time : Float,
                     scene : Page,
                     self : Mate,
                     others : List Mate
                    }
   
-- VIEW

get_current_sprite t c = (Array.get ((modBy 9 (round (t*15)))) c) |> Maybe.withDefault (getFirst c)
player_sprite time animating sprites = html 1920 1080 (
        Html.img [
            (if animating 
            then get_current_sprite time sprites
            else getFirst sprites
            ) |> Html.Attributes.src 
        ]
        []
    )
getFirst x = Maybe.withDefault "" (Array.get 0 x)
getNth l n =  Maybe.withDefault "" (Array.get n (Array.fromList l))

renderOtherPlayer time self = (player_sprite time self.animating self.sprites |> move (-50, 95) |> scale 0.25 |> (if self.flipped then mirrorX else identity)) |> move (self.posX, self.posY)

bg_map model = group [
    html 4096 4096 (
      Html.img 
        [Html.Attributes.src "https://i.imgur.com/JYDokZN.png"]
        []
      ) |> scale 0.5  -- |> move (-200, 175)
    , model.others |> List.map (renderOtherPlayer model.time) |> group
  ]

vignette distance = square pageWidth
    |> filled 
      (radialGradient
        [
          transparentStop black 0 0, 
          transparentStop black (pageHeight-distance) 0.05,
          transparentStop black (pageHeight-5) 0.95
        ]
      )
    |> scaleY (9/16)
    |> scale 1.25
    

gameScene model =
  let 
    self = model.self
  in 
  [  
    -- BG + Other Assets
    square 10000 |> filled (rgb 48 48 48)
    , bg_map model |> move (-self.posX, -self.posY)
    , player_sprite model.time self.animating self.sprites |> move (-50, 95) |> scale 0.25 |> (if self.flipped then mirrorX else identity)
    , GraphicSVG.text (Debug.toString (round self.posX, round self.posY)) |> filled white
    -- Post Processing
    , vignette 1200
  ]

view model = 
    case model.scene of 
        Load -> [GraphicSVG.text "Loading" |> filled black]
        Game -> gameScene model
        Crash -> [GraphicSVG.text "Crashed" |> filled black]

-- HTTP
              
sendPos : String -> Float -> Float -> Bool -> Bool -> Cmd Msg
sendPos id x y a f =
  Http.get
    { url = "https://" ++ ip ++ "/moved?id=" ++ id ++ "&x=" ++ String.fromFloat x ++ "&y=" ++ String.fromFloat y ++ "&a=" ++ String.fromInt (if a then 1 else 0) ++ "&f=" ++ String.fromInt (if f then 1 else 0)
    , expect = Http.expectString PostS
    }

getPos : String -> Cmd Msg
getPos id = 
  Http.get 
  { url = "https://" ++ ip ++ "/getloc?id=" ++ id
  , expect = Http.expectString OtherP
  }

logInCmd : Cmd Msg
logInCmd = 
    Http.get 
    { url = "https://" ++ ip ++ "/login"
    , expect = Http.expectString LogIn
    }

-- LOGIC / UPDATE

update msg model = case msg of
                    Tick t (_,(deltaR, thrustR), (deltaL, thrustL)) -> 
                        case model.scene of
                            Game ->
                                let 
                                    oldSelf = model.self
                                    deltaTime = t - model.time
                                    speed = 180 * deltaTime * (if (deltaL /= 0) && (thrustL /= 0) then 0.785398163397 else 1)
                                    scale = 2
                                    possibleNewPosX = oldSelf.posX + deltaL * speed
                                    possibleNewPosY = oldSelf.posY + thrustL * speed
                                    val = getClip (round (possibleNewPosX/scale)) (round (possibleNewPosY/(-scale)))
                                    
                                    canHor = if val > 1 then True else (
                                        let 
                                            otherVal = getClip (round (possibleNewPosX/scale)) (round (oldSelf.posY/(-scale)))
                                        in otherVal > 1
                                        )
                                    canVer = if val > 1 then True else (
                                        let
                                            otherVal = getClip (round (oldSelf.posX/scale)) (round (possibleNewPosY/(-scale)))
                                        in otherVal > 1
                                        )
                                    
                                    newPosX = if canHor then possibleNewPosX else oldSelf.posX
                                    newPosY = if canVer then possibleNewPosY else oldSelf.posY
                                    newFlipped = if deltaL == (-1) then True else (if deltaL == 1 then False else oldSelf.flipped)
                                    newAnimating = (deltaL /= (0) && canHor) || (thrustL /= 0 && canVer)
                                    moved = (newPosX /= oldSelf.posX) || (newPosY /= oldSelf.posY) || newAnimating /= oldSelf.animating
                                    newSelf = { oldSelf | posX = newPosX, posY = newPosY, flipped=newFlipped, animating=newAnimating}
                                in (
                                    { model | time = t, self=newSelf }, 
                                    Cmd.batch 
                                            [ if moved then (sendPos newSelf.id newPosX newPosY newAnimating newFlipped) else Cmd.none
                                            , if ((modBy 5 (round (model.time * 100))) == 0) then (getPos newSelf.id) else Cmd.none]
                                    )
                            _ -> (model, Cmd.none)
                    
                    PostS _ -> (model, Cmd.none)
                    LogIn res -> 
                        case res of 
                            Ok body ->
                                let 
                                    data = body |> String.split ","

                                    newId = getNth data 0 
                                    iPosX = getNth data 1 |> String.toFloat |> Maybe.withDefault 0
                                    iPosY = getNth data 2 |> String.toFloat |> Maybe.withDefault 0
                                    newSprites = getSprites (newId |> String.toInt |> Maybe.withDefault 9)

                                    oldSelf = model.self
                                    newSelf = { oldSelf | id = newId, posX = iPosX, posY = iPosY, sprites = newSprites }
                                in ({ model | self = newSelf, scene = Game }, Cmd.none)
                            Err _ ->
                                ({ model | scene = Crash }, Cmd.none)

                    OtherP res ->
                        case res of
                            Ok data -> 
                                let
                                    toMate str = 
                                        let
                                            pData = str |> String.split ","

                                            id = getNth pData 0
                                            posX = getNth pData 1 |> String.toFloat |> Maybe.withDefault 0
                                            posY = getNth pData 2 |> String.toFloat |> Maybe.withDefault 0
                                            animating = (getNth pData 3 |> String.toInt |> Maybe.withDefault 0) == 1
                                            flipped = (getNth pData 4 |> String.toInt |> Maybe.withDefault 0) == 1
                                            sprites = getSprites (id |> String.toInt |> Maybe.withDefault 9)

                                        in {id=id,posX=posX,posY=posY,animating=animating,sprites=sprites,flipped=flipped}
                                    players = data |> String.split "\n"
                                    newOthers = players |> List.map toMate
                                in ({model | others = newOthers}, Cmd.none)
                            --   let 
                                
                            --     nums = String.split "," data
                            --     otherNewX = nums |> List.head |> Maybe.withDefault "1" |> String.toInt |> Maybe.withDefault 1 |> toFloat
                            --     otherNewY = nums |> List.tail |> Maybe.withDefault [] |> List.head |> Maybe.withDefault "1" |> String.toInt |> Maybe.withDefault 1 |> toFloat
                            --     animating = 1==(nums |> List.reverse |> List.head |> Maybe.withDefault "0" |> String.toInt |> Maybe.withDefault 0)
                            --     deltaL = clamp (-1) 1 (otherNewX - model.otherX)
                            --     otherFlipped = if deltaL == (-1) then True else (if deltaL == 1 then False else model.otherFlipped)
                            --   in ({ model | otherX = otherNewX, otherY = otherNewY, otherFlipped = otherFlipped, otherA = animating}, Cmd.none)
                            Err _ -> ({ model | scene = Crash }, Cmd.none)
                        
-- INIT

init : () -> ( Model, Cmd Msg )
init _ = ({
            time = 0,
            scene = Load, 
            self = {
                id = "",
                posX = 0,
                posY = -0,
                flipped = False,
                animating = False,
                sprites = WalkCycle.devCycle
            }, 
            others = []
          }, logInCmd)

-- MAIN

render model = collage (pageWidth) (pageHeight) (view model)
main = ellieAppWithTick Tick { init = init, view = \ model -> { title = "", body = render model }, update = update, subscriptions = \_ -> Sub.none }

