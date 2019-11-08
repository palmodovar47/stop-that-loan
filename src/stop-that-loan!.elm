module Main exposing
    ( BadLoan
    , Coords
    , GoodLoan
    , Model
    , Msg(..)
    , init
    , main
    , makeGoodLoan
    , subscriptions
    , update
    , view
    )

import Array exposing (..)
import Browser
import Browser.Events exposing (onAnimationFrame)
import Debug exposing (log)
import Html exposing (Html, button, div, h1, p)
import Html.Attributes exposing (style)
import Html.Events exposing (on, targetValue)
import Json.Decode as Json exposing (Decoder, field, string)
import Platform.Cmd exposing (batch)
import Random
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onMouseDown)
import Task
import Time
import Tuple exposing (first, mapFirst, mapSecond, second)
import Uuid.Barebones



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias GoodLoan =
    { id : String
    , vector : Vector
    , coords : Coords
    , color : String
    , active : Bool
    , speed : Float
    }


genGoodLoan : Model -> GoodLoan
genGoodLoan model =
    let
        package =
            Random.step Uuid.Barebones.uuidStringGenerator
                (Random.initialSeed
                    (Time.posixToMillis model.now)
                )

        uid =
            first package

        vector =
            genRandomVector model

        coords =
            vector.beginning

        speed =
            first (Random.step (Random.float 1 3) (second package))
    in
    GoodLoan uid vector coords "green" True speed


type alias BadLoan =
    { id : String
    , vector : Vector
    , coords : Coords
    , color : String
    , active : Bool
    , speed : Float
    }


genBadLoan : Model -> BadLoan
genBadLoan model =
    let
        package =
            Random.step Uuid.Barebones.uuidStringGenerator
                (Random.initialSeed
                    (Time.posixToMillis model.now)
                )

        uid =
            first package

        vector =
            genRandomVector model

        coords =
            vector.beginning

        speed =
            first (Random.step (Random.float 3 6) (second package))
    in
    BadLoan uid vector coords "red" True speed


type alias Model =
    { goodLoans : List GoodLoan
    , badLoans : List BadLoan
    , initialTimeTillLaunch : Float
    , maxTimeTillLaunch : Float
    , curTimeTillLaunch : Float
    , maxMistakes : Int
    , mistakes : Int
    , maxActiveLoans : Int
    , score : Int
    , brdWidth : Float
    , brdHeight : Float
    , now : Time.Posix
    , zone : Time.Zone
    , gameOver : Bool
    , paused : Bool
    }


initialTimeTillLaunch =
    1500


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        []
        []
        initialTimeTillLaunch
        initialTimeTillLaunch
        0
        5
        0
        10
        0
        1700
        700
        (Time.millisToPosix 0)
        Time.utc
        False
        False
    , Task.perform AdjustTimeZone Time.here
    )



-- UPDATE


type alias Coords =
    ( Float, Float )


type alias Vector =
    { mag : Float, beginning : Coords, end : Coords }


genRandomVector : Model -> Vector
genRandomVector model =
    let
        now =
            Time.posixToMillis model.now

        firstStep =
            Random.step
                (Random.float 0 model.brdHeight)
                (Random.initialSeed now)

        ranY1 =
            first firstStep

        beginning =
            ( 0, ranY1 )

        end =
            ( model.brdWidth, ranY1 )

        magnitude =
            Basics.sqrt ((ranY1 ^ 2) + (model.brdWidth ^ 2))
    in
    Vector magnitude beginning end


type alias IncomingLoanId =
    String


type Msg
    = ShotGoodLoan IncomingLoanId
    | ShotBadLoan IncomingLoanId
    | NextFrame Time.Posix
    | AdjustTimeZone Time.Zone
    | Paused
    | NewGame
    | Resume


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShotGoodLoan loanId ->
            ( { model
                | mistakes = model.mistakes + 1
                , goodLoans =
                    model.goodLoans
                        |> List.filter (\n -> n.id /= loanId)
                , score = model.mistakes + 1
              }
            , Cmd.none
            )

        ShotBadLoan loanId ->
            ( { model
                | badLoans =
                    model.badLoans
                        |> List.filter (\n -> n.id /= loanId)
                , score = model.score + 1
              }
            , Cmd.none
            )

        NextFrame time ->
            ( model
                |> setTime time
                |> checkForGameOver
                |> moveLoansForward
                |> checkForGoals
                |> manageTimers
            , Cmd.none
            )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }
            , Cmd.none
            )

        Paused ->
            ( { model | paused = True }
            , Cmd.none
            )

        NewGame ->
            ( Model
                []
                []
                initialTimeTillLaunch
                initialTimeTillLaunch
                0
                5
                0
                10
                0
                1700
                700
                (Time.millisToPosix 0)
                Time.utc
                False
                False
            , Cmd.none
            )

        Resume ->
            ( { model | paused = False }
            , Cmd.none
            )



-- UPDATE HELPERS


checkForGameOver : Model -> Model
checkForGameOver model =
    if model.mistakes >= model.maxMistakes then
        { model | gameOver = True }

    else
        model


moveLoansForward : Model -> Model
moveLoansForward model =
    { model
        | goodLoans =
            model.goodLoans
                |> List.map moveGoodAlongVector
        , badLoans =
            model.badLoans
                |> List.map moveBadAlongVector
    }


moveGoodAlongVector : GoodLoan -> GoodLoan
moveGoodAlongVector ln =
    { ln | coords = calcNextCoords ln.vector ln.coords ln.speed }


moveBadAlongVector : BadLoan -> BadLoan
moveBadAlongVector ln =
    { ln | coords = calcNextCoords ln.vector ln.coords ln.speed }


calcNextCoords : Vector -> Coords -> Float -> Coords
calcNextCoords vec curCoords spd =
    let
        x =
            first curCoords + spd

        y =
            second curCoords
    in
    ( x, y )


removeGoodLoans : Model -> List String -> List GoodLoan
removeGoodLoans model ids =
    model.goodLoans
        |> List.filter (\loan -> not (List.member loan.id ids))


removeBadLoans : Model -> List String -> List BadLoan
removeBadLoans model ids =
    model.badLoans
        |> List.filter (\loan -> not (List.member loan.id ids))


checkForGoals : Model -> Model
checkForGoals model =
    let
        isPassedGoalLine =
            \loan -> first loan.coords >= model.brdWidth

        onlyIds =
            \loan -> loan.id

        glsIds =
            model.goodLoans
                |> List.filter isPassedGoalLine
                |> List.map onlyIds

        blsIds =
            model.badLoans
                |> List.filter isPassedGoalLine
                |> List.map onlyIds

        mistakes =
            model.mistakes + List.length blsIds

        score =
            model.score + List.length glsIds

        gls =
            glsIds
                |> removeGoodLoans model

        bls =
            blsIds
                |> removeBadLoans model
    in
    { model
        | goodLoans = gls
        , badLoans = bls
        , mistakes = mistakes
        , score = score
    }


manageTimers : Model -> Model
manageTimers model =
    let
        newTime =
            model.curTimeTillLaunch - (1000 / 60)

        newMaxTime =
            model.maxTimeTillLaunch - 0.15

        mustAddLoan =
            newTime <= 0

        now =
            Time.posixToMillis model.now

        mustBeGood =
            if
                first
                    (Random.step
                        (Random.int 0 1)
                        (Random.initialSeed now)
                    )
                    == 0
            then
                False

            else
                True

        gls =
            if mustAddLoan && mustBeGood then
                launchGoodLoan model

            else
                model.goodLoans

        bls =
            if mustAddLoan && not mustBeGood then
                launchBadLoan model

            else
                model.badLoans

        finalCurTime =
            if mustAddLoan then
                newMaxTime

            else
                newTime
    in
    { model
        | curTimeTillLaunch = finalCurTime
        , maxTimeTillLaunch = newMaxTime
        , goodLoans = gls
        , badLoans = bls
    }


launchGoodLoan : Model -> List GoodLoan
launchGoodLoan model =
    genGoodLoan model :: model.goodLoans


launchBadLoan : Model -> List BadLoan
launchBadLoan model =
    genBadLoan model :: model.badLoans


setTime : Time.Posix -> Model -> Model
setTime time model =
    { model | now = time }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.paused || model.gameOver then
        Sub.none

    else
        onAnimationFrame NextFrame



-- VIEW


idValue : Decoder String
idValue =
    field "target" (field "id" Json.string)


onMouseDownWithId : (String -> msg) -> Attribute msg
onMouseDownWithId tagger =
    on "mousedown" (Json.map tagger idValue)


makeGoodLoan : GoodLoan -> Svg Msg
makeGoodLoan loan =
    let
        xx =
            loan.coords |> first |> String.fromFloat

        yy =
            loan.coords |> second |> String.fromFloat
    in
    rect
        [ id loan.id
        , width "50"
        , height "100"
        , onMouseDownWithId ShotGoodLoan
        , x xx
        , y yy
        , fill loan.color
        , stroke "white"
        ]
        [ text "Good" ]


makeBadLoan : BadLoan -> Svg Msg
makeBadLoan loan =
    let
        xx =
            loan.coords |> first |> String.fromFloat

        yy =
            loan.coords |> second |> String.fromFloat
    in
    rect
        [ id loan.id
        , width "50"
        , height "100"
        , onMouseDownWithId ShotBadLoan
        , x xx
        , y yy
        , fill loan.color
        , stroke "white"
        ]
        [ text "Bad" ]


renderActiveLoans : Model -> List (Svg Msg)
renderActiveLoans model =
    let
        good =
            model.goodLoans
                |> List.filter (\n -> n.active)
                |> List.map makeGoodLoan

        bad =
            model.badLoans
                |> List.filter (\n -> n.active)
                |> List.map makeBadLoan
    in
    good |> List.append bad


showControls : Model -> Html Msg
showControls model =
    div []
        [ button [ Html.Events.onClick Paused ] [ Html.text "Pause" ]
        , button [ Html.Events.onClick NewGame ] [ Html.text "New Game" ]
        ]


showGameOver : Model -> Html Msg
showGameOver model =
    div []
        [ h1 [] [ Html.text "GAME OVER!!!" ]
        , p [] [ Html.text ("Score: " ++ String.fromInt model.score) ]
        ]


showGamePaused : Model -> Html Msg
showGamePaused model =
    div []
        [ h1 [] [ Html.text "PAUSED..." ]
        , button [ Html.Events.onClick Resume ] [ Html.text "Resume Game" ]
        ]


stl =
    Html.Attributes.style


gameboardStyles : List (Html.Attribute msg)
gameboardStyles =
    [ stl "border" "solid 1px"
    , stl "background" "gray"
    ]


showGameOn : Model -> Html Msg
showGameOn model =
    div
        ([] |> List.append gameboardStyles)
        [ svg
            [ width (String.fromFloat model.brdWidth)
            , height (String.fromFloat model.brdHeight)
            ]
            ([]
                |> List.append (renderActiveLoans model)
            )
        ]


view : Model -> Html Msg
view model =
    div []
        [ showControls model
        , if model.gameOver then
            showGameOver model

          else if model.paused then
            showGamePaused model

          else
            showGameOn model
        ]
