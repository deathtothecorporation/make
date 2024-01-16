port module Sink exposing (main)

import BigInt exposing (BigInt)
import Browser exposing (Document)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Heroicons.Outline
import Json.Decode as JD
import Ur
import Ur.Cmd
import Ur.Deconstructor as D exposing (Deconstructor)
import Ur.Jam exposing (..)
import Ur.Run
import Ur.Sub
import Ur.Types exposing (Noun(..))
import Urbit.Encoding.Atom as U
import Urbit.Encoding.Phonemic exposing (..)
import Widget
import Widget.Icon as Icon
import Widget.Material as Material


url : String
url =
    "http://localhost:8085"


main : Ur.Run.Program Model Msg
main =
    Ur.Run.application
        { init =
            \_ _ ->
                ( { error = ""
                  , entries = Nothing
                  , newEntry = ""
                  , shipName = Nothing
                  , vapors = Nothing
                  , state = Nothing
                  , showTech =
                        [ ( Desks, True )
                        , ( Perms, True )
                        , ( Pipes, True )
                        , ( Lands, True )
                        ]
                            |> List.map (Tuple.mapFirst toString)
                            |> Dict.fromList
                  }
                , Cmd.batch
                    [ Ur.logIn url "code"
                        |> Cmd.map (result (Debug.toString >> Error) (always Noop))
                    , Ur.getShipName url |> Cmd.map (result (always Noop) GotShipName)
                    , Ur.scry
                        { url = url
                        , path = [ "vapors" ]
                        , agent = "make"
                        , error = Error "Failed to get vapors"
                        , success =
                            vapors
                                |> D.map GotVapors
                                |> debug (Debug.log "vapors")
                        }
                    ]
                    |> Ur.Cmd.cmd
                )
        , urbitUrl = \_ -> url
        , update = update
        , view = view
        , subscriptions = always Sub.none
        , createEventSource = createEventSource
        , urbitSubscriptions =
            \{ shipName } ->
                case shipName of
                    Just ship ->
                        Ur.Sub.batch
                            [ Ur.Sub.sink
                                { ship = ship
                                , app = "make"
                                , path = [ "sync" ]
                                , deconstructor =
                                    D.oneOf
                                        [ vapors
                                            |> D.map GotVapors
                                        , D.tar |> D.map (Debug.toString >> Error)
                                        ]
                                }
                            ]

                    _ ->
                        Ur.Sub.none
        , onEventSourceMsg = onEventSourceMessage
        , onUrlChange = \_ -> Noop
        , onUrlRequest = \_ -> Noop
        }


type alias Model =
    { error : String
    , entries : Maybe (List ( BigInt, String ))
    , newEntry : String
    , shipName : Maybe String
    , vapors : Maybe Vapors
    , showTech : Dict String Bool
    , state : Maybe Noun
    }


type Msg
    = Noop
    | Error String
    | GotState Noun
    | GotVapors Vapors
    | Toggle TechType Bool
    | GotAxal Axal
    | RunCmd (Ur.Cmd.Cmd Msg)
    | GotShipName String


type Axal
    = Axal ( Maybe Tech, Dir )


type Dir
    = Dir ( Maybe String, Maybe Axal )


type Vapor
    = Vapor ( Slip, ( Maybe Pack, Maybe String ) )


type Vapors
    = Vapors (List Vapor)


type alias Mars =
    { slip : Slip
    , pace : Axal
    }


type alias Pack =
    { urth : Noun
    , mars : Mars
    , host : Host
    }


type alias Bond =
    { bond : String }


type alias Item =
    { item : Maybe BigInt }


type alias Host =
    { host : String }


type TechType
    = Desks
    | Perms
    | Pipes
    | Lands


type Tech
    = Desk (List String)
    | Perm (List String)
    | Pipe (List String)
    | Land (Maybe String) (Maybe String)


type Slip
    = Slip ( Bond, Item )


vapors : Deconstructor Vapors
vapors =
    let
        rec =
            D.oneOf
                [ D.lazy (\_ -> vapors) |> debug (Debug.log "lazy vapors")

                --  , (D.cell (D.cell D.sig (D.lazy (\_ -> vapors))) D.ignore) |> D.map (\( (_, a), _ ) -> a) |> debug (Debug.log "lazy vapors 2")
                , D.sig |> D.map (\_ -> Vapors [])
                ]
    in
    D.cell vapor
        (D.cell
            rec
            D.ignore
            |> debug (Debug.log "ignored vapors")
            |> D.map (\( v, _ ) -> v)
        )
        |> D.map (\( v, Vapors vs ) -> Vapors (v :: vs))


item =
    D.oneOf
        [ D.cell D.sig D.bigint |> D.map (\( _, a ) -> Just a)
        , D.sig |> D.map (\_ -> Nothing)
        ]
        |> D.map Item


vapor : Deconstructor Vapor
vapor =
    D.cell
        (D.bigint
            |> D.map (Bond << BigInt.toHexString)
        )
        (D.cell
            (D.cell
                item
                (D.oneOf
                    [ pack
                        |> D.map (\p -> ( Just p, Nothing ))
                    ]
                )
                |> debug (Debug.log "pack")
            )
            D.ignore
        )
        |> D.map (\( bo, ( ( it, pas ), _ ) ) -> Vapor ( Slip ( bo, it ), pas ))


tech : D.Deconstructor Tech
tech =
    let
        tas =
            D.oneOf
                [ D.cord |> D.map List.singleton
                , D.list D.cord
                ]
    in
    D.oneOf
        [ (D.cell (D.const D.cord "desks") <| (D.list tas |> D.map List.concat))
            |> D.map (\( _, a ) -> Desk a)
        , (D.cell (D.const D.cord "perm") <| D.list D.cord)
            |> D.map (\( _, a ) -> Perm a)
        , (D.cell (D.const D.cord "pipe") <| D.list D.cord)
            |> D.map (\( _, a ) -> Pipe a)
        , (D.cell
            (D.const D.cord "land")
           <|
            D.cell (D.cord |> D.map Just) (D.cord |> D.map Just)
          )
            |> D.map (\( _, ( a, b ) ) -> Land a b)
        ]


pack : D.Deconstructor Pack
pack =
    D.cell
        (D.tar
            |> D.map (Debug.log "urth")
        )
        (D.cell
            (D.cell
                (D.cell
                    (D.cell
                        (D.bigint |> D.map (BigInt.toHexString >> Bond))
                        item
                        |> D.map Slip
                    )
                    axal
                    |> D.map
                        (\x ->
                            case x of
                                ( s, a ) ->
                                    Mars s a
                        )
                )
                D.ignore
            )
            (D.bigint |> debug (Debug.log "host") |> D.map (\a -> (U.fromBigInt >> Maybe.map (toPatp >> Host)) a))
        )
        |> D.map
            (\( urth, ( ( mars, () ), ho ) ) ->
                case ho of
                    Just h ->
                        Pack urth mars h

                    Nothing ->
                        Pack urth mars (Host "0")
            )


axal_ =
    axal


axal =
    let
        rec =
            D.lazy (\_ -> axal_)

        dt =
            D.cell (D.cell D.cord (D.cell (D.cell D.sig tech) D.sig)) D.ignore
                |> D.map
                    (\( ( dir, ( ( (), fil ), () ) ), () ) ->
                        ( (), ( dir, fil ) )
                    )

        di =
            D.cell
                (D.cell D.ignore
                    (D.cell D.sig dt
                        |> D.map
                            (\( (), ( dir, fil ) ) -> ( dir, fil ))
                    )
                    |> D.map (\( (), a ) -> a)
                )
                rec
                |> D.map
                    (\a ->
                        case a of
                            ( ( (), ( s, t ) ), ax ) ->
                                ( Just t, Dir ( Just s, Just ax ) )
                    )
    in
    D.cell D.ignore
        (D.alt di (D.sig |> D.map (\_ -> ( Nothing, Dir ( Nothing, Nothing ) ))))
        |> D.map
            (\( (), ( a, b ) ) ->
                Axal ( a, b )
            )


update : Msg -> Model -> ( Model, Ur.Cmd.Cmd Msg )
update msg model =
    case msg of
        Toggle t show ->
            ( { model | showTech = toggle t model }, Ur.Cmd.none )

        GotState noun ->
            ( { model | state = Just noun }, Ur.Cmd.none )

        GotVapors noun ->
            ( { model | vapors = Just noun }, Ur.Cmd.none )

        GotAxal _ ->
            ( model, Ur.Cmd.none )

        Noop ->
            ( model, Ur.Cmd.none )

        Error err ->
            ( { model | error = err }, Ur.Cmd.none )

        RunCmd cmd ->
            ( model, cmd )

        GotShipName name ->
            ( { model | shipName = Just name }, Ur.Cmd.none )


toggle : TechType -> Model -> Dict String Bool
toggle t model =
    let
        sh =
            Dict.get (toString t) model.showTech
    in
    case sh of
        Nothing ->
            model.showTech

        Just b ->
            Dict.update (toString t) (Maybe.map not) model.showTech


showButton model ty =
    let
        show =
            Dict.get (toString ty) model.showTech |> Maybe.withDefault False

        toggleColor =
            if show then
                hue.blue

            else
                hue.lightBlue
    in
    Input.button
        [ padding 20
        , Background.color hue.blue
        , Border.width 2
        , Border.rounded 16
        , Border.color toggleColor
        , Border.shadow
            { offset = ( 0, 2 ), size = 0.2, blur = 10, color = toggleColor }
        , Font.color
            (if show then
                hue.darkCharcoal

             else
                hue.lightBlue
            )
        ]
        { onPress = Just (Toggle ty show)
        , label = text (toString ty)
        }


view : Model -> Document Msg
view model =
    { body =
        [ layout [ width fill, height fill, Background.color hue.white ] <|
            row [ width fill, height fill, padding 10, spacing 10 ]
                [ column
                    [ width <| px 150
                    , height fill
                    , Border.widthEach { right = 2, left = 0, top = 0, bottom = 0 }
                    , Border.color <| rgb255 0xE0 0xE0 0xE0
                    ]
                    [ el [ alignTop, height <| px 30 ] <| showButton model Desks
                    , el [ alignTop, height <| px 30 ] <| showButton model Perms
                    , el [ alignTop, height <| px 30 ] <| showButton model Pipes
                    ]
                , column [ width fill, height fill, spacing 20, scrollbarY, Font.size 16 ]
                    [ el [] (text model.error)
                    , row [ spacing 8 ]
                        [ Widget.textInput (Material.textInput Material.defaultPalette)
                            { chips = []
                            , text = model.newEntry
                            , placeholder = Nothing
                            , label = "New entry"
                            , onChange =
                                \_ ->
                                    Cmd.batch
                                        [ Ur.scry
                                            { url = url
                                            , path = [ "vapors" ]
                                            , agent = "make"
                                            , error = Error "Failed to refresh vapors"
                                            , success =
                                                vapors |> D.map GotVapors
                                            }
                                        ]
                                        |> Ur.Cmd.cmd
                                        |> RunCmd
                            }
                        , Widget.iconButton (Material.containedButton Material.defaultPalette)
                            { text = "submit"
                            , icon = Icon.elmHeroicons Heroicons.Outline.check
                            , onPress = Nothing
                            }
                        ]
                    , model.vapors
                        |> Maybe.map
                            (\v ->
                                v
                                    |> viewVapors model
                                    |> List.singleton
                                    |> column [ spacing 10 ]
                            )
                        |> Maybe.withDefault (text "No vapors")
                        |> List.singleton
                        |> column [ spacing 10 ]
                    ]
                ]
        ]
    , title = "Airlock"
    }


viewAxal : Model -> Axal -> Element msg
viewAxal model (Axal ( t, dir )) =
    let
        show =
            case t of
                Just (Desk _) ->
                    Dict.get "Desks" model.showTech |> Maybe.withDefault False

                Just (Perm _) ->
                    Dict.get "Perms" model.showTech |> Maybe.withDefault False

                Just (Pipe _) ->
                    Dict.get "Pipes" model.showTech |> Maybe.withDefault False

                _ ->
                    False

        print =
            case t of
                Just (Desk l) ->
                    [ text (String.join "\n" <| l)
                    ]
                        |> column [ moveRight 10, spacing 10 ]

                Just (Perm _) ->
                    text "perm"

                Just (Pipe _) ->
                    text "pipe"

                Just (Land _ _) ->
                    text "land"

                _ ->
                    none
    in
    column
        []
        [ viewDir model dir
        , if show then
            print

          else
            none
        ]


viewDir model (Dir ( dirInfo, ax )) =
    (case dirInfo of
        Nothing ->
            [ Maybe.withDefault none (ax |> Maybe.map (viewAxal model))
            ]

        Just dir ->
            [ text dir
            ]
                |> column
                    [ Border.width 2
                    , padding 4
                    ]
                |> List.singleton
    )
        |> row
            [ padding 10
            ]


viewVapors model (Vapors v) =
    v
        |> List.map (viewVapor model)
        |> column
            [ spacing 10, Background.color hue.darkCharcoal, padding 20, Border.rounded 16 ]


viewVapor model (Vapor ( slip, ( mPack, mString ) )) =
    case mPack of
        Just p ->
            column
                [ Background.color hue.green
                , spacing 10
                ]
                [ column
                    [ Border.width 10
                    , padding 4
                    , Font.bold
                    , Border.color (rgb 0 0 0)
                    , Background.color (rgb 0 0 0)
                    , Font.color (rgb 1 1 1)
                    , mouseOver
                        [ Font.color (rgb 0 0 0)
                        , Background.color (rgb 1 1 1)
                        ]
                    , moveRight 20
                    ]
                    [ viewHost model p.host
                    , viewSlip model slip
                    , viewAxal
                        model
                        p.mars.pace
                    ]
                ]

        Nothing ->
            column []
                [ text <| Maybe.withDefault "No pack information available" (Just <| toString mString)
                ]


viewHost model h =
    let
        bgColor =
            hue.lightGrey
    in
    row
        [ Background.color bgColor
        , Border.rounded 10
        , padding 10
        , alignLeft
        , Font.color (rgb255 0 0 0)
        , mouseOver
            [ Font.color
                (rgb 0.6 0.6 0.6)
            ]
        , Font.bold
        , Border.shadow { offset = ( 0, 5 ), size = 1, blur = 8, color = rgba255 0 0 0 0.5 }
        , spacing 15
        ]
        [ text h.host ]


viewSlip model (Slip ( bond, it )) =
    let
        headerAttrs =
            [ Font.bold
            , Font.color hue.blue
            , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
            , Border.color hue.lightGrey
            ]
    in
    row [ spacing 10, padding 10, Background.color hue.darkCharcoal, Border.rounded 16 ]
        [ table [ width shrink, spacing 10, padding 5 ]
            { data = [ { bond = bond.bond, item = Maybe.withDefault "" (it.item |> Maybe.map BigInt.toHexString) } ]
            , columns =
                [ { header = el headerAttrs <| text "bond"
                  , width = fillPortion 1
                  , view = .bond >> text >> el [ centerY ]
                  }
                , { header = el headerAttrs <| text "item"
                  , width = fill |> maximum 80
                  , view = .item >> text >> el [ width (fill |> maximum 50), centerY ]
                  }
                ]
            }
        ]


toString : a -> String
toString =
    Debug.toString


result : (a -> c) -> (b -> c) -> Result a b -> c
result f g res =
    case res of
        Ok b ->
            g b

        Err a ->
            f a


debug : (String -> String) -> D.Deconstructor a -> D.Deconstructor a
debug log f noun =
    case f noun of
        Just x ->
            let
                _ =
                    log (prettyNoun noun)
            in
            Just x

        Nothing ->
            let
                _ =
                    log (prettyNoun noun)
            in
            Nothing


prettyNoun : Noun -> String
prettyNoun noun =
    let
        go isRhs n =
            case n of
                Atom a ->
                    if isSig a then
                        "~"

                    else
                        "@"

                Cell ( lhs, rhs ) ->
                    if isRhs then
                        go False lhs ++ " " ++ go True rhs

                    else
                        "[" ++ go False lhs ++ " " ++ go True rhs ++ "]"
    in
    go False noun


hue =
    { blue = rgb255 0x72 0x9F 0xCF
    , darkCharcoal = rgb255 0x2E 0x34 0x36
    , green = rgb255 0x20 0xBF 0x55
    , lightBlue = rgb255 0xC5 0xE8 0xF7
    , lightGrey = rgb255 0xE0 0xE0 0xE0
    , darkGrey = rgb255 0x6D 0x6D 0x6D
    , orange = rgb255 0xF2 0x64 0x19
    , red = rgb255 0xAA 0x00 0x00
    , white = rgb255 0xFF 0xFF 0xFF
    }


port createEventSource : String -> Cmd msg


port onEventSourceMessage : (JD.Value -> msg) -> Sub msg
