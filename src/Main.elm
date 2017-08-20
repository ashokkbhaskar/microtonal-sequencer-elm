port module Main exposing (..)

import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Array exposing (Array, get, set, length)
import Dict exposing (Dict)
import Time exposing (Time, minute)
import Html.Events exposing (onClick, onInput, on)
import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder)
import Svg exposing (Svg, svg, circle)
import Svg.Attributes as SvgAttr exposing (viewBox, cx, cy, r, fill, visibility)


main = 
    program 
        { init = init 
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


--MODEL

type alias Model = 
    { notes : Dict String (Array Note)
    , scale : Scale
    , sequenceLength : Int
    , currentIndex : Int
    , bpm : Int
    , isPlaying : Bool
    }

type Note
    = Degree Int
    | Tie 
    | Off

type alias Scale =
    { baseFrequency : Float
    , scalePattern : ScalePattern
    }

type ScalePattern
    = Edo Int --EDO means Equal Division of the Octave

init : (Model, Cmd Msg)
init =
    { notes = initialNotes
    , scale = initialScale
    , sequenceLength = initialSeqLength
    , currentIndex = 0
    , bpm = initialBpm
    , isPlaying = False
    } ! []

initialNotes : Dict String (Array Note)
initialNotes =
    let
        emptyTrack = Array.initialize c_MAX_SEQ_LENGTH (\_ -> Tie)
    in
        Dict.fromList
            [ (c_TRACK_0_ID, emptyTrack)
            , (c_TRACK_1_ID, emptyTrack)
            , (c_TRACK_2_ID, emptyTrack)
            , (c_TRACK_3_ID, emptyTrack)
            , (c_TRACK_4_ID, emptyTrack)
            , (c_TRACK_5_ID, emptyTrack)
            ]
c_TRACK_0_ID = "track-0"
c_TRACK_1_ID = "track-1"
c_TRACK_2_ID = "track-2"
c_TRACK_3_ID = "track-3"
c_TRACK_4_ID = "track-4"
c_TRACK_5_ID = "track-5"

initialScale : Scale
initialScale =
    { baseFrequency = 261.63
    , scalePattern = Edo 12
    }
c_MIN_EDO : Int
c_MIN_EDO = 1
c_MAX_EDO : Int
c_MAX_EDO = 999

initialSeqLength : Int
initialSeqLength = 4
c_MIN_SEQ_LENGTH : Int
c_MIN_SEQ_LENGTH = 1
c_MAX_SEQ_LENGTH : Int
c_MAX_SEQ_LENGTH = 16

initialBpm : Int
initialBpm = 100
c_MIN_BPM : Int
c_MIN_BPM = 20
c_MAX_BPM : Int
c_MAX_BPM = 400

c_STEP_INDEX_PROP_NAME = "stepIndex"
c_TRACK_ID_PROP_NAME = "trackId"

c_MIN_OUTPUT_FREQUENCY : Float
c_MIN_OUTPUT_FREQUENCY = 20.0
c_MAX_OUTPUT_FREQUENCY : Float
c_MAX_OUTPUT_FREQUENCY = 20000.0

c_NOTE_INPUT_WIDTH_PX : Int
c_NOTE_INPUT_WIDTH_PX = 45

--UPDATE

type Msg
    = Step Time
    | Toggle
    | Stop
    | ChangeBpm String 
    | ChangeSeqLength String
    | ChangeEdo String
    | ChangeNote String Int String --track ID, step index, note input text

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Step _ ->
            let 
                newIndex =
                    if (model.currentIndex + 1) < model.sequenceLength then
                        (model.currentIndex + 1)
                    else
                        0
            in
                { model | currentIndex = newIndex } !
                    playNotes model.scale newIndex model.notes
        Toggle ->
            let
                cmdList =
                    if model.isPlaying then
                        [ stopPlayback ]
                    else
                        playNotes model.scale model.currentIndex model.notes
            in 
                { model | isPlaying = (not model.isPlaying) } ! cmdList
        Stop ->
            { model | currentIndex = 0, isPlaying = False } ! [ stopPlayback ]
        ChangeBpm newBpmString ->  --need to enforce range validation here
            case String.toInt newBpmString of 
                Ok newBpm ->
                    if c_MIN_BPM <= newBpm && newBpm <= c_MAX_BPM then
                        { model | bpm = newBpm } ! []
                    else
                        model ! []
                Err _ ->
                    model ! []
        ChangeSeqLength newSeqLengthString ->  --see comment for ChangeBpm
            case String.toInt newSeqLengthString of 
                Ok newSeqLength ->
                    if c_MIN_SEQ_LENGTH <= newSeqLength &&
                        newSeqLength <= c_MAX_SEQ_LENGTH then
                            { model | sequenceLength = newSeqLength } ! []
                    else
                        model ! []
                Err _ ->
                    model ! []
        ChangeEdo newEdoString ->  --see comment for ChangeBpm
            case String.toInt newEdoString of 
                Ok newEdo ->
                    if c_MIN_EDO <= newEdo && newEdo <= c_MAX_EDO then
                        let currentScale = model.scale
                            newScale = { currentScale | scalePattern = Edo newEdo}
                        in
                            { model | scale = newScale} ! []
                    else
                        model ! []
                Err _ ->
                    model ! []
        ChangeNote trackId stepIndex noteString ->
            case Dict.get trackId model.notes of
                Nothing ->
                    model ! []
                Just track ->
                    if 0 <= stepIndex && stepIndex < (length track) then
                        let 
                            newNote = 
                                case String.toInt noteString of 
                                    Ok newDegree ->
                                        Degree newDegree
                                    Err _ ->
                                        if String.isEmpty noteString then
                                            Tie
                                        else
                                            Off
                            newTrack =
                                set stepIndex newNote track
                        in
                            { model | notes = Dict.insert trackId newTrack 
                                model.notes } ! []
                    else
                        model ! []


--COMMANDS AND PORTS

port toJs_stopPlayback : () -> Cmd msg

port toJs_startOscillator : (String, Float) -> Cmd msg --track id, frequency

port toJs_stopOscillator : String -> Cmd msg --track id

stopPlayback : Cmd msg
stopPlayback = 
    toJs_stopPlayback ()

playNotes : Scale -> Int -> Dict String (Array Note) -> List (Cmd msg)
playNotes scale stepIndex notes =
    List.map (playNote scale stepIndex) (Dict.toList notes)

playNote : Scale -> Int -> (String, Array Note) -> Cmd msg
playNote scale stepIndex (trackId, noteArray) =
    case (Array.get stepIndex noteArray) of
        Just note ->
            case note of
                Degree degree ->
                    let frequency = degreeToFrequency scale degree
                    in
                        if c_MIN_OUTPUT_FREQUENCY <= frequency &&
                            frequency <= c_MAX_OUTPUT_FREQUENCY then
                                toJs_startOscillator (trackId, frequency)
                        else
                            toJs_stopOscillator trackId --stop oscillator if freq is out of range
                Tie ->
                    Cmd.none
                Off ->
                    toJs_stopOscillator trackId
        Nothing ->
            Cmd.none


--NOTE MATH

degreeToFrequency : Scale -> Int -> Float
degreeToFrequency scale degree =
    case scale.scalePattern of
        Edo intervalsPerOctave ->
            let
                intervalsFloat = toFloat intervalsPerOctave
                degreeFloat = toFloat degree
            in
                scale.baseFrequency * ( 2 ^ ( (1/intervalsFloat) * degreeFloat ) )


--SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    if model.isPlaying then
        Time.every (minute / (toFloat model.bpm)) Step
    else
        Sub.none


--VIEW

view : Model -> Html Msg
view model =
    div []
        [ button
            [ style
                [ ("width", "80px")
                ]
            , onClick Toggle
            ]
            [text <| if model.isPlaying then "Pause" else "Play"]
        , button [onClick Stop] [text "Stop"] 
        , bpmHtmlElement
        , sequenceLengthHtmlElement
        , scaleHtmlElement
        , positionLampLine model.sequenceLength model.currentIndex c_MAX_SEQ_LENGTH
        , trackHtmlElement c_TRACK_0_ID c_MAX_SEQ_LENGTH
        , trackHtmlElement c_TRACK_1_ID c_MAX_SEQ_LENGTH
        , trackHtmlElement c_TRACK_2_ID c_MAX_SEQ_LENGTH
        , trackHtmlElement c_TRACK_3_ID c_MAX_SEQ_LENGTH
        , trackHtmlElement c_TRACK_4_ID c_MAX_SEQ_LENGTH
        , trackHtmlElement c_TRACK_5_ID c_MAX_SEQ_LENGTH
        , p []
            [ text <|
                "Positive and negative integers represent scale steps above and"
                    ++ " below the base frequency, respectively. Blanks represent"
                    ++ " ties (keep playing the previous note). Anything else"
                    ++ " (including numbers with decimal points) stops the previous"
                    ++ " note."
            ]
        ]

bpmHtmlElement : Html Msg
bpmHtmlElement =
    div []
        [ span [] [text "BPM: "] 
        , input --These attributes don't seem to prevent onInput from being called with
        --incorrect payload strings, but they seem to provide some visual feedback
        --(possibly browser-dependent - Chrome might prevent unwanted calls,
        --but Firefox doesn't seem to)
            [ type_ "number"
            , defaultValue <| toString initialBpm
            , Attr.min <| toString c_MIN_BPM
            , Attr.max <| toString c_MAX_BPM
            , title <| "Enter an integer tempo between "
                ++ toString c_MIN_BPM
                ++ " and "
                ++ toString c_MAX_BPM
                ++ " beats per minute (no decimal points)"
            , style 
                [ ("width", "45px")
                ]
            , onInput ChangeBpm
            ] []
        ]

sequenceLengthHtmlElement : Html Msg
sequenceLengthHtmlElement =
    div []
        [ span [] [text "Number of Steps: "]
        , input --see comment for bpmHtmlElement
            [ type_ "number"
            , defaultValue <| toString initialSeqLength
            , Attr.min <| toString c_MIN_SEQ_LENGTH
            , Attr.max <| toString c_MAX_SEQ_LENGTH
            , title <| "Enter a sequence length between "
                ++ toString c_MIN_SEQ_LENGTH
                ++ " and "
                ++ toString c_MAX_SEQ_LENGTH
                ++ " steps (no decimal points)"
            , style 
                [ ("width", "45px")
                ]
            , onInput ChangeSeqLength
            ] []
        ]

scaleHtmlElement : Html Msg
scaleHtmlElement =
    div []
        [ span [] [text "Scale: "]
        , input --see comment for bpmHtmlElement
            [ type_ "number"
            , defaultValue <| 
                case initialScale.scalePattern of
                    Edo numDivisions ->
                        toString numDivisions
            , Attr.min <| toString c_MIN_EDO
            , Attr.max <| toString c_MAX_EDO
            , title <| "Enter the number of scale degrees per octave between "
                ++ toString c_MIN_EDO
                ++ " and "
                ++ toString c_MAX_EDO
                ++ " as an integer (no decimal points)"
            , style 
                [ ("width", "45px")
                ]
            , onInput ChangeEdo
            ] []
        , span [] [text "Equal divisons of the octave"]
        ]

trackHtmlElement : String -> Int -> Html Msg
trackHtmlElement trackId maxSeqLength =
    div [] <| noteInputList trackId maxSeqLength

noteInputHtmlElement : String -> Int -> Note -> Html Msg
noteInputHtmlElement trackId index initialNote =
    input 
        [ defaultValue <|
            case initialNote of
                Degree degree ->
                    toString degree
                Tie ->
                    ""
                Off ->
                    "x"
        , style 
            [ ("width", toString c_NOTE_INPUT_WIDTH_PX ++ "px")
            , ("box-sizing", "border-box")
            ]
        , property c_TRACK_ID_PROP_NAME <| Encode.string trackId
        , property c_STEP_INDEX_PROP_NAME <| Encode.int index
        , onNoteInput ChangeNote
        ] []

noteInputList : String -> Int -> List (Html Msg)
noteInputList trackId maxSeqLength =
    List.map (noteInput trackId) (List.range 0 <| maxSeqLength - 1)

noteInput : String -> Int -> Html Msg
noteInput trackId index =
    let
        initialNotesOfTrack = Dict.get trackId initialNotes
        initialNote =
            case initialNotesOfTrack of
                Just initialNoteArray -> 
                    Array.get index initialNoteArray |> Maybe.withDefault Tie
                Nothing ->
                    Tie
    in
        noteInputHtmlElement trackId index initialNote

onNoteInput : (String -> Int -> String -> msg) -> Attribute msg
onNoteInput tagger =
    on "input" (noteInputDecoder tagger)

noteInputDecoder : (String -> Int -> String -> msg) -> Decoder msg
noteInputDecoder tagger =
    Decode.map3 tagger
        (Decode.at ["target", c_TRACK_ID_PROP_NAME]   Decode.string)
        (Decode.at ["target", c_STEP_INDEX_PROP_NAME] Decode.int)
        (Decode.at ["target", "value"]                Decode.string)

positionLampLine : Int -> Int -> Int -> Html msg
positionLampLine sequenceLength currentIndex maxSeqLength =
    svg [ SvgAttr.width <|
            toString (maxSeqLength * c_NOTE_INPUT_WIDTH_PX) ++ "px"
        , SvgAttr.height "10px"
        --, viewBox <| "0 0 "
        --    ++ toString (c_MAX_SEQ_LENGTH * c_NOTE_INPUT_WIDTH_PX)
        --    ++ " 10"
        ]
        --[ circle [ cx "50", cy "50", r "45", fill "#0B79CE" ] []
        --, circle [ cx "50", cy "50", r "45", fill "#0B79CE" ] []
        --]
        (positionLampList sequenceLength currentIndex maxSeqLength)

positionLampList : Int -> Int -> Int -> List (Svg msg)
positionLampList sequenceLength currentIndex maxSeqLength =
    List.map (positionLamp sequenceLength currentIndex) (List.range 0 <| maxSeqLength - 1)

positionLamp : Int -> Int -> Int -> Svg msg
positionLamp sequenceLength currentIndex index =
    let xPositionInt = 
        (c_NOTE_INPUT_WIDTH_PX * index) + round (toFloat c_NOTE_INPUT_WIDTH_PX / 2)
    in
        circle
            [ cx <| toString xPositionInt
            , cy "5"
            , r "5"
            , fill (if index == currentIndex then "#FF0000" else "#600000")
            , visibility (if index < sequenceLength then "visible" else "hidden")
            ] []