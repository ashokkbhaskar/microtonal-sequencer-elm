port module Main exposing (..)

import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Array exposing (Array, get, set, length)
import Time exposing (Time, minute)
import Html.Events exposing (onClick, onInput, on)
import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder)


main = 
    program 
        { init = init 
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


--MODEL

type alias Model = 
    { notes : Array Note
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

initialNotes : Array Note
initialNotes = Array.initialize c_MAX_SEQ_LENGTH (\index -> Degree index)

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

c_STEP_INDEX_PROP_NAME : String
c_STEP_INDEX_PROP_NAME = "stepIndex"

c_MIN_OUTPUT_FREQUENCY : Float
c_MIN_OUTPUT_FREQUENCY = 20.0
c_MAX_OUTPUT_FREQUENCY : Float
c_MAX_OUTPUT_FREQUENCY = 20000.0

--UPDATE

type Msg
    = Step Time
    | Toggle
    | Stop
    | ChangeBpm String 
    | ChangeSeqLength String
    | ChangeEdo String
    | ChangeNote Int String --step index, note input text

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
                    [ playNote model.scale (get newIndex model.notes) ]
        Toggle ->
            { model | isPlaying = (not model.isPlaying) } !
                [ 
                    if model.isPlaying then
                        stopPlayback
                    else
                        playNote model.scale (get model.currentIndex model.notes) 
                ]
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
        ChangeNote stepIndex noteString ->
            if 0 <= stepIndex && stepIndex < (length model.notes) then
                case String.toInt noteString of 
                    Ok newDegree ->
                        { model | notes = set stepIndex (Degree newDegree)
                            model.notes } ! []
                    Err _ ->
                        if String.isEmpty noteString then
                            { model | notes = set stepIndex Tie 
                                model.notes } ! []
                        else
                            { model | notes = set stepIndex Off
                                model.notes } ! []
            else
                model ! []


--COMMANDS AND PORTS

port toJs_stopPlayback : () -> Cmd msg

port toJs_playFrequency : Float -> Cmd msg

stopPlayback : Cmd msg
stopPlayback = 
    toJs_stopPlayback ()

playNote : Scale -> Maybe Note -> Cmd msg
playNote scale maybeNote =
    case maybeNote of
        Just note ->
            case note of
                Degree degree ->
                    let frequency = degreeToFrequency scale degree
                    in
                        if c_MIN_OUTPUT_FREQUENCY <= frequency &&
                            frequency <= c_MAX_OUTPUT_FREQUENCY then
                                toJs_playFrequency frequency
                        else
                            Cmd.none
                Tie ->
                    Cmd.none
                Off ->
                    toJs_stopPlayback ()
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
        , trackHtmlElement c_MAX_SEQ_LENGTH
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

trackHtmlElement : Int -> Html Msg
trackHtmlElement maxSeqLength =
    div [] <| noteInputList maxSeqLength

noteInputHtmlElement : Int -> Note -> Html Msg
noteInputHtmlElement index initialNote =
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
            [ ("width", "40px")
            ]
        , property c_STEP_INDEX_PROP_NAME <| Encode.int index
        , onNoteInput ChangeNote
        ] []

noteInputList : Int -> List (Html Msg)
noteInputList maxSeqLength =
    List.map noteInput (List.range 0 <| maxSeqLength - 1)

noteInput : Int -> Html Msg
noteInput index =
    noteInputHtmlElement index (get index initialNotes |> Maybe.withDefault Tie)

onNoteInput : (Int -> String -> msg) -> Attribute msg
onNoteInput tagger =
    on "input" (noteInputDecoder tagger)

noteInputDecoder : (Int -> String -> msg) -> Decoder msg
noteInputDecoder tagger =
    Decode.map2 tagger
        (Decode.at ["target", c_STEP_INDEX_PROP_NAME] Decode.int)
        (Decode.at ["target", "value"]                Decode.string)