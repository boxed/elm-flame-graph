module Main exposing (main)

import Browser
import FlameGraph exposing (StackFrame(..))
import Html exposing (Html, a, button, div, pre, text)
import Html.Attributes exposing (class, href, style)
import Html.Events exposing (onClick)

type alias Flags =
    { data : String
    , urlFormat : String
    }


main : Program Flags Model Msg
main =
    Browser.element
        { init = \flags -> ( { initialModel | frames = Just (FlameGraph.fromString flags.data), urlFormat = nonEmpty flags.urlFormat }, Cmd.none )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


nonEmpty : String -> Maybe String
nonEmpty s =
    if String.isEmpty s then
        Nothing

    else
        Just s


type alias Model =
    { frames : Maybe (List StackFrame)
    , selected : Maybe StackFrame
    , hovered : Maybe StackFrame
    , urlFormat : Maybe String
    }


initialModel : Model
initialModel =
    { frames = Nothing
    , selected = Nothing
    , hovered = Nothing
    , urlFormat = Nothing
    }


type Msg
    = SelectFrame StackFrame
    | ClearSelected
    | FrameHover StackFrame


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        SelectFrame frame ->
            ( { model | selected = Just frame }, Cmd.none )

        ClearSelected ->
            ( { model | selected = Nothing }, Cmd.none )

        FrameHover frame ->
            ( { model | hovered = Just frame }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


containerStyles : List (Html.Attribute a)
containerStyles =
    [ style "width" "100%"
    , style "font-family" "monospace"
    , class "flame-graph"
    ]


view : Model -> Html Msg
view model =
    let
        sumFrames : List StackFrame -> Int
        sumFrames =
            List.map
                (\(StackFrame { count }) -> count)
                >> List.sum

        totalSamples : Int
        totalSamples =
            model.frames |> Maybe.map sumFrames |> Maybe.withDefault 0
    in
    div containerStyles
        [ case model.selected of
            Just (StackFrame { name }) ->
                div [ style "padding" "0 12px" ]
                    [ button
                        [ style "margin-right" "12px"
                        , style "border" "1px solid #CCC"
                        , style "border-radius" "2px"
                        , style "cursor" "pointer"
                        , onClick ClearSelected
                        ]
                        [ text "reset zoom" ]
                    , case model.urlFormat |> Maybe.andThen (\fmt -> formatUrl fmt name) of
                        Just url ->
                            a [ href url ] [ text name ]

                        Nothing ->
                            text name
                    ]

            Nothing ->
                text ""
        , pre [ style "padding" "0 12px" ]
            [ case model.hovered of
                Just (StackFrame { name, count }) ->
                    let
                        label =
                            String.concat
                                [ name
                                , " ("
                                , String.fromInt count
                                , " sample"
                                , if count == 1 then
                                    ""

                                  else
                                    "s"
                                , ", "
                                , String.left 5 <| String.fromFloat (toFloat count / toFloat totalSamples * 100)
                                , "%)"
                                ]
                    in
                    case model.urlFormat |> Maybe.andThen (\fmt -> formatUrl fmt name) of
                        Just url ->
                            a [ href url ] [ text label ]

                        Nothing ->
                            text label

                Nothing ->
                    text ""
            ]
        , case ( model.selected, model.frames ) of
            ( Just selected, Just root ) ->
                FlameGraph.viewFromRoot
                    FrameHover
                    SelectFrame
                    selected
                    root

            ( _, Just root ) ->
                FlameGraph.view
                    FrameHover
                    SelectFrame
                    root

            _ ->
                text "Loading..."
        ]


formatUrl : String -> String -> Maybe String
formatUrl template name =
    let
        parsed =
            parseFrameName name
    in
    case parsed of
        Just { filename, lineno } ->
            Just
                (template
                    |> String.replace "{filename}" filename
                    |> String.replace "{lineno}" lineno
                    |> String.replace "{name}" name
                )

        Nothing ->
            Nothing


parseFrameName : String -> Maybe { filename : String, lineno : String }
parseFrameName name =
    let
        trimmed =
            String.trim name
    in
    if String.endsWith ")" trimmed then
        -- Try "func (file:line)" pattern
        case lastSplit "(" trimmed of
            Just inside ->
                parseFileLineno (String.dropRight 1 inside)

            Nothing ->
                Nothing

    else
        parseFileLineno trimmed


parseFileLineno : String -> Maybe { filename : String, lineno : String }
parseFileLineno s =
    case lastSplit ":" s of
        Just lineno ->
            if String.all Char.isDigit lineno && not (String.isEmpty lineno) then
                Just { filename = String.dropRight (String.length lineno + 1) s, lineno = lineno }

            else
                Nothing

        Nothing ->
            Nothing


lastSplit : String -> String -> Maybe String
lastSplit sep s =
    case String.indexes sep s of
        [] ->
            Nothing

        indexes ->
            indexes
                |> List.reverse
                |> List.head
                |> Maybe.map (\i -> String.dropLeft (i + String.length sep) s)
