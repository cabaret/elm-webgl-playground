module Main exposing (..)

import Html exposing (Html, div, button, text, input, label, br)
import Html.Attributes as Attributes exposing (style, width, height, type_, value)
import Html.Events exposing (onInput)
import WebGL exposing (Shader, Mesh, Entity, entity)
import Math.Vector2 exposing (Vec2, vec2)
import Math.Vector3 exposing (Vec3, vec3)


canvasWidth : Int
canvasWidth =
    1680


canvasHeight : Int
canvasHeight =
    880


resolution : Vec2
resolution =
    vec2 (toFloat canvasWidth) (toFloat canvasHeight)


type Msg
    = UpdateCoordinate Coordinate String


type Coordinate
    = X
    | Y


type alias Vertex =
    { a_position : Vec2
    , a_color : Vec3
    }


type alias Uniforms =
    { u_resolution : Vec2
    , u_translation : Vec2
    }


type alias Varyings =
    { v_vColor : Vec3 }


type alias Point =
    ( Float, Float )


type alias Color =
    Vec3


type alias Model =
    { translation :
        { x : Float
        , y : Float
        }
    }


initialModel : Model
initialModel =
    { translation =
        { x = 0
        , y = 0
        }
    }


strToFloat : String -> Float
strToFloat str =
    case String.toFloat str of
        Ok f ->
            f

        Err _ ->
            0


round_ : Float -> Float
round_ =
    truncate >> toFloat


vertexShader : Shader Vertex Uniforms Varyings
vertexShader =
    [glsl|
      precision mediump float;
      attribute vec2 a_position;
      attribute vec3 a_color;
      uniform vec2 u_resolution;
      uniform vec2 u_translation;
      varying vec3 v_vColor;
      void main() {
        vec2 position = a_position + u_translation;
        vec2 zeroToOne = position / u_resolution;
        vec2 zeroToTwo = zeroToOne * 2.0;
        vec2 clipSpace = zeroToTwo - 1.0;

        gl_Position = vec4(clipSpace * vec2(1, -1), 0, 1);

        v_vColor = a_color;
      }
  |]


fragmentShader : Shader {} Uniforms Varyings
fragmentShader =
    [glsl|
      precision mediump float;
      varying vec3 v_vColor;
      void main() {
        gl_FragColor = vec4(v_vColor, 1);
      }
    |]


vertexWithColor : Color -> Vec2 -> Vertex
vertexWithColor =
    flip Vertex


shapeMesh : Mesh Vertex
shapeMesh =
    let
        withColor =
            vertexWithColor (vec3 1 0 0)
    in
        WebGL.triangles
            [ ( withColor (vec2 0 0)
              , withColor (vec2 200 0)
              , withColor (vec2 0 40)
              )
            , ( withColor (vec2 0 40)
              , withColor (vec2 200 0)
              , withColor (vec2 200 40)
              )
            , ( withColor (vec2 0 100)
              , withColor (vec2 150 100)
              , withColor (vec2 0 140)
              )
            , ( withColor (vec2 0 140)
              , withColor (vec2 150 100)
              , withColor (vec2 150 140)
              )
            , ( withColor (vec2 0 0)
              , withColor (vec2 40 0)
              , withColor (vec2 0 300)
              )
            , ( withColor (vec2 0 300)
              , withColor (vec2 40 0)
              , withColor (vec2 40 300)
              )
            ]


shapeEntity : Model -> Entity
shapeEntity model =
    entity
        vertexShader
        fragmentShader
        shapeMesh
        { u_resolution = resolution
        , u_translation = vec2 model.translation.x model.translation.y
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateCoordinate coord str ->
            let
                translation =
                    case coord of
                        X ->
                            { x = strToFloat str
                            , y = model.translation.y
                            }

                        Y ->
                            { x = model.translation.x
                            , y = strToFloat str
                            }
            in
                { model | translation = translation } ! []


sliderView : Float -> Coordinate -> Html Msg
sliderView coordValue coord =
    let
        maxValue =
            case coord of
                X ->
                    canvasWidth

                Y ->
                    canvasHeight
    in
        label []
            [ text (toString coord)
            , input
                [ type_ "range"
                , value (toString coordValue)
                , Attributes.min "0"
                , Attributes.max (toString maxValue)
                , onInput (UpdateCoordinate coord)
                ]
                []
            , text (toString coordValue)
            ]


view : Model -> Html Msg
view model =
    let
        buttonStyle extraStyle =
            style
                (List.append
                    [ ( "display", "inline-block" )
                    , ( "padding", "20px" )
                    , ( "border", "2px solid transparent" )
                    , ( "margin-right", "20px" )
                    , ( "text-transform", "uppercase" )
                    , ( "font-weight", "bold" )
                    , ( "font-size", "18px" )
                    , ( "outline", "none" )
                    , ( "cursor", "pointer" )
                    ]
                    extraStyle
                )
    in
        div []
            [ div
                [ style
                    [ ( "position", "absolute" )
                    , ( "top", "0px" )
                    , ( "left", "0px" )
                    ]
                ]
                [ sliderView model.translation.x X
                , br [] []
                , sliderView model.translation.y Y
                ]
            , WebGL.toHtml
                [ width canvasWidth
                , height canvasHeight
                , style [ ( "display", "block" ) ]
                ]
                [ shapeEntity model ]
            ]


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }
