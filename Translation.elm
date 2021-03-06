module Main exposing (..)

import Html exposing (Html, Attribute, div, button, text, input, label, br, span)
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
    | UpdateAngle String
    | UpdateScale Coordinate String


type Coordinate
    = X
    | Y


type alias SliderConfig =
    { val : Float
    , min : Float
    , max : Float
    , step : Float
    , lbl : String
    , msg : Attribute Msg
    }


type alias Vertex =
    { a_position : Vec2
    , a_color : Vec3
    }


type alias Uniforms =
    { u_resolution : Vec2
    , u_translation : Vec2
    , u_rotation : Vec2
    , u_scale : Vec2
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
    , angle : Float
    , scale :
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
    , angle = 0
    , scale =
        { x = 1
        , y = 1
        }
    }


strToFloat : String -> Float
strToFloat str =
    String.toFloat str
        |> Result.withDefault 0


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
      uniform vec2 u_rotation;
      uniform vec2 u_scale;
      varying vec3 v_vColor;

      void main() {
        vec2 scaledPosition = a_position * u_scale;
        // https://webglfundamentals.org/webgl/lessons/webgl-2d-rotation.html
        vec2 rotatedPosition = vec2(
           scaledPosition.x * u_rotation.y + scaledPosition.y * u_rotation.x,
           scaledPosition.y * u_rotation.y - scaledPosition.x * u_rotation.x);

        vec2 position = rotatedPosition + u_translation;
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
shapeEntity { translation, angle, scale } =
    let
        angleInRadians =
            degrees angle
    in
        entity
            vertexShader
            fragmentShader
            shapeMesh
            (Uniforms
                resolution
                (vec2 translation.x translation.y)
                (vec2 (sin angleInRadians) (cos angleInRadians))
                (vec2 scale.x scale.y)
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateAngle str ->
            { model | angle = strToFloat str } ! []

        UpdateScale coord str ->
            let
                scale =
                    case coord of
                        X ->
                            { x = strToFloat str
                            , y = model.scale.y
                            }

                        Y ->
                            { x = model.scale.x
                            , y = strToFloat str
                            }
            in
                { model | scale = scale } ! []

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


angleSlider : Float -> Html Msg
angleSlider angle =
    genericSlider
        { val = angle
        , min = -360
        , max = 360
        , step = 1
        , lbl = "angle"
        , msg = (onInput (UpdateAngle))
        }


scaleSlider : Float -> Coordinate -> Html Msg
scaleSlider coordValue coord =
    genericSlider
        { val = coordValue
        , min = 0
        , max = 5
        , step = 0.01
        , lbl = ("scale" ++ (toString coord))
        , msg = (onInput (UpdateScale coord))
        }


translationSlider : Float -> Coordinate -> Html Msg
translationSlider coordValue coord =
    let
        maxValue =
            case coord of
                X ->
                    toFloat canvasWidth

                Y ->
                    toFloat canvasHeight
    in
        genericSlider
            { val = coordValue
            , min = 0
            , max = maxValue
            , step = 1
            , lbl = (toString coord)
            , msg = (onInput (UpdateCoordinate coord))
            }


genericSlider : SliderConfig -> Html Msg
genericSlider { val, min, max, step, lbl, msg } =
    label []
        [ span
            [ style
                [ ( "width", "60px" )
                , ( "display", "inline-block" )
                ]
            ]
            [ text lbl ]
        , input
            [ type_ "range"
            , value (toString val)
            , Attributes.min (toString min)
            , Attributes.max (toString max)
            , Attributes.step (toString step)
            , msg
            ]
            []
        , text (" " ++ (toString val))
        ]


controlsView : Model -> Html Msg
controlsView model =
    div
        [ style
            [ ( "position", "absolute" )
            , ( "top", "0px" )
            , ( "right", "0px" )
            , ( "width", "275px" )
            , ( "padding", "20px" )
            , ( "background-color", "hotpink" )
            ]
        ]
        [ translationSlider model.translation.x X
        , br [] []
        , translationSlider model.translation.y Y
        , br [] []
        , angleSlider model.angle
        , br [] []
        , scaleSlider model.scale.x X
        , br [] []
        , scaleSlider model.scale.y Y
        ]


view : Model -> Html Msg
view model =
    div []
        [ controlsView model
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
