module Main exposing (..)

import Html exposing (Html, div, button, text, input, label)
import Html.Attributes as Attributes exposing (style, width, height, type_, value)
import Html.Events exposing (onClick, onInput)
import WebGL exposing (Shader, Mesh, Entity, entity)
import Math.Vector2 exposing (Vec2, vec2)
import Math.Vector3 exposing (Vec3, vec3)
import Random exposing (Generator, list, pair, float, int)


objectCount : Int
objectCount =
    1


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
    = RenderObjects
    | SetObject Square
    | UpdateCoordinate Coordinate String


type Coordinate
    = X
    | Y


type alias Square =
    { base : Point
    , width : Float
    , height : Float
    , color : Color
    }


type alias Vertex =
    { position : Vec2
    , color : Vec3
    }


type alias Uniforms =
    { resolution : Vec2 }


type alias Point =
    ( Float, Float )


type alias Color =
    Vec3


type alias Model =
    { object : Square
    , position :
        { x : Float
        , y : Float
        }
    }


initialModel : Model
initialModel =
    Model
        (Square ( 0, 0 ) 0 0 (vec3 0 0 0))
        { x = 0
        , y = 0
        }


colorValueGenerator : Generator Float
colorValueGenerator =
    float 0.5 1


colorGenerator : Generator Color
colorGenerator =
    Random.map3 vec3 colorValueGenerator colorValueGenerator colorValueGenerator


positionGenerator : Float -> Generator Float
positionGenerator =
    float 0


pointGenerator : Float -> Float -> Generator Point
pointGenerator width height =
    Random.map2 (,)
        (positionGenerator width)
        (positionGenerator height)


squareGenerator : Float -> Float -> Generator Square
squareGenerator width height =
    Random.map4 Square
        (pointGenerator width height)
        (positionGenerator (width / 2))
        (positionGenerator (height / 2))
        colorGenerator


objectGenerator : Generator Square
objectGenerator =
    let
        w =
            (toFloat canvasWidth)

        h =
            (toFloat canvasHeight)
    in
        (squareGenerator w h)


strToFloat : String -> Float
strToFloat str =
    case String.toFloat str of
        Ok v ->
            v

        Err _ ->
            0


round_ : Float -> Float
round_ =
    truncate >> toFloat


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateCoordinate coord str ->
            let
                newPosition =
                    case coord of
                        X ->
                            { x = strToFloat str
                            , y = model.position.y
                            }

                        Y ->
                            { x = model.position.x
                            , y = strToFloat str
                            }
            in
                { model | position = newPosition } ! []

        SetObject object ->
            let
                ( x, y ) =
                    object.base
            in
                { model
                    | position = { x = round_ x, y = round_ y }
                    , object = object
                }
                    ! []

        RenderObjects ->
            model ! [ generateObjects ]


vertexShader : Shader Vertex Uniforms { vColor : Vec3 }
vertexShader =
    [glsl|
      precision mediump float;
      attribute vec2 position;
      attribute vec3 color;
      uniform vec2 resolution;
      varying vec3 vColor;
      void main() {
        vec2 zeroToOne = position / resolution;
        vec2 zeroToTwo = zeroToOne * 2.0;
        vec2 clipSpace = zeroToTwo - 1.0;

        gl_Position = vec4(clipSpace * vec2(1, -1), 0, 1);

        vColor = color;
      }
  |]


fragmentShader : Shader {} Uniforms { vColor : Vec3 }
fragmentShader =
    [glsl|
      precision mediump float;
      varying vec3 vColor;
      void main() {
        gl_FragColor = vec4(vColor, 1);
      }
    |]


vertexWithColor : Color -> Vec2 -> Vertex
vertexWithColor =
    flip Vertex


shapeMesh : Square -> Float -> Float -> Mesh Vertex
shapeMesh shape x y =
    let
        withColor =
            vertexWithColor shape.color

        mesh =
            let
                x2 =
                    x + shape.width

                y2 =
                    y + shape.height
            in
                [ ( withColor (vec2 x y)
                  , withColor (vec2 x2 y)
                  , withColor (vec2 x y2)
                  )
                , ( withColor (vec2 x y2)
                  , withColor (vec2 x2 y)
                  , withColor (vec2 x2 y2)
                  )
                ]
    in
        WebGL.triangles mesh


shapeEntity : Square -> Model -> Entity
shapeEntity object { position } =
    entity
        vertexShader
        fragmentShader
        (shapeMesh object position.x position.y)
        { resolution = resolution
        }


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
                [ label []
                    [ text "x"
                    , input
                        [ type_ "range"
                        , value (toString model.position.x)
                        , Attributes.min "0"
                        , Attributes.max (toString canvasWidth)
                        , onInput (UpdateCoordinate X)
                        ]
                        []
                    , text (toString model.position.x)
                    ]
                , label []
                    [ text "y"
                    , input
                        [ type_ "range"
                        , value (toString model.position.y)
                        , Attributes.min "0"
                        , Attributes.max (toString canvasHeight)
                        , onInput (UpdateCoordinate Y)
                        ]
                        []
                    , text (toString model.position.y)
                    ]
                ]
            , WebGL.toHtml
                [ width canvasWidth
                , height canvasHeight
                , style [ ( "display", "block" ) ]
                ]
                [ shapeEntity model.object model ]
            ]


generateObjects : Cmd Msg
generateObjects =
    Random.generate SetObject objectGenerator


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, generateObjects )
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }
