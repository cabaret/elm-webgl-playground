module Main exposing (..)

import Html exposing (Html, div, button, text)
import Html.Attributes exposing (style, width, height)
import Html.Events exposing (onClick)
import WebGL exposing (Shader, Mesh, Entity, entity)
import Math.Vector2 exposing (Vec2, vec2)
import Math.Vector3 exposing (Vec3, vec3)
import Random exposing (Generator, list, pair, float, int)
import Random.Extra as RandomE


objectCount : Int
objectCount =
    50


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
    = RenderObjects ObjectType
    | SetObjects (List Object)


type ObjectType
    = Mixed
    | Triangles
    | Squares


type Shape
    = Triangle Point Point Point
    | Square Point Float Float


type alias Object =
    { shape : Shape
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
    { objects : List Object
    , objectType : ObjectType
    }


initialModel : Model
initialModel =
    Model [] Mixed


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


triangleVertices : Float -> Float -> Generator ( Point, Point, Point )
triangleVertices width height =
    Random.map3 (,,)
        (pointGenerator width height)
        (pointGenerator width height)
        (pointGenerator width height)


triangleGenerator : Float -> Float -> Generator Shape
triangleGenerator width height =
    Random.map3 Triangle
        (pointGenerator width height)
        (pointGenerator width height)
        (pointGenerator width height)


squareBase : Float -> Float -> Generator Point
squareBase width height =
    (pointGenerator width height)


squareGenerator : Float -> Float -> Generator Shape
squareGenerator width height =
    Random.map3 Square
        (squareBase width height)
        (positionGenerator (width / 2))
        (positionGenerator (height / 2))


objectGenerator : ObjectType -> Generator Object
objectGenerator objectType =
    let
        w =
            (toFloat canvasWidth)

        h =
            (toFloat canvasHeight)

        shapeGenerator =
            case objectType of
                Mixed ->
                    (RandomE.choices
                        [ squareGenerator w h
                        , triangleGenerator w h
                        ]
                    )

                Triangles ->
                    triangleGenerator w h

                Squares ->
                    squareGenerator w h
    in
        Random.map2 Object shapeGenerator colorGenerator


objectsGenerator : ObjectType -> Generator (List Object)
objectsGenerator objectType =
    list objectCount <| objectGenerator objectType


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetObjects objects ->
            { model | objects = objects } ! []

        RenderObjects objectType ->
            { model | objectType = objectType } ! [ generateObjects objectType ]


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


shapeMesh : Object -> Mesh Vertex
shapeMesh shape =
    let
        withColor =
            vertexWithColor shape.color

        mesh =
            case shape.shape of
                Triangle ( p1x, p1y ) ( p2x, p2y ) ( p3x, p3y ) ->
                    [ ( withColor (vec2 p1x p1y)
                      , withColor (vec2 p2x p2y)
                      , withColor (vec2 p3x p3y)
                      )
                    ]

                Square ( x, y ) w h ->
                    let
                        x2 =
                            x + w

                        y2 =
                            y + h
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


shapeEntity : Object -> Entity
shapeEntity object =
    entity
        vertexShader
        fragmentShader
        (shapeMesh object)
        { resolution = resolution
        }


getActiveStyle : ObjectType -> ObjectType -> ( String, String )
getActiveStyle currentObjectType buttonObjectType =
    if currentObjectType == buttonObjectType then
        ( "border", "2px solid #333" )
    else
        ( "", "" )


view : Model -> Html Msg
view model =
    let
        getStyleForType =
            getActiveStyle model.objectType

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
                [ button
                    [ buttonStyle
                        [ ( "background-color", "tomato" )
                        , getStyleForType Mixed
                        ]
                    , onClick (RenderObjects Mixed)
                    ]
                    [ text "mix!" ]
                , button
                    [ buttonStyle
                        [ ( "background-color", "peachpuff" )
                        , getStyleForType Triangles
                        ]
                    , onClick (RenderObjects Triangles)
                    ]
                    [ text "triangles!" ]
                , button
                    [ buttonStyle
                        [ ( "background-color", "MediumAquamarine" )
                        , getStyleForType Squares
                        ]
                    , onClick (RenderObjects Squares)
                    ]
                    [ text "squares!" ]
                ]
            , WebGL.toHtml
                [ width canvasWidth
                , height canvasHeight
                , style [ ( "display", "block" ) ]
                ]
                (List.map shapeEntity model.objects)
            ]


generateObjects : ObjectType -> Cmd Msg
generateObjects objectType =
    Random.generate SetObjects (objectsGenerator objectType)


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, generateObjects Mixed )
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }
