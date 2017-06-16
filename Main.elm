module Main exposing (..)

import Html exposing (Html, div, button, text)
import Html.Attributes exposing (style, width, height)
import Html.Events exposing (onClick)
import WebGL exposing (Shader, Mesh, Entity, entity)
import Math.Vector2 exposing (Vec2, vec2)
import Math.Vector3 exposing (Vec3, vec3)
import Random exposing (Generator, list, pair, float)


objectCount : Int
objectCount =
    50


canvasWidth : Int
canvasWidth =
    1680


canvasHeight : Int
canvasHeight =
    880


type ObjectType
    = Triangles
    | Squares


type Msg
    = SetTriangles (List Triangle)
    | SetSquares (List Square)
    | RenderTriangles
    | RenderSquares


type alias Vertex =
    { position : Vec2
    , color : Vec3
    }


type alias Triangle =
    { vertices : ( Point, Point, Point ), color : Color }


type alias Square =
    { base : Point
    , width : Float
    , height : Float
    , color : Color
    }


type alias Uniforms =
    { resolution : Vec2 }


type alias Point =
    ( Float, Float )


type alias Model =
    { triangles : List Triangle
    , squares : List Square
    , objectType : ObjectType
    }


initialModel : Model
initialModel =
    Model [] [] Triangles


type alias Color =
    ( Float, Float, Float )


colorValueGenerator : Generator Float
colorValueGenerator =
    float 0 1


colorGenerator : Generator Color
colorGenerator =
    Random.map3 (,,) colorValueGenerator colorValueGenerator colorValueGenerator


positionGenerator : Float -> Generator Float
positionGenerator =
    float 0


triangleVertices : Float -> Float -> Generator ( Point, Point, Point )
triangleVertices width height =
    Random.map3 (,,)
        (pointGenerator width height)
        (pointGenerator width height)
        (pointGenerator width height)


squareBase : Float -> Float -> Generator Point
squareBase width height =
    (pointGenerator width height)


pointGenerator : Float -> Float -> Generator Point
pointGenerator width height =
    Random.map2 (,)
        (positionGenerator width)
        (positionGenerator height)


triangleGenerator : Float -> Float -> Generator Triangle
triangleGenerator width height =
    Random.map2 Triangle
        (triangleVertices width height)
        colorGenerator


squareGenerator : Float -> Float -> Generator Square
squareGenerator width height =
    Random.map4 Square
        (squareBase width height)
        (positionGenerator (width / 2))
        (positionGenerator (height / 2))
        colorGenerator


trianglesGenerator : Float -> Float -> Generator (List Triangle)
trianglesGenerator width height =
    list objectCount <| triangleGenerator width height


squaresGenerator : Float -> Float -> Generator (List Square)
squaresGenerator width height =
    list objectCount <| squareGenerator width height


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetTriangles triangles ->
            { model | triangles = triangles } ! []

        SetSquares squares ->
            { model | squares = squares } ! []

        RenderSquares ->
            { model | objectType = Squares } ! [ generateSquares ]

        RenderTriangles ->
            { model | objectType = Triangles } ! [ generateTriangles ]


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


colorToVec3 : Color -> Vec3
colorToVec3 ( r, g, b ) =
    vec3 r g b


triangleMesh : Triangle -> Mesh Vertex
triangleMesh triangle =
    let
        ( ( p1x, p1y ), ( p2x, p2y ), ( p3x, p3y ) ) =
            triangle.vertices

        color =
            colorToVec3 triangle.color

        mesh =
            [ ( Vertex (vec2 p1x p1y) color
              , Vertex (vec2 p2x p2y) color
              , Vertex (vec2 p3x p3y) color
              )
            ]
    in
        WebGL.triangles mesh


squareMesh : Square -> Mesh Vertex
squareMesh square =
    let
        ( x, y ) =
            square.base

        x1 =
            x

        x2 =
            x + square.width

        y1 =
            y

        y2 =
            y + square.height

        color =
            colorToVec3 square.color

        mesh =
            [ ( Vertex (vec2 x1 y1) color
              , Vertex (vec2 x2 y1) color
              , Vertex (vec2 x1 y2) color
              )
            , ( Vertex (vec2 x1 y2) color
              , Vertex (vec2 x2 y1) color
              , Vertex (vec2 x2 y2) color
              )
            ]
    in
        WebGL.triangles mesh


triangleEntity : Triangle -> Entity
triangleEntity triangle =
    entity
        vertexShader
        fragmentShader
        (triangleMesh triangle)
        { resolution = vec2 (toFloat canvasWidth) (toFloat canvasHeight)
        }


squareEntity : Square -> Entity
squareEntity square =
    entity
        vertexShader
        fragmentShader
        (squareMesh square)
        { resolution = vec2 (toFloat canvasWidth) (toFloat canvasHeight)
        }


triangleEntities : List Triangle -> List Entity
triangleEntities =
    List.map triangleEntity


squareEntities : List Square -> List Entity
squareEntities =
    List.map squareEntity


view : Model -> Html Msg
view model =
    let
        buttonStyle extraStyle =
            style
                (List.append
                    extraStyle
                    [ ( "display", "inline-block" )
                    , ( "padding", "20px" )
                    , ( "border", "none" )
                    , ( "margin-right", "20px" )
                    , ( "text-transform", "uppercase" )
                    , ( "font-weight", "bold" )
                    , ( "font-size", "18px" )
                    , ( "outline", "none" )
                    ]
                )

        entities =
            case model.objectType of
                Squares ->
                    (squareEntities model.squares)

                Triangles ->
                    (triangleEntities model.triangles)
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
                    [ buttonStyle [ ( "background-color", "tomato" ) ]
                    , onClick RenderTriangles
                    ]
                    [ text "triangles!" ]
                , button
                    [ buttonStyle [ ( "background-color", "peachpuff" ) ]
                    , onClick RenderSquares
                    ]
                    [ text "squares!" ]
                ]
            , WebGL.toHtml
                [ width canvasWidth
                , height canvasHeight
                , style [ ( "display", "block" ) ]
                ]
                entities
            ]


generateTriangles : Cmd Msg
generateTriangles =
    Random.generate
        SetTriangles
        (trianglesGenerator (toFloat canvasWidth) (toFloat canvasHeight))


generateSquares : Cmd Msg
generateSquares =
    Random.generate
        SetSquares
        (squaresGenerator (toFloat canvasWidth) (toFloat canvasHeight))


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, generateTriangles )
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }
