module Main exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (style, width, height)
import WebGL exposing (Shader, Mesh, Entity, entity)
import Math.Vector2 exposing (Vec2, vec2)
import Math.Vector3 exposing (Vec3, vec3)
import Random exposing (Generator, list, pair, float)


canvasWidth : Int
canvasWidth =
    1680


canvasHeight : Int
canvasHeight =
    800


type Msg
    = SetTriangles (List (List Point))
    | SetColor Color


type alias Vertex =
    { position : Vec2
    , color : Vec3
    }


type alias Uniforms =
    { resolution : Vec2 }


type alias TriangleDefinition =
    List Point


type alias Triangle =
    ( Point, Point, Point )


type alias Point =
    ( Float, Float )


type alias Model =
    { triangles : List TriangleDefinition
    , color : Color
    }


type Color
    = Red
    | Green
    | Blue


colorMap : Color -> Vec3
colorMap color =
    case color of
        Red ->
            vec3 1 0 0

        Green ->
            vec3 0 1 0

        Blue ->
            vec3 0 0 1


intToColor : Float -> Color
intToColor i =
    case i of
        1 ->
            Red

        2 ->
            Green

        _ ->
            Blue


color : Generator Color
color =
    Random.map intToColor (float 1 3)


randomPosition : Float -> Generator Float
randomPosition size =
    float -size size


randomTriangle : Float -> Float -> Generator (List (List Point))
randomTriangle width height =
    let
        getPositionWithinHeight =
            randomPosition height

        getPositionWithinWidth =
            randomPosition width
    in
        list 20 <| list 3 <| Random.map2 (,) getPositionWithinWidth getPositionWithinHeight


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetColor clr ->
            let
                _ =
                    Debug.log "clr" clr
            in
                { model | color = clr } ! []

        SetTriangles triangles ->
            { model | triangles = triangles } ! []


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


pointToVec2 : Point -> Vec2
pointToVec2 ( x, y ) =
    vec2 x y


triangleMesh : TriangleDefinition -> Color -> Mesh Vertex
triangleMesh triangle clr =
    let
        vectors =
            List.map pointToVec2 triangle

        vertexes =
            List.map (\v2 -> Vertex v2 (colorMap clr)) vectors

        triangles =
            case vertexes of
                [ a, b, c ] ->
                    Just [ ( a, b, c ) ]
                        |> Maybe.withDefault []

                _ ->
                    []
    in
        WebGL.triangles triangles


triangleEntity : TriangleDefinition -> Color -> Entity
triangleEntity triangle clr =
    entity
        vertexShader
        fragmentShader
        (triangleMesh triangle clr)
        { resolution = vec2 (toFloat canvasWidth) (toFloat canvasHeight)
        }


triangleEntities : Model -> List Entity
triangleEntities model =
    List.map (\triangle -> triangleEntity triangle model.color) model.triangles


view : Model -> Html msg
view model =
    WebGL.toHtml
        [ width canvasWidth
        , height canvasHeight
        , style [ ( "display", "block" ) ]
        ]
        (triangleEntities model)


main : Program Never Model Msg
main =
    Html.program
        { init =
            Model [] Red
                ! [ Random.generate SetTriangles (randomTriangle (toFloat canvasWidth) (toFloat canvasHeight))
                  ]
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }
