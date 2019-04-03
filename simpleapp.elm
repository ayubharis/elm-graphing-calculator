{- Graphing Calculator using SVG in Elm
- Haris Ayub CS1XA3 Project 2 - Part 3
- April 1st 2019
-}

import Browser
--import Css exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
--import Html.Styled exposing (..)
--import Html.Styled.Attributes exposing (css, href, src)
import Html.Events exposing (onInput, onClick)
import Parser exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Random exposing (..)
import String
import List

--I was planning on using CSS to format the graph to be prettier, but since I 
--didn't start the project with elm-css, there were too many instances where I
--had to re-do the code, so the graph is quite ugly

main =
  Browser.sandbox { init = init, update = update, view = view }

--Our model contains, the range for our x and y values, a zoom factor, 
--and lists for the functions on the graph

type alias Model = { xMin : Float
                   , xMax : Float
                   , yMin : Float
                   , yMax : Float
                   , zoom : Float
                   , zoomStr : String
                   , xRangeStr : String
                   , yRangeStr : String
                   , funcStr : String
                   , expList : List Expression
                   , funcList : List Function
                   }

--Here we have an alias that will be used to make our functions more readable,
--in our case a Function takes a number and returns another one 

type alias Function = (Float -> Float)

--A new type that we use to make parsing a lot easier

type Expression = Const Float
                | X
                | Add Expression Expression
                | Sub Expression Expression
                | Mult Expression Expression
                | Div Expression Expression
                | Pow Expression Expression
                | Sin Expression
                | Cos Expression
                | Tan Expression
                | Ln Expression
                

--A function that shows the string representation of an expression
showExp : Expression -> String
showExp e = 
  case e of
    Const c -> String.fromFloat c
    X -> "x"
    (Add a b) -> (showExp a) ++ "+" ++ (showExp b)
    Sub a b -> (showExp a) ++ "-" ++ (showExp b)
    Mult a b -> (showExp a) ++ "*" ++ (showExp b)
    Div a b -> (showExp a) ++ "/" ++ (showExp b)
    Pow a b -> (showExp a) ++ "^" ++ (showExp b)
    Sin a -> "sin" ++ (showExp a)
    Cos a -> "cos" ++ (showExp a)
    Tan a -> "tan" ++ (showExp a)
    Ln a -> "ln" ++ (showExp a)

--A function to evaluate an expression, we will curry this function to
--get a function that takes a number and returns a number

evaluate : Expression -> Function
evaluate exp num =
  case exp of
    Const c -> c
    X -> num
    (Add a b) -> (evaluate a num) + (evaluate b num)
    Sub a b -> (evaluate a num) - (evaluate b num)
    Mult a b -> (evaluate a num) * (evaluate b num)
    Div a b -> (evaluate a num) / (evaluate b num)
    Pow a b -> (evaluate a num) ^ (evaluate b num)
    Sin a -> sin (evaluate a num)
    Cos a -> cos (evaluate a num)
    Tan a -> tan (evaluate a num)
    Ln a -> logBase e (evaluate a num)


init = { xMin = -20
       , xMax = 20
       , yMin = -20
       , yMax = 20
       , zoom = 1
       , zoomStr = ""
       , funcStr = "x^4/4"
       , xRangeStr = ""
       , yRangeStr = ""
       , expList = [(Pow X (Const 2)), Mult (Ln X) (Sin X)]
       , funcList = [evaluate (Pow X (Const 2)), evaluate (Mult (Ln X) (Sin X))]
       }


type Msg = NewZoom String
         | NewFunc String
         | ChangeZoom
         | AddFunc
         | XRange String
         | YRange String
         | UpdateRange
         | DeleteFunc Expression

--Here we update Msg depending on what the user has done
--If they update a text field, the string is put in a temporary
--value, and then later parsed when they click a button
--There is also a delete function, which removes all functions
update : Msg -> Model -> Model
update msg model =
  case msg of
     NewZoom str ->
       { model | zoomStr = str}
     ChangeZoom -> --if the zoomStr is invalid, do not zoom
       { model | zoom = Maybe.withDefault 1 (String.toFloat model.zoomStr)}
     NewFunc str ->
       { model | funcStr = str}
     AddFunc -> --remove the spaces and then parse the expression that was input
       let newExpList = List.append [(parseExpression (String.filter (\c -> c /= ' ') model.funcStr))]  model.expList
       in
       { model | expList = newExpList
               , funcList = List.map (evaluate) newExpList}
     XRange str ->
       { model | xRangeStr = str}
     YRange str ->
       { model | yRangeStr = str}
     UpdateRange ->
       let
         xTup = parseRange model.xRangeStr
         yTup = parseRange model.yRangeStr
       in
       { model | xMin = Tuple.first xTup, xMax = Tuple.second xTup, yMin = Tuple.first yTup, yMax = Tuple.second yTup}
     DeleteFunc exp -> --keep all the expressions which are NOT equal to the provided expression to delete
       let newExpList = List.filter (notEq exp) model.expList
       in
       { model | expList = newExpList
               , funcList = List.map (evaluate) newExpList}

--a function to check equality of 2 exactly identical expressions
--I made this due to being unable to map /= over a list
notEq : Expression -> Expression -> Bool
notEq e1 e2 = e1 /= e2

--returns a tuple for range, given a string like "-5,25"
parseRange : String -> (Float, Float)
parseRange s = 
  let
    points = String.split "," s
    oneStr = Maybe.withDefault "-20" (List.head points)
    twoStr = Maybe.withDefault "20" (List.head (List.reverse points))
    one = Maybe.withDefault (-20) (String.toFloat oneStr)
    two = Maybe.withDefault 20 (String.toFloat twoStr)
  in (one,two)

view : Model -> Html Msg
view model = 
  div []
    ([ viewInput "zoomStr" "Enter new zoom factor!" model.zoomStr NewZoom
    , button [ onClick ChangeZoom ] [Html.text "Zoom!"]
    , viewInput "funcStr" "Enter a function!" model.funcStr NewFunc 
    , button [ onClick AddFunc ] [Html.text "Add Function"]
    , br [] []
    , viewInput "xRangeStr" "Enter x range e.g. (-20,20)" model.xRangeStr XRange
    , viewInput "yRangeStr" "Enter y range e.g. (-20,20)" model.yRangeStr YRange
    , button [ onClick UpdateRange ] [Html.text "Update Range"]
    , br [] []
    , br [] []
    ] ++ (showFunctions model.expList) ++ [div [] [(graph model)]])

--A function to make text fields easier to write, taken from elm-lang.org
viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
  input [ Html.Attributes.type_ t, placeholder p, value v, onInput toMsg ] []

--Recursively show all the functions, with an option to delete them
showFunctions : List Expression -> List (Html Msg)
showFunctions es = 
  case es of
    [] -> []
    x::xs ->
      [ button [ onClick (DeleteFunc x) ] [ Html.text "Delete this function" ]
      , Html.text (showExp x)
      , br [] []
      ] ++ showFunctions xs

type alias Operator = (Expression -> Expression -> Expression) -- A type Alias for most constructors in Expression

--My parser 
--Not fully functional, and has some bad cases
--but since it is pretty complicated to write a
--parser, even with using elm/parser I decided
--to leave this as is and work on other features

parseExpression : String -> Expression
parseExpression s = 
  let 
    isNum = String.toFloat s
    sList = String.toList s
  in
  -- case sList of     --This was an attempt to parse more complicated
                       --functions like ln(sin(x^2)), but I could not 
                       --figure out how to pattern match properly
  --   's'::'i'::'n'::'('::exp::')'::lst ->
  --     Sin (parseExpression (String.fromList exp))
  --   'c'::'o'::'s'::'('::exp::')'::lst ->
  --     Cos (parseExpression exp)
  --   't'::'a'::'n'::'('::exp::')'::lst ->
  --     Tan (parseExpression exp)
  --   'l'::'n'::'('::exp::')'::lst ->
  --     Ln (parseExpression exp)
  --   _ ->
  case s of
    "sin(x)" -> Sin X
    "cos(x)" -> Cos X
    "tan(x)" -> Tan X
    "ln(x)" -> Ln X
    "x" -> X
    "e" -> Const Basics.e
    "pi" -> Const Basics.pi
    _ -> 
      case isNum of 
      Nothing -> 
        if (String.contains "+" s) then
          parseHelper (String.split "+" s) Add
        else if (String.contains "-" s) then
          parseHelper (String.split "-" s) Sub
        else if (String.contains "*" s) then
          parseHelper (String.split "*" s) Mult
        else if (String.contains "/" s) then
          parseHelper (String.split "/" s) Div
        else --if (String.contains "^" s)
          parseHelper (String.split "^" s) Pow
      _ ->
        Const (Maybe.withDefault 1 (String.toFloat s))

parseHelper : List String -> Operator -> Expression
parseHelper ss oper = 
  case ss of
    [] -> Const 1 --This case should be dependent on the operation, 
                  --however I had some trouble pattern matching on operators
                  --So I picked a Const thats safe in all cases (e.g. Div)
    x::[] -> parseExpression x
    x::xs -> 
      oper (parseExpression x) (parseHelper xs oper)

--Function that graphs everything, this function calls all the smaller functions
--that make up everything in the graph, including functions and gridlines
graph : Model -> Html Msg
graph model = 
  let 
    zoom = model.zoom
    xMin = model.xMin
    xMax = model.xMax
    yMin = model.yMin
    yMax = model.yMax
    fs = model.funcList
    --since our canvas is always the same size, we need to define a
    --stretching/compressing factor for both our x and y
    xScale = 750/(abs xMin + abs xMax)
    yScale = 500/(abs yMin + abs yMax)
    xMinStr = String.fromFloat(xMin * xScale)
    yMinStr = String.fromFloat(yMin * yScale)
    xMaxStr = String.fromFloat(xMax * xScale)
    yMaxStr = String.fromFloat(yMax * yScale)
    xAvgStr = String.fromFloat(xScale*(xMax + xMin)/2)
    yAvgStr = String.fromFloat(yScale*(yMax + yMin)/2)
    --since our graph is drawn from negative y at the top, we need to redefine y=0
    newZero = (yMax + yMin) * yScale
    newZeroStr = String.fromFloat newZero
  in
  svg
    [ Svg.Attributes.width "750"
    , Svg.Attributes.height "500"
    , Svg.Attributes.viewBox (xMinStr++" "++yMinStr++ " 750 500")
    ]
    ([ rect
        [ x xMinStr
        , y yMinStr
        , Svg.Attributes.width "750"
        , Svg.Attributes.height "500"
        , Svg.Attributes.style "fill:white;stroke:black;stroke-width:2;opacity:0.5"
        ] 
        []
    , line -- draw the x axis
        [ x1 xMinStr
        , x2 xMaxStr
        , y1 newZeroStr
        , y2 newZeroStr
        , Svg.Attributes.style "stroke:rgb(0,0,0);stroke-width:1.5"
        ]
        []
    , line -- draw the y axis
        [ x1 "0"
        , x2 "0"
        , y1 yMinStr
        , y2 yMaxStr
        , Svg.Attributes.style "stroke:rgb(0,0,0);stroke-width:1.5"
        ]
        []
    ] ++ drawGridLines xMin xMax yMin yMax xScale yScale newZero (xMin*xScale) (yMin*yScale) 
      ++ drawFuncs fs xMin xMax zoom xScale yScale newZero (initialSeed 43103))

---A function to recursively draw gridlines
drawGridLines : Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> List (Svg Msg)
drawGridLines xMn xMx yMn yMx xScale yScale newZero stepW stepH =
  let
    xMin = xMn*xScale
    xMax = xMx*xScale
    yMin = yMn*yScale
    yMax = yMx*yScale
    width = (abs xMin) + (abs xMax)
    height = (abs yMin) + (abs yMax)
    wInt = width/8
    hInt = height/8
  in
  if stepW >= xMax || stepH >= yMax then []
  else [ line --Draw horizontal and vertical gridlines
          [ x1 (String.fromFloat stepW)
          , x2 (String.fromFloat stepW)
          , y1 (String.fromFloat yMin)
          , y2 (String.fromFloat yMax)
          , Svg.Attributes.style "stroke:rgb(150,150,150);stroke-width:0.5"
          ]
          []
        , line 
          [ x1 (String.fromFloat xMin)
          , x2 (String.fromFloat xMax)
          , y1 (String.fromFloat stepH)
          , y2 (String.fromFloat stepH)
          , Svg.Attributes.style "stroke:rgb(150,150,150);stroke-width:0.5"
          ]
          []
        , Svg.text_ --draw axis labels
        [ x "0"
        , y (String.fromFloat (stepH))
        , Svg.Attributes.fill "red"
        ]
        [Svg.text (String.fromFloat (myRound ((-1)*height*stepH/500/yScale + (newZero)/yScale) 2)) ] 
        --above we have a few things to do ^^ to reformat numbers to get our desired scale
        , Svg.text_
        [ x (String.fromFloat stepW)
        , y (String.fromFloat newZero)
        , Svg.Attributes.fill "blue"
        ]
        [Svg.text (String.fromFloat (myRound (width*stepW/750/xScale) 2)) ]
       ] ++ drawGridLines xMn xMx yMn yMx xScale yScale newZero (stepW + wInt) (stepH + hInt)

myRound : Float -> Int -> Float --The built in round gets rid of all decimals, so I wrote this
myRound num dec = 
  let newDec = toFloat dec
  in
  toFloat (round (num*(10.0^newDec))) / (10.0^newDec)

--a function we use to draw all the functions in expression list
--it first picks a random colour and then recursively plots
--each function using Svg polyline
drawFuncs : List Function -> Float -> Float -> Float -> Float -> Float -> Float -> Seed -> List (Svg Msg)
drawFuncs fs xMin xMax zoom xScale yScale newZero seed =
  let
    rTup = Random.step (Random.int 0 255) seed
    red = String.fromInt(Tuple.first rTup)
    seed2 = Tuple.second rTup
    gTup = Random.step (Random.int 0 255) seed2
    green = String.fromInt(Tuple.first gTup)
    seed3 = Tuple.second gTup
    bTup = Random.step (Random.int 0 255) seed3
    blue = String.fromInt(Tuple.first bTup)
    nextSeed = Tuple.second bTup
  in
  case fs of 
  [] -> []
  x::xs -> 
    [polyline
        [ Svg.Attributes.fill "none"
        , Svg.Attributes.stroke ("rgb("++red++","++green++","++blue++")")
        , strokeWidth "1.5"
        , points (generatePoints (modifyFunc x zoom) xMin xMax 0.05 xScale yScale newZero)
        ]
        []
    ] ++ drawFuncs xs xMin xMax zoom xScale yScale newZero nextSeed

--This function can zoom in or out (or even flip) the graph
--The -1* is due to the earlier mentioned problem of Svg
--starting from negative y at the top

modifyFunc : Function -> Float -> Function
modifyFunc f zoom = \x -> -1 * zoom * f x

--a function to generate points formatted as "x,y" as input to the points attribute of Svg polyline

generatePoints : Function -> Float -> Float -> Float -> Float -> Float -> Float -> String
generatePoints func start stop step xScale yScale newZero = 
  let 
    x = xScale*start
    y = yScale*(func start) + newZero
    currentPoint = 
      if String.fromFloat(func start) == "Infinity" 
      || String.fromFloat(func start) == "-Infinity" 
      || String.fromFloat(func start) == "NaN"
        then ""
      else String.fromFloat x ++ "," ++ String.fromFloat y ++ " "
  in
  if start >= stop then currentPoint
  else  currentPoint ++ (generatePoints func (start + step) stop step xScale yScale newZero)