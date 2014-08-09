import Mouse
import Text
import Window
import WebSocket
import Json
import Dict

 
blog : Text
blog = style linkStyle <| Text.link "http://blog.jameslarisch.com" <| toText "blog"

github : Text
github = style linkStyle <| Text.link "http://github.com/semaj" <| toText "github"

linkedin : Text
linkedin = style linkStyle <| Text.link "http://linkedin.com/in/jameslarisch/" <| toText "linkedin"

links : Element
links = centered <| blog ++ (toText " | ") ++ github ++ (toText " | ") ++ linkedin

body : Text
body = style bodyStyle <|
  toText "\nHi! Weird website huh? It's " ++  
  Text.link "https://github.com/semaj/jameslarisch.com-elm/blob/master/public/Index.elm" (toText "written") ++
  toText " purely in " ++ Text.link "http://elm-lang.org/" (toText "Elm") ++
  toText ", a neat functional-reactive language which compiles down to CSS/HTML/JS.\n\n" ++
  toText "The pentagons move when a new transaction hits the Bitcoin network.\nThat random string is the new transaction hash!\n\n" ++ 
  toText "As for me, I'm a budding Software Engineer.\n\n" ++
  toText "My language of choice is Ruby, but I've worked a lot with (and like) Java, among others.\n\n" ++ 
  toText "I recently finished a co-op at " ++ Text.link "http://hubspot.com" (toText "Hubspot") ++
  toText ", which was AMAZING - I worked extensively with Java, Hadoop, HBase, and Kafka (Kafka is awesome). The experience really sparked my interest in distributed systems.\n\n" ++
  toText "I love Bitcoin - in fact, a few friends and I are working on `Zenchi`, a Bitcoin" ++ 
  toText " spending tracking site. It's closed source for now, but I've release some tools " ++
  toText "we've made and use " ++ Text.link "https://github.com/zenchilabs" (toText "here") ++
  toText ".\n\nFeel free to contact me at larisch.j(at)husky.neu.edu."

getSpinners : Int -> Int -> Int -> Form
getSpinners raw x y =
  let a = 15 * raw in
  group [
    move (-125, 0) <| rotate (toDegrees x) (filled red (ngon 3 20)),
    move (-25, 0) <| rotate (toDegrees a) (filled green (ngon 5 50)),
    move (0, 0) <| rotate (toDegrees a) (filled purple (ngon 5 50)),
    move (25, 0) <| rotate (toDegrees a) (filled yellow (ngon 5 50)),
    move (125, 0) <| rotate (toDegrees y) (filled blue (ngon 3 20))
  ]

getPage : Text -> Int -> (Int,Int) -> (Int, Int) -> Element
getPage hash c (w,h) (x,y) = 
  flow down [
    width w <| title "James Larisch",
    width w <| links,
    collage w 120 [ getSpinners c x y ],
    width w <| centered <| style hashStyle (hash ++ (toText "\n")),
    container w (round (0.75 * (toFloat h))) midTop <| 
              width (round (0.4 * (toFloat w))) <| 
              leftAligned body
  ]

main = lift4 getPage unconfirmed (count unconfirmed) Window.dimensions Mouse.position

-- Styles

title : String -> Element
title s = 
  let titleStyle = 
    { typeface = [ "Helvetica Neue" ],
      height = Just 36,
      color = black,
      bold = False,
      italic = False,
      line = Nothing
    } 
  in
  centered <| style titleStyle <| toText s

linkStyle : Style
linkStyle =
  { typeface = [ "Helvetica Neue" ],
    height = Just 14, 
    color = black,
    bold = False,
    italic = False,
    line = Nothing
  }

bodyStyle : Style
bodyStyle = 
  { typeface = [ "Garamond" ],
    height = Just 16, 
    color = black,
    bold = False,
    italic = False,
    line = Nothing
  }

hashStyle : Style
hashStyle = 
  { typeface = [ "Arial" ],
    height = Just 10, 
    color = black,
    bold = True,
    italic = False,
    line = Nothing
  }

-- Helpers

toDegrees : Int -> Float
toDegrees i = degrees <| toFloat <| mod i 360

unconfirmed : Signal Text
unconfirmed = lift clean <| WebSocket.connect "ws://ws.blockchain.info/inv" (constant "{\"op\":\"unconfirmed_sub\"}")
 
clean : String -> Text
clean t = 
  (Json.fromString t)   |> 
  delve [ "x", "hash" ] |>
  mbind decodeStr       |>
  getOrElse " -- "      |>
  toText
 

-- The following was written by https://github.com/lambdatoast

delve : [String] -> Maybe Json.Value -> Maybe Json.Value
delve xs mv = reducejson (map (\x -> getProp x) xs) mv
 
reducejson : [Json.Value -> Maybe Json.Value] -> Maybe Json.Value -> Maybe Json.Value
reducejson xs mv = foldl (\f b -> mbind f b) mv xs 
 
getProp : String -> Json.Value -> Maybe Json.Value
getProp n json = case json of
                  Json.Object d -> Just (Dict.getOrElse (Json.String "") n d)
                  _ -> Nothing
 
decodeStr : Json.Value -> Maybe String
decodeStr v = case v of
                Json.String s -> Just s
                _ -> Nothing
 
getStr : String -> Json.Value -> Maybe String
getStr n json = mbind decodeStr (getProp n json)
 
getOrElse : a -> Maybe a -> a
getOrElse b ma = case ma of
                 Just a -> a
                 Nothing -> b
                 
mbind : (a -> Maybe b) -> Maybe a -> Maybe b
mbind f ma = case ma of
                   Just a -> f a
                   _ -> Nothing
                   
mmap : (a -> b) -> Maybe a -> Maybe b
mmap f = mbind (\a -> Just (f a))

