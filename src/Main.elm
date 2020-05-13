module Main exposing (..)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


-- empty "test3" [("test", ["k1"], ["h1"]), ("test2", ["k1"], ["h1"])]
-- insert "test1" "k3" "nice" [("test", ["k1", "k2"], ["h1", "h2"]), ("test2", ["k1", "k2"], ["h1", "h2"]), ("test3", ["k1", "k2"], ["h1", "h2"])]
-- upd "k2" "niceh" ("nice", ["k1", "k2"], ["h1", "h2"])
-- pre [][text (show "test3" [("test", ["k1", "k2"], ["h1", "h2"]), ("test2", ["k1", "k2"], ["h1", "h2"]), ("test3", ["k1", "k2", "k3"], ["h1", "h2", "h3"])])]
-- showRep "test" [("test", ["k1", "k2"], ["h1", "h2"]), ("test2", ["k1", "k2"], ["h1", "h2"]), ("test3", ["k1", "k2"], ["h1", "h2"])]
-- isIn "test" "k1" [("test", [], [])]
-- value "test1" "k2" [("test", ["k1", "h2"], ["h1", "h2"]), ("test1", ["9k1", "k2"], ["h1", "1h2"])]
-- value "test2" "k1" [("test", ["k1", "k2"], ["h1", "h2"]), ("test2", ["k1", "k2"], ["2h1", "h2"]), ("test3", ["k1", "k2"], ["h1", "h2"])]
-- card "test3" [("test", ["k1", "k2", "k3"], ["h1", "h2", "h3"]), ("test2", ["2k1", "2k2", "2k3", "2k4"], ["2h1", "2h2", "2h3", "2h4"]), ("test3", ["k1", "k2"], ["h1", "h2"])]
-- dom "test3" [("test", ["k1", "k2", "k3"], ["h1", "h2", "h3"]), ("test2", ["2k1", "2k2"], ["2h1", "2h2"]), ("test3", ["k1", "k2"], ["h1", "h2"])]
-- equal ("table1", ["k1", "k2", "k3"], ["h1", "h2", "h3"]) ("table2", ["k3", "k2", "k1"], ["h1", "h2", "h3"])

main =
    Browser.sandbox { init = init, update = update, view = view }

--main = text <| Debug.toString
--            <| value "test2" "k1" [("test", ["k1", "k2"], ["h1", "h2"]), ("test2", ["k1", "k2"], ["2h1", "h2"]), ("test3", ["k1", "k2"], ["h1", "h2"])]

type alias Name = String
type alias Key = String
type alias Value = String
type alias Keys = List Key
type alias Values = List Value
type alias Table = (Name, Keys, Values)

type alias Model = (States, List Table)

type alias States =
  { name : Name
  , key : Key
  , value: Value
  , output : String  
  }


init : Model
init = (States "" "" "" "", [("test", ["k1", "k2", "k3"], ["h1", "h2", "h3"]), ("test2", ["2k1", "2k2", "2k3", "2k4"], ["2h1", "2h2", "2h3", "2h4"]), ("test3", ["k1", "k2"], ["h1", "h2"])])


type Msg
      -- Vstupy
  = Name String
  | Key String
  | Value String
      -- Tlacidla
  | Empty
  | Show
  | ShowRep
  | Insert
  | Remove
  | IsIn
  | ValFunc
  | Card
  | Dom


update : Msg -> Model -> Model
update msg (st, li) =
  case msg of
    Name text ->
      ({ st | name = text, output = "" }, li)

    Key text ->
      ({ st | key = text, output = "" }, li)

    Value text ->
      ({ st | value = text, output = "" }, li)

    Empty ->
      if st.name == "" 
      then (States "" "" "" "Treba vlozit meno novej tabulky", li)
      else (States "" "" "" ("Vytvorena nova tabulka: " ++ st.name), empty st.name li)

    Show ->
        (States "" "" "" (if st.name == "" 
                          then "Treba vlozit meno tabulky" 
                          else if member st.name (getFirsts li)
                          then "Hodnoty tabulky " ++ st.name ++ " su: " ++ show st.name li
                          else "Neznama tabulka "  ++ st.name), li)

    ShowRep ->
        (States "" "" "" (if st.name == "" 
                          then "Treba vlozit meno tabulky" 
                          else if member st.name (getFirsts li)
                          then "Hodnoty tabulky " ++ st.name ++ " reprezentovane strukturov: " ++ showRep st.name li
                          else "Neznama tabulka "  ++ st.name), li)

    Insert -> 
        (States "" "" "" (if st.name == "" || st.key == ""
                          then "Pre vkladanie do tabulky treba vlozit meno tabulky a kluc"
                          else if member st.name (getFirsts li)
                          then "Pridana hodnota " ++ st.key ++ ":" ++ st.value ++ " do tabulky " ++ st.name
                          else "Neznama tabulka" ++ st.name), insert st.name st.key st.value li)

    Remove -> 
        (States "" "" "" (if st.name == "" || st.key == ""
                          then "Pre odstranenie z tabulky treba vlozit meno tabulky a kluc"
                          else if member st.name (getFirsts li) && member st.key (getkeys (gettable st.name li))
                          then "Odstranena hodnota " ++ st.key ++ ":" ++ (myValue st.name st.key li) ++ " z tabulky " ++ st.name
                          else "Neznama tabulka alebo neplatny kluc" ++ st.name ++ "-" ++ st.key), remove st.name st.key li)
    IsIn ->
        (States "" "" "" (if st.name == "" || st.key == ""
                          then "Pre zistenie ci sa kluc nachadza v tabulke treba vlozit meno tabulky a kluc"
                          else if not (member st.name (getFirsts li))
                          then "Neznama tabulka"
                          else if isIn st.name st.key li
                          then "Kluc " ++ st.key ++ " sa nachadza v tabulke " ++ st.name
                          else "Kluc " ++ st.key ++ " sa nenachadza v tabulke " ++ st.name), li)
    
    ValFunc -> 
        (States "" "" "" (if st.name == "" || st.key == ""
                          then "Pre zistenie hodnoty treba vlozit meno tabulky a kluc"
                          else if member st.name (getFirsts li) && member st.key (getkeys (gettable st.name li))
                          then "Hodnota kluca " ++ st.key ++ " v tabulke " ++ st.name ++ " je " ++ myValue st.name st.key li
                          else "Neznama tabulka alebo neplatny kluc" ++ st.name ++ "-" ++ st.key), li)

    Card ->
        (States "" "" "" (if st.name == "" 
                          then "Treba vlozit meno tabulky" 
                          else if member st.name (getFirsts li)
                          then "Kardinalita tabulky " ++ st.name ++ ": " ++ String.fromInt (card st.name li)
                          else "Neznama tabulka "  ++ st.name), li)

    Dom ->
        (States "" "" "" (if st.name == "" 
                          then "Treba vlozit meno tabulky" 
                          else if member st.name (getFirsts li)
                          then "Kluce tabulky " ++ st.name ++ ": " ++ listToString identity (dom st.name li)
                          else "Neznama tabulka "  ++ st.name), li)
                          

view : Model -> Html Msg
view (st, li) =
  div [style "marginLeft" "20px"]
    [ h3 [][text "Tabulky"]
    , div [][text "Vstupy:"]
    , viewInput "text" "Meno tabulky" st.name Name
    , br [][]
    , viewInput "text" "Kluc" st.key Key
    , br [][]
    , viewInput "text" "Hodnota" st.value Value
    , br [][]
    , br [][]
    , button [ onClick Empty ] [ text "empty" ]
    , button [ onClick Insert ] [ text "insert" ]
    , button [ onClick Remove ] [ text "remove" ]
    , br [][]
    , button [ onClick Show ] [ text "show"]
    , button [ onClick ShowRep ] [ text "showRep"]
    , button [ onClick IsIn ] [ text "isIn"]
    , button [ onClick ValFunc ] [ text "value"]
    , button [ onClick Card ] [ text "card"]
    , button [ onClick Dom ] [ text "dom"]
    , br [][]
    , br [][]
    , div [][text "Definovane Tabulky: "]
    , viewNames li
    , p [] [text st.output]
    ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
  input [ type_ t, placeholder p, value v, onInput toMsg ] []

viewNames : List Table -> Html msg
viewNames lists =
  case lists of
    [] ->
      div [ style "color" "red" ] [ text "Ziadny zoznam nie je definovany." ]
    _ :: _ ->
      div [][text (listToString identity (getFirsts lists))]

gettable : Name -> List Table -> Table
gettable name tablelist =
    case tablelist of
        [] -> ("",[],[])
        first :: rest ->
            if getname first == name
            then first
            else gettable name rest

getname : Table -> Name
getname (name,_,_) = name

getkeys : Table -> Keys
getkeys (_,keys,_) = keys

getvalues : Table -> Values
getvalues (_,_,values) = values

member : a -> List a -> Bool
member a l = 
  case l of
    [] -> False
    first :: rest -> if (a == first) then True else member a rest

append : a -> List a -> List a
append a l = 
    case l of
        [] -> [a]
        first :: rest -> first :: append a rest

empty : Name -> List Table -> List Table
empty name tablelist =
  case tablelist of
    [] -> [(name, [], [])]
    first :: rest ->
      if getname first == name
      then (name, [], []) :: rest
      else first :: empty name rest
        

join : Table -> Table -> Table
join a b = 
    case getkeys b of
        [] -> a
        bkfirst :: bkrest ->
            case getvalues b of
                [] -> a
                bvfirst :: bvrest ->
                    join (getname a, append bkfirst (getkeys a), append bvfirst (getvalues a)) (getname b, bkrest, bvrest)

upd : Key -> Value -> Table -> Table
upd key val table =
    case getkeys table of
        [] -> table
        kfirst :: krest ->
            case getvalues table of
                [] -> table
                vfirst :: vrest ->
                    if kfirst == key
                    then (getname table, [kfirst] ++ krest, [val] ++ vrest)
                    else join (getname table, [kfirst], [vfirst]) (upd key val (getname table, krest, vrest))

insert : Name -> Key -> Value -> List Table -> List Table
insert name key val tablelist = 
    if val == ""
    then remove name key tablelist
    else case tablelist of
        [] -> tablelist
        first :: rest -> 
            if getname first == name 
            then if member key (getkeys first)
                 then [upd key val first] ++ rest
                 else [(name, append key (getkeys first), append val (getvalues first))] ++ rest
            else first :: insert name key val rest

show : Name -> List Table -> String
show name tablelist = 
    case tablelist of
        [] -> ""
        first :: rest ->
            if getname first == name
            then case getkeys first of
                [] -> ""
                kfirst :: krest ->
                    case getvalues first of
                        [] -> ""
                        vfirst :: vrest -> kfirst ++ ":" ++ vfirst ++ "\n" ++ show name [(name, krest, vrest)] 
            else show name rest

showRep : Name -> List Table -> String
showRep name tablelist = 
    case tablelist of
        [] -> ""
        first :: rest ->
            if getname first == name
            then Debug.toString first
            else showRep name rest

isIn : Name -> Key -> List Table -> Bool
isIn name key tablelist =
    case tablelist of
        [] -> False
        first :: rest ->
            if getname first == name
            then case getkeys first of
                [] -> False
                kfirst :: krest ->
                    if kfirst == key
                    then True
                    else isIn name key [(name, krest, getvalues first)]
            else isIn name key rest

myValue : Name -> Key -> List Table -> Value
myValue name key tablelist =
    if key /= ""
    then case tablelist of
        [] -> ""
        first :: rest -> 
            if getname first == name
            then case getkeys first of
                [] -> ""
                kfirst :: krest ->
                    case getvalues first of
                        [] -> ""
                        vfirst :: vrest ->
                            if kfirst == key
                            then vfirst
                            else myValue name key [(name, krest, vrest)]
            else myValue name key rest
    else ""

myfilter : (a -> Bool) -> List a -> List a
myfilter func l =
    case l of
        [] -> []
        first :: rest ->
            if func first
            then myfilter func rest
            else first :: myfilter func rest

remove : Name -> Key -> List Table -> List Table
remove name key tablelist =
    case tablelist of
        [] -> tablelist
        first :: rest ->
            if getname first == name
            then (name, myfilter (\v -> v == key) (getkeys first), myfilter (\v -> v == (myValue name key tablelist)) (getvalues first)) :: rest
            else first :: remove name key rest

length : List a -> Int
length a = 
  case a of
    [] -> 0
    _ :: rest -> 1 + length rest

card : Name -> List Table -> Int
card name tablelist = 
    case tablelist of
        [] -> 0
        first :: rest ->
            if getname first == name
            then length (getkeys first)
            else card name rest

dom : Name -> List Table -> Keys
dom name tablelist =
    case tablelist of
        [] -> []
        first :: rest ->
            if getname first == name
            then getkeys first
            else dom name rest

isInList : a -> List a -> Bool
isInList val l = 
    case l of
        [] -> False
        first :: rest ->
            if first == val
            then True
            else isInList val rest

eqlist : List a -> List a -> Bool
eqlist l1 l2 =
    if l1 == [] && l2 == []
    then True
    else case l1 of
        [] -> False
        first :: rest ->
            if isInList first l2
            then eqlist rest (myfilter (\v -> v == first) l2)
            else False


equal : Table -> Table -> Bool
equal table1 table2 =
    if eqlist (getkeys table1) (getkeys table2) && eqlist (getvalues table1) (getvalues table2)
    then True
    else False

getFirsts : List Table -> List Name
getFirsts list =
  case list of
    [] -> []
    first :: rest ->
      getname first :: getFirsts rest


listToString : (a -> String) -> List a -> String
listToString fun list =
  let
    lts f li =
      case li of
        [] -> ""
        first :: rest ->
          (f first)
          ++ if rest == [] then "" else ", "
          ++ lts f rest
  in
    "[" ++ (lts fun list) ++ "]"
    
