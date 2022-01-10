module Main exposing (..)

import Browser
import Browser.Events as BE
import Html exposing (div, text, p, button, table, tr, td)
import Html.Attributes as HA
import Html.Events as HE
import Svg exposing (path)
import Svg.Attributes as SA
import Svg.Events as SE
import Svg.Keyed
import Tuple exposing (pair)
import Array
import Time exposing (Posix, posixToMillis)
import Grid exposing (Grid, Coords, get_row, set_cell, get_cell, coords)
import Json.Decode as D

keyedNode = Svg.Keyed.node
tabindex = fi >> HA.attribute "tabindex"

ariaLabel = HA.attribute "aria-label"
ariaLive = HA.attribute "aria-live"

pluralise n single plural = if n == 1 then single else plural

connected_components = Grid.connected_components False ((/=) Discarded)

strf : String -> List String -> String
strf template bits =
    let
        next_bit cbits = case cbits of
            a::rest -> (a,rest)
            [] -> ("",[])
    in
        Tuple.first <| List.foldl (\chr -> \(out,cbits) -> 
            if chr=='%' then
                let
                    (suffix,nbits) = next_bit cbits
                in
                    (out++suffix, nbits)
            else
                (out++(String.fromChar chr), cbits)
        ) ("",bits) (String.toList template)

phi = (1 + (sqrt 5)) / 2

golden a b k i = 
    let
        z = phi * (toFloat i) * k
        q = (z - (toFloat (floor z)))
    in
        a + (b-a)*q

fi = String.fromInt
ff = String.fromFloat
tf = toFloat

main = Browser.application
    { init = \() -> \_ -> \_ -> (init,Cmd.none)
    , view = view
    , update = \msg -> \m -> (update msg m, Cmd.none)
    , subscriptions = subscriptions
    , onUrlRequest = \_ -> Noop
    , onUrlChange = \_ -> Noop
    }

animating = True
subscriptions model = if animating then Time.every 100 Frame else Sub.none

type alias Player = Int

type Cell
    = Free
    | Discarded
    | Claimed Player

type alias Model = 
    { grid : Grid Cell
    , selected_cells : List Coords
    , current_player : Player
    , time : Posix
    , stage : GameStage
    , keyboard_cell : Maybe Coords
    }

type Msg
    = ClickCell Coords
    | KeyboardSelectCell Coords
    | PrevIntro
    | NextIntro
    | FinishMove
    | Start
    | StartAgain
    | Frame Posix
    | FocusCells
    | MoveKeyboardCell (Coords)
    | Noop
    | ShowHelp

blank_intro =
    { text = [tspan [] ""]
    , grid = fullGrid
    }

fullGrid = Grid.fill 6 6 Free

apply_moves : List (List Coords) -> Grid Cell
apply_moves moves = List.foldl (\(p,cells) -> \grid -> dig (modBy 2 p) grid cells) fullGrid (List.indexedMap pair moves)

tspan attrs t = Svg.tspan attrs [Svg.text t]
plain = tspan []
bold = [SA.fontWeight "bold"]
italic = [SA.fontStyle "italic"]

intro_screens = Array.fromList
    [ { text = [plain "This island ", tspan italic "is", plain " big enough for the both of us"]
      , grid = fullGrid
      }
    , { text = [plain "but we ", tspan (bold) "don't want to be together", plain "."]
      , grid = fullGrid
      }
    , { text = [plain "We'll divide it up between us"]
      , grid = fullGrid
      }
    , { text = [plain "by digging ", tspan bold "two squares at a time", plain "."]
      , grid = apply_moves [[(2,2),(3,2)]]
      }
    , { text = [plain "Our islands will be ", tspan bold "odd sizes", plain "."]
      , grid = apply_moves 
            [ [(2,2),(3,2)]
            , [(2,0),(2,1)]
            , [(1,2),(1,3)]
            , [(0,3),(0,4)]
            , [(2,4), (2,5)]
            , [(4,2), (5,2)]
            , [(3,4), (3,5)]
            , [(4,3), (4,4)]
            ]
      }
    , { text = [plain "and ", tspan bold "less than 10 squares each", plain "."]
      , grid = apply_moves
            [ [(0,2), (1,2)]
            , [(2,2), (3,2)]
            , [(4,2), (4,3)]
            , [(5,2), (5,1)]
            ]
      }
    , { text = [tspan (bold) "Most squares wins!"]
      , grid = apply_moves 
            [ [(2,2),(3,2)]
            , [(2,0),(2,1)]
            , [(1,2),(1,3)]
            , [(0,3),(0,4)]
            , [(2,4), (2,5)]
            , [(4,2), (5,2)]
            , [(3,4), (3,5)]
            , [(4,3), (4,4)]
            , [(3,0), (4,0)]
            , [(4,1), (5,1)]
            , [(2,3), (3,3)]
            , [(5,4), (5,5)]
            ]
      }
    ]

get_intro_screen i = Maybe.withDefault blank_intro (Array.get i intro_screens)


init =
    { grid = Grid.fill 6 6 Free
--    { grid = 
    , selected_cells = []
    , current_player = 0
    , time = Time.millisToPosix 0
    , stage = Intro 0
    , keyboard_cell = Nothing
    }

view model = 
    { title = "This island is big enough for the both of us"
    , body = 
        [ div 
            [ HA.style "max-height" "100vh"
            , HA.style "overflow" "hidden"
            , HA.style "touch-action" "manipulation"
            ]
            [ view_grid model
            ]
        ]
    }

can_select pos model =
    let
        is_free = (get_cell pos model.grid) == Just Free
        is_adjacent = List.any (coords_adjacent pos) model.selected_cells
        num_selected = List.length model.selected_cells
    in
        is_free && (num_selected==0 || (num_selected < 2 && is_adjacent))

hsl h s l = strf "hsl(%,%,%)" [ff h, (ff s)++"%", (ff l)++"%"]

scale = 100

strokeWidth = SA.strokeWidth (ff (scale/10))

format_coords (x,y) = (ff (scale * (tf x)))++","++(ff (scale * (tf y)))
format_fcoords (x,y) = (ff (scale * x))++","++(ff (scale * y))

box_margin = 2

bounding_box model = 
    { x1 = -box_margin * scale
    , y1 = -box_margin * scale
    , x2 = tf <| (model.grid.width + 2*box_margin)*scale
    , y2 = tf <| (model.grid.height + 2*box_margin)*scale
    }

view_grid model =
    let
        sea = view_sea model
        bbox = bounding_box model
        intro_symbol i player = 
            let
                x = if i==0 then ((tf model.grid.width)*(if player==0 then 1 else 3)/4) else 1 + (if player==0 then 0 else (tf model.grid.width)-2)
                y = if i==0 then ((tf model.grid.height)/2+0) else 1 + (if player==0 then 0 else (tf model.grid.height)-2)
            in
                Svg.text_
                    [ (SA.x << ff) (scale * x)
                    , (SA.y << ff) (scale * y)
                    , (SA.fontSize << ff) (scale * 1.5)
                    , SA.dominantBaseline "central"
                    , SA.textAnchor "middle"
                    ]
                    [Svg.text (player_symbol player)]

        islands = view_islands model

        intro_symbols = case model.stage of
            Intro i -> if i<2 then List.map (intro_symbol i) [0,1] else []
            _ -> []
    in
        Svg.svg 
        [ SA.style "width: 100%; height: 100vh;"
        , SA.viewBox (strf "% % % %" (List.map ff [bbox.x1, bbox.y1, bbox.x2, bbox.y2]))
        ]

        ([sea, islands]++intro_symbols++[controls model])

view_islands model =
    let
        grid = case model.stage of
            Intro i -> (get_intro_screen i).grid
            _ -> model.grid

        components = connected_components grid
        is_lake component = not (List.any (\(x,y) -> x==0 || y==0 || x==grid.width-1 || y==grid.width-1) component)
        diagonal_lakes = List.filter is_lake <| Grid.connected_components True ((==) Discarded) grid
        lakes = Grid.connected_components False ((==) Discarded) (Grid.fromCoords grid.width grid.height (\p -> if List.member p (List.concat diagonal_lakes) then Discarded else Free))
        cells = (Array.toList >> List.indexedMap (view_cell { model | grid = grid })) grid.cells
        move_msg dir = D.succeed (MoveKeyboardCell dir)
        noop = D.fail "nope"
        select_msg = case model.keyboard_cell of
            Just pos -> D.succeed (KeyboardSelectCell pos)
            _ -> noop
        decode_key = D.field "key" D.string |> D.andThen
            (\key -> case key of
                "ArrowLeft" -> move_msg (-1,0)
                "ArrowRight" -> move_msg (1,0)
                "ArrowUp" -> move_msg (0,-1)
                "ArrowDown" -> move_msg (0,1)
                "Enter" -> select_msg
                " " -> select_msg
                _ -> noop
            )
        shown_cells = Svg.g 
            [ tabindex 0
            , HE.on "keyup" decode_key
            ] 
            (case model.stage of
                Intro i -> if i>1 then cells else []
                _ -> cells
            )

        outlines = Svg.g [] (List.map (view_component { model | grid = grid }) (components++lakes))

        selected_grid = List.foldl (\pos -> set_cell pos Discarded) model.grid model.selected_cells
        selection = Svg.g [] [view_component {model | grid = selected_grid} (List.sort model.selected_cells)]
    in
        Svg.g
            []
            [outlines, selection, shown_cells]

equally_spaced a b gap =
    let
        n = ceiling ((b-a)/gap)
    in
        List.map (\i -> a + gap*(tf i)) (List.range 0 n)

view_sea model =
    let
        hue = 240
        sat = 30
        lum = 70
        fill = hsl hue sat lum
        bbox = bounding_box model
        bg = Svg.rect [SA.x "-10000", SA.y "-10000", SA.width "20000", SA.height "20000", SA.fill fill] []
        wave_y_period = 11
        speed = 0.00001
        time = (tf (posixToMillis model.time)) * speed
        dt = (time - (tf (floor time)))
        wave y =
            let
                ty = y+dt*wave_y_period
                d = strf "M % % Q % % % %" [ff bbox.x1, ff ty, ff ((bbox.x1+bbox.x2)/2), ff (ty + wave_y_period/2), ff bbox.x2, ff ty]
            in
                Svg.path [SA.stroke (hsl hue (sat*0.7) 75), strokeWidth, SA.d d, SA.fill "none"] []
    in
        Svg.g []
        ([ bg ])--++(List.map wave (equally_spaced (bbox.y1-wave_y_period) bbox.y2 wave_y_period)))

kind_of_component grid component =
    Maybe.withDefault Free (List.head component |> Maybe.andThen (\(x,y) ->
        let
            i = y * grid.width + x
        in
            (Array.get i grid.cells)
    ))

component_color kind =
    let
        hue = case kind of
            Free -> 120
            Discarded -> 240
            Claimed p -> player_color p

        sat = case kind of
            Free -> 0.0
            Discarded -> 30
            Claimed _ -> 55

        lum = case kind of 
            Discarded -> 70
            Claimed _ -> 55
            Free -> 55
    in
        (hue, sat, lum)

lerp a b t = a + (b-a) * t
clampf a t = ((max a t) - a) / (1-a)
clamp a b x = max 0 (min b x)

view_component model component =
    let
        kind = kind_of_component model.grid component
        (hue, sat, lum) = component_color kind
        t = trace_outline component
        turtle = corners_to_turtle t
        d2 = case List.head t of
            Just start -> turtle_to_outline start turtle
            _ -> ""
        d = case (List.head t, List.tail t) of
            (Just start, Just rest) -> "M "++(format_coords start)++" "++(String.join " " (List.map (\p -> "L "++(format_coords p)) rest))++" z"
            _ -> ""

        trace = Svg.path [SA.d d2, strokeWidth, SA.stroke (hsl hue sat (lum*0.5)), SA.fill (hsl hue sat lum)] []
        speed = 0.0001
        t1 = (tf (posixToMillis model.time)) * speed
        shore time =
            let
                n = floor time
                dt = (time - (tf n))
                sscale = scale / 10
                shorewidth = (lerp 1 3 dt) * sscale
                gap = (lerp 1 0 (clampf 0.5 dt)) * sscale
                dashes = (String.join " " (List.map (\i -> ((ff (sscale * (golden 3 20 10 (n+i))))++" "++(ff (sscale * (golden 1 25 5 (n+i)))))) (List.range 0 15)))
            in
                Svg.g []
                    [ Svg.path [SA.d d2, SA.strokeLinecap "round", SA.stroke (hsl 240 (30*0.7) 75), SA.strokeWidth (ff shorewidth), SA.strokeDasharray dashes] []
                    , Svg.path [SA.d d2, SA.stroke (hsl 240 30 70), SA.strokeWidth (ff (shorewidth-gap))] []
                    ]
        shores = if (t1 - (tf (floor t1))) < 0.5 then [shore (t1+17.5), shore t1] else [shore t1, shore (t1+17.5)]
        size = List.length component
        description = case kind of
            Free -> strf "Island of % %, belonging to nobody" [fi size, pluralise size "square" "squares"]
            Discarded -> "Sea"
            Claimed p -> strf "Island of % %, belonging to player %" [fi size, pluralise size "square" "squares", fi (p+1)]
    in
        Svg.g 
            [ ariaLabel description
            ]
            ((if kind == Discarded then [] else shores)++[trace])

line x1 y1 x2 y2 attr = Svg.line ([(SA.x1 << ff) (scale*x1), (SA.y1 << ff) (scale*y1), (SA.x2 << ff) (scale*x2), (SA.y2 << ff) (scale*y2)]++attr) []

view_cell : Model -> Int -> Cell -> Html.Html Msg
view_cell model i cell =
    let
        column = modBy model.grid.width i
        row = (i-column) // model.grid.width
        pos = (column, row)
        class = case cell of
            Free -> "free"
            Discarded -> "removed"
            Claimed p -> "claimed "++(fi p)
        components = connected_components model.grid
        component = (List.filter (List.member pos) >> List.head >> Maybe.withDefault []) components
        content = case cell of
            Claimed j -> player_symbol j
            _ -> ""
        is_selected = List.member pos model.selected_cells
        selectable = can_select pos model && List.length model.selected_cells > 0
        has_neighbour (dx,dy) = List.member (column+dx, row+dy) (if is_selected then model.selected_cells else component)

        up = has_neighbour (0,-1)
        down = has_neighbour (0,1)
        left = has_neighbour (-1,0)
        right = has_neighbour (1,0)

        n = row * model.grid.width + column

        (hue, sat, lum) = (0,0,100)

        opacity = if is_selected then 0.5 else if selectable then 0.3 else 0
        x = tf column
        y = tf row 
        l = 1 - 2*arc_size
        events = case model.stage of
            Playing -> 
                [ SE.onClick (ClickCell (column,row))
                ]
            _ -> []

        is_keyboard_cell = model.keyboard_cell == Just pos
        
        stroke = if is_keyboard_cell then (hsl hue sat lum) else "none"

        d = String.join " "
            [ if up || left then strf "M %" [format_fcoords (x,y)] else strf "M %" [format_fcoords (x + arc_size, y)]
            , if up || right then strf "L %" [format_fcoords (x+1,y)] else strf "L % a % 0 0 1 %" [format_fcoords (x+1-arc_size,y), format_fcoords (arc_size,arc_size), format_fcoords (arc_size, arc_size)]
            , if right || down then strf "L %" [format_fcoords (x+1,y+1)] else strf "L % a % 0 0 1 %" [format_fcoords (x+1,y+1-arc_size), format_fcoords (arc_size, arc_size), format_fcoords (-arc_size, arc_size)]
            , if down || left then strf "L %" [format_fcoords (x,y+1)] else strf "L % a % 0 0 1 %" [format_fcoords (x+arc_size, y+1), format_fcoords (arc_size, arc_size), format_fcoords (-arc_size, -arc_size)]
            , if up || left then strf "L %" [format_fcoords (x,y)] else strf "L % a % 0 0 1 %" [format_fcoords (x,y+arc_size), format_fcoords (arc_size, arc_size), format_fcoords (arc_size, -arc_size)]
            , "z"
            ]

        square = Svg.path 
            ([ SA.d d
            , (SA.opacity << ff) opacity
            , SA.fill <| hsl hue sat lum
            ]++events)
            []

        border_gap = golden 0.2 0.4 8 n
        borders : List (Svg.Svg Msg)
        borders = (List.filter Tuple.first >> List.map Tuple.second) 
            [ (right, line ((tf column)+1) ((tf row)+border_gap) ((tf column)+1) ((tf row)+1-border_gap) [SA.opacity "0.2", SA.stroke "black", SA.strokeLinecap "round", strokeWidth])
            , (down, line ((tf column)+border_gap) ((tf row)+1) ((tf column)+1-border_gap) ((tf row)+1) [SA.opacity "0.2", SA.stroke "black", SA.strokeLinecap "round", strokeWidth])
            ]

        description =
            let
                kind_description = case cell of
                    Discarded -> "Sea"
                    Free -> "Unclaimed"
                    Claimed p -> "Claimed by player "++(fi (p+1))
                selected_description = if is_selected then ", selected" else if selectable then ", selectable" else ""
            in
                strf "% (%,%)%" [kind_description, fi column, fi row, selected_description]

        keyboard_square = 
            if is_keyboard_cell then
                [ Svg.path
                    [ SA.d d
                    , SA.stroke stroke
                    , strokeWidth
                    , SA.fill "none"
                    , ariaLabel description
                    , ariaLive "polite"
                    ]
                    []
                ]
            else
                []
    in
        Svg.g 
            []
            ([square]++borders++keyboard_square)

onEnter : Msg -> Html.Attribute Msg
onEnter msg = HE.on "keyup" (D.andThen (\c -> if c == "Enter" || c == " " then D.succeed msg else D.fail "nope") (D.field "key" D.string))

controls model = 
    let
        bbox = bounding_box model

        can_dig = List.length model.selected_cells == 2

        m2 = 
            let
                grid = case model.stage of
                    Intro i -> (get_intro_screen i).grid
                    _ -> model.grid
            in
                { model | grid = grid } |> end_turn

        score_box m player =
            let
                width = 1
                height = 2
                x = -1.5
                y = if player == 0 then 0.5 else (tf m.grid.height) - height - 0.5
                highlight = case m.stage of
                    Finished res -> case res of
                        Winner p -> p.n == player
                        _ -> False
                    Playing -> m.current_player == player
                    _ -> False

                hue = player_color player
                sat = 55
                lum = 55
                score = player_score m player
                description = 
                    if highlight then
                        case m.stage of
                            Finished _ -> " They won!"
                            Playing -> " Their turn."
                            _ -> ""
                    else
                        ""
            in
                Svg.g
                [ ariaLabel ((strf "Player % has % %." [fi (player+1), fi score, pluralise score "point" "points"])++description)
                ]
                [ Svg.rect
                    [ (SA.x << ff) (scale * x)
                    , (SA.y << ff) (scale * y)
                    , (SA.width << ff) (scale * width)
                    , (SA.height << ff) (scale * height)
                    , (SA.rx << ff) (scale * 0.2)
                    , SA.fill (hsl hue sat lum)
                    , SA.stroke (if highlight then (hsl hue sat (lum*0.5)) else "none")
                    , strokeWidth
                    ]
                    []
                , Svg.text_
                    [ (SA.x << ff) (scale * (x + width/2))
                    , (SA.y << ff) (scale * (y + height/2))
                    , SA.dominantBaseline "central"
                    , SA.textAnchor "middle"
                    , (SA.fontSize << ff) (scale * 0.6)
                    , SA.fill (hsl hue sat (lum*0.3))
                    , SA.fontWeight (if highlight then "bold" else "normal")
                    , SA.fontFamily "sans-serif"
                    ]
                    [(Svg.text << fi) score]
                , Svg.text_
                    [ (SA.x << ff) (scale * (x+width/2))
                    , (SA.y << ff) (scale * y)
                    , SA.dominantBaseline "central"
                    , SA.textAnchor "middle"
                    , (SA.fontSize << ff) (scale * 1)
                    ]
                    [ Svg.text (player_symbol player) ]
                ]

        button_attrs msg label =
            [ SE.onClick msg
            , SA.cursor "pointer"
            , onEnter msg
            , SA.cursor "pointer"
            , tabindex 0
            , ariaLabel label
            , HA.attribute "role" "button"
            ]

        action_button x width msg text = Svg.g 
            (button_attrs msg text)
            [ Svg.rect 
                [ (SA.x << ff) (scale * x)
                , (SA.y << ff) (scale * (tf (model.grid.height )+0.5))
                , (SA.width << ff) (scale * width)
                , (SA.height << ff) (scale * 1)
                , (SA.rx << ff) (scale * 0.2)
                , SA.fill (hsl 240 50 80)
                , SA.stroke (hsl 240 50 50)
                , strokeWidth
                ]
                []
            , Svg.text_ 
                [ (SA.x << ff) (scale * (x+width/2))
                , (SA.y << ff) (scale * (tf (model.grid.height )+1))
                , SA.dominantBaseline "central"
                , SA.textAnchor "middle"
                , (SA.fontSize << ff) (scale * 0.6)
                , SA.fontFamily "sans-serif"
                ]
                [Svg.text text]
            ]

        arrow_button label dir x msg = 
            Svg.g
                (button_attrs msg label)
                [ Svg.path
                  [ SA.d (strf "M % l % l % l % z" (List.map format_fcoords [(x, (tf model.grid.height)+0.5), (dir,0.5), (-dir,0.5), (0,-1)]))
                  , SA.fill (hsl 0 0 80)
                  , SA.stroke (hsl 0 0 50)
                  , strokeWidth
                  , SA.strokeLinejoin "round"
                  ]
                  []
                ]

        current_player = info model model.current_player

        main_button msg text = (0, action_button 1 (tf (model.grid.width-2)) msg text)

        help_button = (3, action_button -1.5 1.0 ShowHelp "?")

        button = keyedNode "g" [] (List.map (\(i,e) -> ("button "++(fi i), e)) <| case model.stage of
            Playing -> if List.length model.selected_cells == 2 then [main_button FinishMove "Dig", help_button] else [help_button]
            Finished _ -> [main_button StartAgain "Start again"]
            Intro i -> case i of
                0 ->
                    [ (1,arrow_button "next page" 1 1 NextIntro)
                    , (2,action_button 3 4 Start "Start digging!")
                    ]
                6 ->
                    [ (0,arrow_button "previous page" -1 0 PrevIntro)
                    , (1,action_button 1 4 Start "Start digging!")
                    ]
                _ ->
                    [ (0,arrow_button "previous page" -1 0 PrevIntro)
                    , (1,arrow_button "next page" 1 1 NextIntro)
                    , (2,action_button 3 4 Start "Start digging!")
                    ]
            )

        plain_text t = [Svg.text t]

        hint_text =
            case model.stage of
                Finished res -> plain_text <| case res of
                    Draw -> "It's a draw!"
                    Winner p -> (p.name)++" wins!"
                Playing -> plain_text <|
                    case List.length model.selected_cells of
                        0 -> "Select two adjacent pieces to remove."
                        1 -> "Select another piece to remove."
                        2 -> "Press \"Dig\" to remove these pieces."
                        _ -> ""
                Intro i -> (get_intro_screen i).text

        hint = Svg.text_
            [ (SA.x << ff) (scale * (tf model.grid.width)/2)
            , (SA.y << ff) (scale * -1)
            , SA.dominantBaseline "central"
            , SA.textAnchor "middle"
            , (SA.fontSize << ff) (scale * 0.4)
            , HA.attribute "role" "status"
            , ariaLive "polite"
            ]
            (hint_text)

        scores = case model.stage of
            Intro i -> if i == 6 then [score_box m2 0, score_box m2 1] else []
            _ -> [score_box model 0, score_box model 1]
    in
        Svg.g [] (scores++[hint, button])

update msg model = 
    case msg of
        Noop -> model
        ClickCell pos -> click_cell pos model |> clear_keyboard_cell
        FinishMove -> finish_move model
        ShowHelp -> { model | stage = Intro 0 }
        Start -> { model | stage = Playing }
        StartAgain -> { init | stage = Playing }
        Frame t -> { model | time = t }
        NextIntro -> next_intro model
        PrevIntro -> prev_intro model
        FocusCells -> case model.keyboard_cell of
            Nothing -> { model | keyboard_cell = Just (0,0) }
            _ -> model
        MoveKeyboardCell dir -> move_keyboard_cell dir model
        KeyboardSelectCell pos -> click_cell pos model

clear_keyboard_cell model = { model | keyboard_cell = Nothing }

move_keyboard_cell (dx,dy) model =
    let
        (ox,oy) = Maybe.withDefault (0,0) model.keyboard_cell
        x = clamp 0 (model.grid.width-1) (ox+dx)
        y = clamp 0 (model.grid.height-1) (oy+dy)
    in
        { model | keyboard_cell = Just (x,y) }

prev_intro model = 
    let
        nstage = case model.stage of
            Intro i -> Intro (max 0 (i-1))
            _ -> model.stage
    in
        { model | stage = nstage }
next_intro model = 
    let
        nstage = case model.stage of
            Intro i -> if i+1 >= Array.length intro_screens then Playing else Intro (i+1)
            _ -> model.stage
    in
        { model | stage = nstage }

click_cell pos model = case (get_cell pos model.grid) of
        Just Free -> select_cell pos model
        _ -> model

coords_adjacent (x1,y1) (x2,y2) = (x1==x2 && (abs (y1-y2)==1)) || ((abs (x1-x2)==1) && y1==y2)

select_cell pos model = 
    if List.member pos model.selected_cells then
        { model | selected_cells = List.filter ((/=) pos) model.selected_cells }
    else
        if can_select pos model then
            { model | selected_cells = pos :: model.selected_cells }
        else
            model

dig player grid cells = List.foldl (\pos -> \g -> set_cell pos Discarded g) grid cells |> claim_components player

finish_move model =
    let
        ngrid = dig model.current_player model.grid model.selected_cells
    in
        { model | grid = ngrid, selected_cells = [] } |> end_turn

claim_components current_player grid =
    let
        components = connected_components grid

        is_claimable component =
            let
                n = List.length component
            in
                modBy 2 n == 1 && n<10 && kind_of_component grid component == Free

        claimable_components = List.filter is_claimable components
    in
        List.foldl (\component -> \g -> List.foldl (\pos -> \g2 -> set_cell pos (Claimed current_player) g2) g component) grid claimable_components

end_turn model =
    let
        next_player = modBy 2 (model.current_player+1)
        is_finished = (not << List.any ((==) Free) << Array.toList) model.grid.cells 

        result = 
            let
                p0 = info model 0
                p1 = info model 1
            in
                if p0.score > p1.score then Winner p0 else if p1.score > p0.score then Winner p1 else Draw

    in
        { model | current_player = next_player, stage = if is_finished then Finished result else Playing }

player_score model player = (Array.length << Array.filter ((==) (Claimed player))) model.grid.cells

player_symbols =
    ["🐱"
    ,"🐶"
    ]

nth list default i = Maybe.withDefault default (Array.get i (Array.fromList list))

player_symbol = nth player_symbols "?"

player_colors =
    [ 120.0
    , 0.0
    ]
player_color = nth player_colors 0.0

view_scores model = 
    table []
    [ view_score model 0
    , view_score model 1
    ]

view_score model player =
    let
        score = player_score model player
        name = player_symbol player
        is_current = player == model.current_player
    in
        tr [HA.style "font-weight" (if is_current then "bold" else "normal")]
        [ td [] [text name]
        , td [] [text <| fi score]
        ]

type alias PlayerInfo =
    { score : Int
    , name : String
    , n : Int
    }

type GameStage
    = Intro Int
    | Playing
    | Finished Result

type Result
    = Draw
    | Winner PlayerInfo

info : Model -> Player -> PlayerInfo
info model player = 
    { score = player_score model player
    , name = player_symbol player
    , n = player
    }

type Corner
    = TL
    | TR
    | BL
    | BR

type Movement
    = Forward
    | TurnLeft
    | TurnRight

type Direction
    = Up
    | Down
    | Left
    | Right

type Face
    = Neutral
    | Happy
    | Sad

next_corner : (Coords -> Bool) -> (Coords,Corner) -> (Coords,Corner)
next_corner filled ((x,y),corner) = case corner of
    TL ->
        case (filled (x-1,y), filled (x-1,y-1), filled (x,y-1)) of
            (True,True,True) -> ((x,y),corner) -- impossible! landlocked
            (False,True,True) -> ((x-1,y-1),BL)
            (_,False,True) -> ((x,y-1),TL)
            (_,_,False) -> ((x,y),TR)
    TR ->
        case (filled (x,y-1), filled (x+1,y-1), filled (x+1,y)) of
            (True,True,True) -> ((x,y),corner) -- impossible! landlocked
            (False,True,True) -> ((x+1,y-1),TL)
            (_,False,True) -> ((x+1,y),TR)
            (_,_,False) -> ((x,y),BR)
    BR ->
        case (filled (x+1,y), filled (x+1,y+1), filled (x,y+1)) of
            (True,True,True) -> ((x,y),corner) -- impossible! landlocked
            (False,True,True) -> ((x+1,y+1),TR)
            (_,False,True) -> ((x,y+1),BR)
            (_,_,False) -> ((x,y),BL)
    BL ->
        case (filled (x,y+1), filled (x-1,y+1), filled (x-1,y)) of
            (True,True,True) -> ((x,y),corner) -- impossible! landlocked
            (False,True,True) -> ((x-1,y+1),BR)
            (_,False,True) -> ((x-1,y),BL)
            (_,_,False) -> ((x,y),TL)

type TraceState
    = TracingOutline (Coords,Corner) (List Coords)
    | FinishedOutline (List Coords)

trace_step : Coords -> (Coords -> Bool) -> (Coords,Corner) -> List Coords -> TraceState
trace_step start filled (pos,corner) path =
    if corner == TL && start == pos && path /= [] then
        FinishedOutline path
    else
        let
            ns = next_corner filled (pos,corner)
            (x,y) = pos
            cpos = case corner of
                TL -> pos
                TR -> (x+1,y)
                BR -> (x+1,y+1)
                BL -> (x,y+1)
        in
            TracingOutline ns (cpos::path)


trace_outline : List Coords -> List Coords
trace_outline cells =
    let
        filled pos = List.member pos cells
    in
        List.reverse (case List.head cells of
            Just pos -> 
                let
                    go : TraceState -> List Coords
                    go s = case s of
                        TracingOutline ss path -> go (trace_step pos filled ss path)
                        FinishedOutline path -> path
                in
                    go (TracingOutline (pos,TL) [])

            Nothing -> []
        )

trace_next_step model =
    case model.trace_state of
        FinishedOutline _ -> model
        TracingOutline ss path ->
            case (List.head << connected_components) model.grid of
                Just cells -> case List.head cells of
                    Just start ->
                        let
                            filled pos = List.member pos cells
                        in
                            { model | trace_state = trace_step start filled ss path }
                    _ -> model
                _ -> model

trace_state_path t = case t of
    FinishedOutline path -> path
    TracingOutline ss path -> path

lefter dir = case dir of
    Up -> (-1,0)
    Down -> (1,0)
    Left -> (0,1)
    Right -> (0,-1)
righter dir = case dir of
    Up -> (1,0)
    Down -> (-1,0)
    Left -> (0,-1)
    Right -> (0,1)
turn_left dir = case dir of
    Up -> Left
    Left -> Down
    Down -> Right
    Right -> Up
turn_right dir = case dir of
    Up -> Right
    Right -> Down
    Down -> Left
    Left -> Up

forwarder dir = case dir of
    Up -> (0,-1)
    Down -> (0,1)
    Left -> (-1,0)
    Right -> (1,0)

apply_movement movement = case movement of
    TurnLeft -> turn_left
    TurnRight -> turn_right
    Forward -> identity

corners_to_turtle : List Coords -> List Movement
corners_to_turtle path =
    let

        vector_to_movements dir v = if v==lefter dir then [TurnLeft,Forward] else if v==righter dir then [TurnRight,Forward] else [Forward]

        step (x2,y2) ((x1,y1),dir,out) =
            let
                v = (x2-x1,y2-y1)
                movements = vector_to_movements dir v
                ndir = List.foldl apply_movement dir movements
            in
                ((x2,y2),ndir,out++movements)
    in
        case List.head path of
            Just start -> List.foldl step (start,Right,[]) ((List.drop 1 path)++[start]) |> \(a,b,c) -> c
            Nothing -> []
            
arc_size = 0.4

turtle_to_outline : Coords -> List Movement -> String
turtle_to_outline start movements =
    let
        (startx,starty) = start
        (x1,y1) = ((tf startx) + arc_size, tf starty)
        step movement state =
            let
                (x,y) = state.pos
                dir = state.dir
                d = state.d
                last_move = state.last_move
                ndir = apply_movement movement dir
                (fx,fy) = forwarder dir
                turn turner =
                    let
                        (tx,ty) = turner dir
                    in
                        ((fx+tx)*arc_size, (fy+ty)*arc_size)
                (dx,dy) = case movement of 
                    Forward -> case last_move of
                        Forward -> (fx,fy)
                        _ -> (fx * (1-2*arc_size), fy * (1-2*arc_size))
                    TurnLeft -> turn lefter
                    TurnRight -> turn righter
                sweep  = case movement of
                    TurnLeft -> "0"
                    TurnRight -> "1"
                    _ -> ""
                dpart = case movement of
                    Forward -> strf "l % %" [ff (scale*dx), ff (scale*dy)]
                    _ -> strf "a % % 0 0 % % %" [ff (scale*arc_size), ff (scale*arc_size), sweep, ff (scale*dx), ff (scale*dy)]
                npos = (x+dx, y+dy)
            in
               { last_move = movement, dir = ndir, pos = npos, d = d++" "++dpart }

    in
        List.foldl step {last_move = TurnRight, dir = Right, pos = (x1,y1),d = strf "M % %" [ff (scale*x1), ff (scale*y1)]} movements |> step TurnRight |> \s -> s.d
