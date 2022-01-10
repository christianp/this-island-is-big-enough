module Grid exposing (Grid, Coords, fromString, fromCoords, fill, get_row, set_cell, get_cell, coords, connected_components)

import Array exposing (Array)
import Set

type alias Coords = (Int,Int)

type alias Grid x = 
    { width: Int 
    , height: Int
    , cells: Array x
    }

fromCoords width height fn =
    let
        decide i =
            let
                x = modBy width i
                y = (i - x) // width
            in
                fn (x,y)
    in
        Grid width height ((Array.fromList << List.map decide) (List.range 0 (width*height-1)))

fromString str parse_cell =
    let
        rows = (String.split "\n" (String.trim str))
        height = List.length rows
        width = Maybe.withDefault 0 (Maybe.map String.length (List.head rows))
        cells = (String.join "" >> String.toList >> (List.map parse_cell) >> Array.fromList) rows
    in
        { width = width
        , height = height
        , cells = cells
        }

fill width height value = 
    { width = width
    , height = height
    , cells = Array.repeat (width*height) value
    }

get_row : Int -> Grid x -> Array x
get_row row grid = Array.slice (row*grid.width) ((row+1)*grid.width) grid.cells

set_cell : Coords -> x -> Grid x -> Grid x
set_cell (x,y) value grid =  
    let
        i = y*grid.width + x
    in
        { grid | cells = (Array.indexedMap (\j -> \c -> if j==i then value else c) grid.cells) }

get_cell : Coords -> Grid x -> Maybe x
get_cell (x,y) grid = 
    let
        i = y*grid.width + x
    in
        if x>=0 && x<grid.width && y>=0 && y<grid.height then
            Array.get i grid.cells
        else
            Nothing

coords grid = List.concatMap (\y -> List.map (\x -> (x,y)) (List.range 0 (grid.width-1))) (List.range 0 (grid.height-1))


connected_components : Bool -> (x -> Bool) -> Grid x -> List (List Coords)
connected_components allow_diagonals include grid = 
    let
        init_components : Array (Maybe Int)
        init_components = Array.indexedMap (\i -> \c -> if include c then Just i else Nothing) grid.cells

        merge_components : y -> y -> Array y -> Array y
        merge_components a b components = Array.map (\c -> if c==b then a else c) components
        check_edge p1 p2 components = 
            let
                g : Grid (Maybe Int)
                g = Grid grid.width grid.height components
                a = get_cell p1 g
                b = get_cell p2 g
            in
                case (a,b) of
                    (Just (Just ja), Just (Just jb)) -> if ja /= jb then merge_components (Just ja) (Just jb) components else components
                    _ -> components
        check_pos pos =
            let
                check p2 components = check_edge pos p2 components
                (x,y) = pos
                straights = check (x-1,y) >> check (x+1,y) >> check (x,y-1) >> check (x,y+1)
            in
                if allow_diagonals then
                    check (x-1,y-1) >> check (x-1,y+1) >> check (x+1,y+1) >> check (x-1,y+1) >> straights
                else
                    straights

        component_per_cell = (Array.toList << List.foldl check_pos init_components) (coords grid)
        unique_components = (List.filterMap identity >> Set.fromList >> Set.toList) component_per_cell
        final_components = List.map (\id -> List.filterMap identity <| List.map2 (\component -> \pos -> if component == Just id then Just pos else Nothing) component_per_cell (coords grid)) unique_components
    in
        final_components

