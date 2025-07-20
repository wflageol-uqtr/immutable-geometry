let list_reduce fn list = List.fold_left fn (List.hd list) (List.tl list)

class point ?(color="black") x y =
  object (self)
    val x = x
    val y = y 
    val color = color
    
    method print =
      Printf.printf "Point(%d, %d, %s)\n" x y color;
      
    method x = x
    method y = y
    method center = new point x y
      
    method move dx dy =
      {< x = x + dx; y = y + dy >}
      
    method rotate =
      self#rotate_around self#center
      
    method rotate_around origin =
      let dx = x - origin#x in
      let dy = y - origin#y in 
      
      {< x = -dy + origin#x; y = dx + origin#y >}
end

class size w h =
  object (self)
    val w = w
    val h = h
    
    method print =
      Printf.printf "Size(%d ,%d)\n" w h;
      
    method w = w
    method h = h
      
    method scale value =
      {< w = float_of_int w *. value |> truncate; h = float_of_int h *. value |> truncate >}
      
    method fit_into (target: size) =
      let scale = min (float_of_int target#h /. float_of_int h)
                      (float_of_int target#w /. float_of_int w) in
      self#scale scale
end

class rectangle ?(color="black") x y w h =
  object (self)
    inherit point ~color:color x y
    inherit size w h
    
  method print =
    Printf.printf "Rectangle(%d, %d, %d, %d, %s)\n" x y w h color;
    
  method left = x
  method right = x + w
  method top = y
  method bottom = y + h
  method center = new point (self#x + self#w / 2) (self#y + self#h / 2)
    
  method rotate_around origin =
    (* We only need to rotate the upper triangle of the rectangle to rebuild it, so we only need 3 points. *)
    let top_left = new point self#x self#y in
    let top_right = new point (self#x + self#w) self#y in
    let bot_left = new point self#x (self#y + self#h) in
    
    let p1 = top_left#rotate_around origin in
    let p2 = top_right#rotate_around origin in
    let p3 = bot_left#rotate_around origin in
    
    let y_list = [p1#y; p2#y; p3#y] in
    let x_list = [p1#x; p2#x; p3#x] in
    
    let top = list_reduce min y_list in
    let bot = list_reduce max y_list in
    let left = list_reduce min x_list in
    let right = list_reduce max x_list in
    
    {< x = left;
       y = top;
       w = right - left;
       h = bot - top >}
       
  method private does_intersect (target: rectangle) =
    target#left < self#right
    || target#top < self#bottom
    || self#left < target#right
    || self#top < target#bottom
    
  method intersect (target: rectangle) =
    if self#does_intersect target
    then let top = max self#top target#top in
         let bot = min self#bottom target#bottom in
         let left = max self#left target#left in
         let right = min self#right target#right in
         
         Some {< x = left; y = top; w = (right - left); h = (bot - top) >}
    else None
end

let p = new point ~color:"yellow" 2 2;;
let p2 = p#move 1 2;;
let p3 = p#rotate;;
let p4 = p#rotate_around (new point 0 0);;

let s = new size 4 6;;
let s2 = s#scale 3.0;;
let s3 = s#fit_into (new size 3 3);;

let r = new rectangle ~color:"green" 2 2 3 4;;
let r2 = r#move 1 2;;
let r3 = r#rotate;;
let r4 = r#rotate_around (new point 0 0)
let r5 = r#scale 5.0;;
let r6 = r#fit_into (new size 8 8);;
let r7 = r#intersect (new rectangle 4 3 3 2);;

p#print;;
p2#print;;
p3#print;;
p4#print;;

s#print;;
s2#print;;
s3#print;;

r#print;;
r2#print;;
r3#print;;
r4#print;;
r5#print;;
r6#print;;

match r7 with
| Some r -> r#print
| None -> Printf.printf "None\n";;