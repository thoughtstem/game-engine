33
((3) 0 () 0 () () (h ! (equal)))
procedure
(sprite->entity imgs                             
                #:name name                      
                #:position position              
                #:components components ...) -> entity?
  imgs : (or/c image? (listof image?))
  name : string?
  position : posn?
  components : component?
procedure
(change-sprite sprite-or-func) -> func?
  sprite-or-func : (or image? func?)
procedure
(sheet->sprite sheet                 
               #:row r               
               #:column c            
               #:row-number rnum     
               #:speed delay)    -> sprite?
  sheet : image?
  r : integer?
  c : integer?
  rnum : integer?
  delay : integer?
procedure
(sheet->rainbow-hue-sheet sheet) -> sheet?
  sheet : image?
(sheet->rainbow-tint-sheet sheet) -> sheet?
  sheet : image?
procedure
(component? x) -> boolean?
  x : any/c
procedure
(new-component struct? update) -> void?
  struct? : (-> any/c boolean?)
  update : (-> game? entity? component? entity?)
procedure
(key-movement speed) -> component?
  speed : integer?
procedure
(posn x y) -> component?
  x : integer?
  y : integer?
procedure
(after-time ticks fun) -> component?
  ticks : integer?
  fun : (-> game? entity? component? entity?)
procedure
(on-collide name fun) -> component?
  name : string?
  fun : (-> game? entity? component?)
procedure
(every-tick func) -> component?
  func : (-> game? entity? component?)
procedure
(circle-collider radius) -> component?
  radius : number?
procedure
(health amount) -> component?
  amount : integer?
procedure
(physical-collider) -> component?
procedure
(detect-collide entity-name1     
                entity-name2     
                func)        -> component?
  entity-name1 : string?
  entity-name2 : string?
  func : func?
procedure
(on-edge edge #:offset off func) -> component?
  edge : symbol?
  off : integer?
  func : func?
procedure
(detect-edge name edge func) -> component?
  name : string?
  edge : symbol?
  func : func?
procedure
(move) -> func?
procedure
(move-dir-spd #:dir dir #:speed speed) -> func?
  dir : integer?
  speed : integer?
procedure
(move-right #:speed spd) -> func?
  spd : integer?
(move-down #:speed spd) -> func?
  spd : integer?
(move-left #:speed spd) -> func?
  spd : integer?
(move-up #:speed spd) -> func?
  spd : integer?
procedure
(move-random #:speed spd) -> func?
  spd : integer?
procedure
(move-up-and-down #:min small      
                  #:max large      
                  #:speed spd) -> func?
  small : integer?
  large : integer?
  spd : integer?
procedure
(spin #:speed spd) -> func?
  spd : integer?
procedure
(go-to x y) -> func?
  x : integer?
  y : integer?
procedure
(go-to-random min-x max-x min-y max-y) -> func?
  min-x : integer?
  max-x : integer?
  min-y : integer?
  max-y : integer?
procedure
(go-to-pos pos) -> func?
  pos : symbol?
(go-to-pos-inside pos) -> func?
  pos : symbol?
procedure
(respawn edge) -> func?
  edge : symbol?
procedure
(set-speed spd) -> func?
  spd : integer?
(set-direction dir) -> func?
  dir : integer?
