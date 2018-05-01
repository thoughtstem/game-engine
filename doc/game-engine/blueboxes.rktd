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
(sheet->sprite img                   
               #:rows r              
               #:columns c           
               #:row-number rnum     
               #:speed delay)    -> animated-sprite?
  img : image?
  r : integer?
  c : integer?
  rnum : integer?
  delay : integer?
procedure
(sheet->rainbow-hue-sheet img) -> image?
  img : image?
procedure
(sheet->rainbow-tint-sheet img) -> image?
  img : image?
procedure
(component? x) -> boolean?
  x : any/c
procedure
(new-component struct? update) -> void?
  struct? : (-> any/c boolean?)
  update : (-> game? entity? component? entity?)
procedure
(static) -> component?
procedure
(key-movement speed) -> component?
  speed : integer?
procedure
(posn x y) -> component?
  x : integer?
  y : integer?
procedure
(after-time ticks func) -> component?
  ticks : integer?
  func : (-> game? entity? component? entity?)
procedure
(do-every ticks func) -> component?
  ticks : integer?
  func : (-> game? eneity? component? entity?)
procedure
(on-start func) -> component?
  func : (-> game? entity? component?)
procedure
(on-collide entity-name func) -> component?
  entity-name : string?
  func : (-> game? entity? component?)
procedure
(detect-collide entity-name1     
                entity-name2     
                func)        -> component?
  entity-name1 : string?
  entity-name2 : string?
  func : (-> game? entity? component?)
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
(spawner sprite time) -> component
  sprite : entity?
  time : integer?
procedure
(physical-collider) -> component?
procedure
(speed value) -> component?
  value : integer?
(direction value) -> component?
  value : integer?
procedure
(counter count) -> component?
  count : integer?
procedure
(do-many ...) -> func?
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
  pos : something?
(go-to-pos-inside pos) -> func?
  pos : something?
procedure
(respawn loc) -> func?
  loc : something?
procedure
(set-speed amount) -> func?
  amount : integer?
(set-direction amount) -> func?
  amount : integer?
(set-counter amount) -> func?
  amount : integer?
procedure
(random-direction min max) -> func?
  min : integer?
  max : integer?
(random-speed min max) -> func?
  min : integer?
  max : integer?
procedure
(change-ai-speed-by delta) -> func?
  delta : integer?
(change-speed-by delta) -> func?
  delta : integer?
(change-direction-by delta) -> func?
  delta : integer?
(change-counter-by delta) -> func?
  delta : integer?
procedure
(change-sprite sprite-or-func) -> func?
  sprite-or-func : (or/c sprite? func?)
