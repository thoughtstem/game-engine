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
(respawn edge) -> func?
  edge : something?
procedure
(set-speed spd) -> func?
  spd : integer?
(set-direction dir) -> func?
  dir : integer?
