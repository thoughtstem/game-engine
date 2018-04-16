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
  fun : (-> game? entity? component? entity?)
procedure
(circle-collider radius) -> component?
  radius : number?
procedure
(health amount) -> component?
  amount : integer?
procedure
(physical-collider) -> component?
