module Material

open Color

type Material = {
  mutable color: Color;
  mutable ambient: float;
  mutable diffuse: float;
  mutable specular: float;
  mutable shininess: float;
}

let material () = {
  color = color 1. 1. 1.
  ambient = 0.1
  diffuse = 0.9
  specular = 0.9
  shininess = 200.
}
