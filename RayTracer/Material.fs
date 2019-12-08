module Material

open Color

type Material = {
  color: Color;
  ambient: float;
  diffuse: float;
  specular: float;
  shininess: float;
}

let material () = {
  color = color 1. 1. 1.
  ambient = 0.1
  diffuse = 0.9
  specular = 0.9
  shininess = 200.
}
