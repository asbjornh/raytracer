type Matrix = float
type Material = float

type IShape =
  abstract Transform: Matrix
  abstract Material: Material

type Sphere =
  {
    Transform: Matrix;
    Material: Material
  }
  interface IShape with
    member this.Transform = this.Transform
    member this.Material = this.Material

type Shape =
  | Sphere of Sphere

let intersectSphere (s: IShape) =
  s.Transform * 0.2

type Intersection = { object: IShape }

let intersection (s: IShape) =
  { object = s }

let intersect (s: Shape) =
  match s with
  | Sphere s -> intersectSphere s

let sphere = {
  Transform = 2.;
  Material = 3.;
}

let test = intersectSphere sphere
let test2 = intersection sphere



// type Car = {
//     Registration: string
//     Owner: string
//     Wheels: int
//     customAttribute1: string
//     customAttribute2: string
// }

// type Truck = {
//    Registration: string
//    Owner: string
//    Wheels: int
//    customField5: string
//    customField6: string
// }

// type Bike = {
//     Owner: string
//     Color: string
// }

// type Vehicle = {
//     Registration: string
//     Owner: string
// }

// let inline someComplexFun v  =
//    let owner =  (^v: (member Owner: string)(v)) 
//    let registration = (^v: (member Registration: string)(v))
//    {Registration = registration; Owner = owner}

// let car = {Car.Registration = "xyz"; Owner = "xyz"; Wheels = 3; customAttribute1= "xyz"; customAttribute2 = "xyz"}
// let truck = {Truck.Registration = "abc"; Owner = "abc"; Wheels = 12; customField5 = "abc"; customField6 = "abc"}
// let bike = {Owner = "hell's angels"; Color = "black"}
// let a = someComplexFun car //val it : Vehicle = {Registration = "xyz";
//                    //Owner = "xyz";}
// let b = someComplexFun truck //val it : Vehicle = {Registration = "abc";
//                      //Owner = "abc";}
// let c = someComplexFun bike
