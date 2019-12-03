module UnitsTest

open Expecto
open Units

[<Tests>]

let tests =
  testList "Tests for Unit" [
    testCase "point() creates tuples with w=1" <| fun _ ->
      let p = point 4.0 -4.0 3.0 
      Expect.equal p (4.0, -4.0, 3.0, 1.0) ""

    testCase "vector() creates tuples with w=0" <| fun _ ->
      let v = vector 4.0 -4.0 3.0
      Expect.equal v (4.0, -4.0, 3.0, 0.0) ""

    testCase "Adding two tuples" <| fun _ ->
      let a = (3.0, -2.0, 5.0, 1.0)
      let b = (-2.0, 3.0, 1.0, 0.0)
      Expect.equal (add a b) (1.0, 1.0, 6.0, 1.0) ""

    testCase "Subtracting two points" <| fun _ ->
      let a = point 3.0 2.0 1.0
      let b = point 5.0 6.0 7.0
      Expect.equal (subtract a b) (vector -2.0 -4.0 -6.0) ""

    testCase "Subtracting a vector from a point" <| fun _ ->
      let a = point 3.0 2.0 1.0
      let b = vector 5.0 6.0 7.0
      Expect.equal (subtract a b) (point -2.0 -4.0 -6.0) ""

    testCase "Subtracting two vectors" <| fun _ ->
      let a = vector 3.0 2.0 1.0
      let b = vector 5.0 6.0 7.0
      Expect.equal (subtract a b) (vector -2.0 -4.0 -6.0) ""

    testCase "Subtracting a vector from the zero vector" <| fun _ ->
      let zero = vector 0.0 0.0 0.0
      let a = vector 1.0 -2.0 3.0
      Expect.equal (subtract zero a) (vector -1.0 2.0 -3.0) ""

    testCase "Negating a tuple" <| fun _ ->
      let a = (1.0, -2.0, 3.0, -4.0)
      Expect.equal (negate a) (-1.0, 2.0, -3.0, 4.0) ""

    testCase "Multiplying a tuple by a scalar" <| fun _ ->
      let a = (1.0, -2.0, 3.0, -4.0)
      Expect.equal (multiply 3.5 a) (3.5, -7.0, 10.5, -14.0) ""

    testCase "Multiplying a tuple by a fraction" <| fun _ ->
      let a = (1.0, -2.0, 3.0, -4.0)
      Expect.equal (multiply 0.5 a) (0.5, -1.0, 1.5, -2.0) ""

    testCase "Dividing a tuple by a scalar" <| fun _ ->
      let a = (1.0, -2.0, 3.0, -4.0)
      Expect.equal (divide 2.0 a) (0.5, -1.0, 1.5, -2.0) ""
  ]

    // testCase "universe exists (╭ರᴥ•́)" <| fun _ ->
    //   let subject = true
    //   Expect.isTrue subject "I compute, therefore I am."

    // testCase "when true is not (should fail)" <| fun _ ->
    //   let subject = false
    //   Expect.isTrue subject "I should fail because the subject is false"

    // testCase "I'm skipped (should skip)" <| fun _ ->
    //   Tests.skiptest "Yup, waiting for a sunny day..."

    // testCase "I'm always fail (should fail)" <| fun _ ->
    //   Tests.failtest "This was expected..."

    // testCase "contains things" <| fun _ ->
    //   Expect.containsAll [| 2; 3; 4 |] [| 2; 4 |]
    //     "This is the case; {2,3,4} contains {2,4}"

    // testCase "contains things (should fail)" <| fun _ ->
    //   Expect.containsAll [| 2; 3; 4 |] [| 2; 4; 1 |]
    //     "Expecting we have one (1) in there"

    // testCase "Sometimes I want to ༼ノಠل͟ಠ༽ノ ︵ ┻━┻" <| fun _ ->
    //   Expect.equal "abcdëf" "abcdef" "These should equal"

    // test "I am (should fail)" {
    //   "╰〳 ಠ 益 ಠೃ 〵╯" |> Expect.equal true false
    // }