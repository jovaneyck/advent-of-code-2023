#r "nuget: Plotly.NET"

open Plotly.NET

let points =
    [ for x in 0..10 do
          for y in 0..10 do
              for z in 0..10 do
                  (x, y, z) ]

Chart3D.Chart.Point3D(points)
|> Chart.withTitle "Hello World!"
|> Chart.show
