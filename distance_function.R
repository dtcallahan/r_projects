#creating a function to calculate and print the distance between two coordinate pairs
#euclidean and manhattan

distance.calc = function(x1,y1,x2,y2)
  {ed = sqrt((x2-x1)^2 + (y2-y1)^2)
  md = abs(x1-x2) + abs(y1-y2)
  cat ("Euclidean distance:", ed,
     "\nManhattan distance:", md)
  }

distance.calc(145,11,3,-4)
