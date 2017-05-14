# AGP
Art Gallery Problem

So we are building a stimulator for the art gallery problem in Drracket
So we have to take input from user the number of sides, then points, as we have decided two options for it
i.e. either you can use a graph based input scheme or simply give list of points to us

Storing of vertices ie points is done using classes not struct. Because classes provide you the ability to create member fucntions hich appear soothing to eyes. (is-merge-vertex? ) (is-split-vertex? ) are member function s of this vertex class. Color is also attribute for the object vertex.
Line is a struct. 
Now we take input set of points and create instance of Polygon class. Plan to use (in-cycle) data structure of racket to store the points (just a cyclic list!!!). Polygon class will have various member functions like angle for each vertex, then questions like (is-orthogonal? ) (is-convex? )  and some others. (to have edits). Check whether an edge is parallel to x axis then we have to do our JUGAAD (point 0.01 in both vertices). Plolygon class rougly initializes by number of sides and an array of clockwise vertices (array A). We will then create array of decreasing y coordinate points (array B). Both are required in our functions. 
Triangle class is inherited from y monotone polygons

Then if polygon is orthogonal or convex or something special that could be optimized we plan to make classes for each of these inherited from the original polygon class. (inhertence-maxx!!! )



FOR NORMAL POLYGONS
Now we will implement the algorithm, using input as polygonal class we will first perform triangulation. 
Performing Triangulations :
  Breaking into y monotone polygons
  We need 
    SLS which is a list
    helper function takes edge (a line), polygon and return vertex.
    To check regular vertex left or right check angle bisector
    It return a list/array of y monotone polygons.
  NOW TRIANGULATING Y MONOTONE 
    We have stacks in this part.2 stacks are there viz. a left stack and a right stack. stacks contains vertices :
     [[[[( a) both stack either have 1 element or
      b) one stack has 1 and other has more than 1.]]]]]]]]]]]]]]]]]
    We are   
      
      d
