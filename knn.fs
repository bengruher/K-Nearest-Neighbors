(* 
@author Ben Gruher
@file pa3.fs
@see "Seattle University, CPSC 3400, Winter 2019"
*)

module knn

///finds distance between two points on the x,y plane. takes two tuples of coordinates and returns float distance value
let euclidianDistance a b =
    let x1, y1 = a
    let x2, y2 = b
    sqrt((x1-x2)**2.0 + (y1-y2)**2.0)



///returns a function that finds the k nearest neighbors when given a tuple query
///parameters: int k (number of nearest neighbors), distance function, data (list of tuples with feature tuple and label)
let knn k distance (data:'a list) =
    ///replaces furthest instance from query with new instance if it is closer
    ///returns: list of k instances
    ///parameters: list of instances, next instance, query
    let rec insertTopK nearests potentialNearest query = 
        match nearests with 
        | [] -> []
        | (a,b)::tail -> 
            match potentialNearest with
            | (x,y) -> 
                if distance x query < distance a query then insertTopK (potentialNearest::tail) (a,b) query
                else (a,b)::(insertTopK tail potentialNearest query)
    let firstKInstances = data.[..k-1] 

    ///function called by the closure function. returns a list of k closest instances when provided a dataset and a query
    let rec inner dataList query = 
            match dataList with  
            | [] -> firstKInstances
            | head::tail -> 
                insertTopK (inner tail query) head query  
    (fun query ->
        inner data.[k..] query)

let rec sum instances = 
    match instances with
    | [] -> 0.0
    | (a,b)::tail ->
        b + sum tail

let avgInstances instances = 
    let total = sum instances
    total/float(List.length instances)





