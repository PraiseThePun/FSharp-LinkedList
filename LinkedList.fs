module LinkedList

type node = { mutable previous: node option; mutable next: node option; value: int}
type LinkedList = {mutable first: node option; mutable last: node option}

let mkLinkedList() = {first = None; last = None}

let push value linkedList =
    match linkedList.last with
    | None -> 
        let newNode = {previous = None; next = None; value = value}
        linkedList.last <- Some newNode
        linkedList.first <- Some newNode
    | Some last -> 
        let newNode = {previous = linkedList.last; next = None; value = value}
        last.next <- Some newNode
        linkedList.last <- Some newNode

let pop linkedList = 
    let last = linkedList.last.Value.value

    try        
        linkedList.last.Value.previous.Value.next <- None
        linkedList.last <- linkedList.last.Value.previous   
    with
        | _ -> linkedList.last <- None
    
    last
    
let shift linkedList = 
    let first = linkedList.first.Value.value

    try
        linkedList.first.Value.next.Value.previous <- None
        linkedList.first <- linkedList.first.Value.next
    with
        | _ -> linkedList.first <- None
    
    first

let unshift value linkedList = 
    match linkedList.first with
    | None -> 
        let newNode = {previous = None; next = None; value = value}
        linkedList.first <- Some newNode
        linkedList.last <- Some newNode
    | Some first ->
        let newNode = {previous = None; next = linkedList.first; value = value}
        first.previous <- Some newNode
        linkedList.first <- Some newNode
