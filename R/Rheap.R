##' Create a min-heap from a vector using Floyd's algorithm
##'
##' @title Create a min-heap
##' @param x Numeric vector to convert to heap
##' @return A minheap object
##' @export
minheap <- function(x = numeric(0)) {
    .Call(C_minheap, as.numeric(x))
}


##' Extract the top (min or max) element from a heap
##'
##' @title Extract top element from heap
##' @param heap Heap object
##' @return Head element of heap
##' @export
head <- function(heap) {
    heap[1]
}

##' Create a max-heap from a vector using Floyd's algorithm
##'
##' @title Create a max-heap
##' @param x Numeric vector to convert to heap
##' @return A maxheap object
##' @export
maxheap <- function(x = numeric(0)) {
    .Call(C_maxheap, as.numeric(x))
}

##' Push an element onto a heap
##'
##' @title Push element onto heap
##' @param heap Heap object
##' @param value Numeric value to push
##' @return Updated heap object
##' @export
push <- function(heap, value) {
    UseMethod("push")
}

##' @rdname push
##' @method push minheap
##' @export
push.minheap <- function(heap, value) {
    .Call(C_push_minheap, heap, as.numeric(value))
}

##' @rdname push
##' @method push maxheap
##' @export
push.maxheap <- function(heap, value) {
    .Call(C_push_maxheap, heap, as.numeric(value))
}

##' Pop top element from a heap
##'
##' @title Pop top element from a heap
##' @param heap Heap object
##' @return List containing value and updated heap
##' @export
pop <- function(heap) {
    UseMethod("pop")
}

##' @rdname pop
##' @method pop minheap
##' @export
pop.minheap <- function(heap) {
    .Call(C_pop_minheap, heap)
}

##' @rdname pop
##' @method pop maxheap
##' @export
pop.maxheap <- function(heap) {
    .Call(C_pop_maxheap, heap)
}

##' Insert element into fixed-size heap
##'
##' For a min-heap, finds the maximum leaf element and replaces it with the new value
##' if the new value is smaller. For a max-heap, finds the minimum leaf element and
##' replaces it with the new value if the new value is larger.
##'
##' @title Insert into fixed-size heap
##' @param heap Heap object
##' @param value Numeric value to insert
##' @return Updated heap object
##' @export
insert <- function(heap, value) {
    UseMethod("insert")
}

##' @rdname insert
##' @method insert minheap
##' @export
insert.minheap <- function(heap, value) {
    .Call(C_insert_minheap, heap, as.numeric(value))
}

##' @rdname insert
##' @method insert maxheap
##' @export
insert.maxheap <- function(heap, value) {
    .Call(C_insert_maxheap, heap, as.numeric(value))
}

##' Push a new element into a heap then pop the top element
##'
##' This function is more efficient than separate calls to push and pop.
##'
##' @title Push and pop top element
##' @param heap Heap object
##' @param value Numeric value to push
##' @return List containing the top element and updated heap
##' @export
pushpop <- function(heap, value) {
    UseMethod("pushpop")
}

##' @rdname pushpop
##' @method pushpop minheap
##' @export
pushpop.minheap <- function(heap, value) {
    .Call(C_pushpop_minheap, heap, as.numeric(value))
}

##' @rdname pushpop
##' @method pushpop maxheap
##' @export
pushpop.maxheap <- function(heap, value) {
    .Call(C_pushpop_maxheap, heap, as.numeric(value))
}

##' Pop the top element then push a new element into a heap
##'
##' This function is more efficient than separate calls to pop and push.
##'
##' @title Pop top element then push new element
##' @param heap Heap object
##' @param value Numeric value to push
##' @return List containing the top element and updated heap
##' @export
poppush <- function(heap, value) {
    UseMethod("poppush")
}

##' @rdname poppush
##' @method poppush minheap
##' @export
poppush.minheap <- function(heap, value) {
    .Call(C_poppush_minheap, heap, as.numeric(value))
}

##' @rdname poppush
##' @method poppush maxheap
##' @export
poppush.maxheap <- function(heap, value) {
    .Call(C_poppush_maxheap, heap, as.numeric(value))
}

##' Repeatedly push then pop a heap
##'
##' For each value in values, the value is pushed to the heap and then the top 
##' (minimum or maximum) of the resulting heap is popped.  The popped values are 
##' discarded and the resulting heap is returned.
##'
##' @title Repeatedly Push and Pop a Heap
##' @param heap Heap object
##' @param values Numeric vector of values to push
##' @return Updated heap object
##' @export
npushpop <- function(heap, values) {
    UseMethod("npushpop")
}

##' @rdname npushpop
##' @method npushpop minheap
##' @export
npushpop.minheap <- function(heap, values) {
    .Call(C_npushpop_minheap, heap, as.numeric(values))
}

##' @rdname npushpop
##' @method npushpop maxheap
##' @export
npushpop.maxheap <- function(heap, values) {
    .Call(C_npushpop_maxheap, heap, as.numeric(values))
}

##' Repeatedly pop then push into a heap.
##'
##' For each value in values, the top element (minimum or maximum) of the heap 
##' is popped and the new value is pushed to the heap. The popped values are 
##' discarded and the resulting heap is returned.
##'
##' @title Repeatedly poppush elements into a heap
##' @param heap Heap object
##' @param values Numeric vector of values to push
##' @return Updated heap object
##' @export
npoppush <- function(heap, values) {
    UseMethod("npoppush")
}

##' @rdname npoppush
##' @method npoppush minheap
##' @export
npoppush.minheap <- function(heap, values) {
    .Call(C_npoppush_minheap, heap, as.numeric(values))
}

##' @rdname npoppush
##' @method npoppush maxheap
##' @export
npoppush.maxheap <- function(heap, values) {
    .Call(C_npoppush_maxheap, heap, as.numeric(values))
}


##' Print a min-heap
##'
##' @title Print a min-heap
##' @param x Min-heap object
##' @param ... currently ignored
##' @export
print.minheap <- function(x, ...) {
  cat(sprintf("Min heap with %d elements\n", length(x)))
}

##' Print a max-heap
##'
##' @title Print a max-heap
##' @param x Max-heap object
##' @param ... currently ignored
##' @export
print.maxheap <- function(x, ...) {
  cat(sprintf("Max heap with %d elements\n", length(x)))
}

##' Maintain heap property upwards from position
##'
##' @title Heapify up from position
##' @param heap Heap object
##' @param pos Position to heapify up from
##' @return Updated heap object
##' @export
upheap <- function(heap, pos) {
    UseMethod("upheap")
}

##' @rdname upheap
##' @method upheap minheap
##' @export
upheap.minheap <- function(heap, pos) {
    .Call(C_upheap_minheap, heap, as.integer(pos))
}

##' @rdname upheap
##' @method upheap maxheap
##' @export
upheap.maxheap <- function(heap, pos) {
    .Call(C_upheap_maxheap, heap, as.integer(pos))
}

##' Maintain heap property downwards from position
##'
##' @title Heapify down from position
##' @param heap Heap object
##' @param pos Position to heapify down from
##' @param size Size of heap to consider
##' @return Updated heap object
##' @export
dnheap <- function(heap, pos, size) {
    UseMethod("dnheap")
}

##' @rdname dnheap
##' @method dnheap minheap
##' @export
dnheap.minheap <- function(heap, pos, size) {
    .Call(C_dnheap_minheap, heap, as.integer(pos), as.integer(size))
}

##' @rdname dnheap
##' @method dnheap maxheap
##' @export
dnheap.maxheap <- function(heap, pos, size) {
    .Call(C_dnheap_maxheap, heap, as.integer(pos), as.integer(size))
}
