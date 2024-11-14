#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

// Core heap operations on raw double arrays
static void upheap_min(double *x, R_xlen_t pos) {
    while (pos > 1) {
        R_xlen_t parent = pos / 2;
        if (x[pos-1] < x[parent-1]) {
            double tmp = x[parent-1];
            x[parent-1] = x[pos-1];
            x[pos-1] = tmp;
            pos = parent;
        } else {
            break;
        }
    }
}

static void upheap_max(double *x, R_xlen_t pos) {
    while (pos > 1) {
        R_xlen_t parent = pos / 2;
        if (x[pos-1] > x[parent-1]) {
            double tmp = x[parent-1];
            x[parent-1] = x[pos-1];
            x[pos-1] = tmp;
            pos = parent;
        } else {
            break;
        }
    }
}

static void dnheap_min(double *x, R_xlen_t pos, R_xlen_t size) {
    while (1) {
        R_xlen_t smallest = pos;
        R_xlen_t left = 2 * pos;
        R_xlen_t right = 2 * pos + 1;
        
        if (left <= size && x[left-1] < x[smallest-1]) {
            smallest = left;
        }
        if (right <= size && x[right-1] < x[smallest-1]) {
            smallest = right;
        }
        
        if (smallest != pos) {
            double tmp = x[pos-1];
            x[pos-1] = x[smallest-1];
            x[smallest-1] = tmp;
            pos = smallest;
        } else {
            break;
        }
    }
}

static void dnheap_max(double *x, R_xlen_t pos, R_xlen_t size) {
    while (1) {
        R_xlen_t largest = pos;
        R_xlen_t left = 2 * pos;
        R_xlen_t right = 2 * pos + 1;
        
        if (left <= size && x[left-1] > x[largest-1]) {
            largest = left;
        }
        if (right <= size && x[right-1] > x[largest-1]) {
            largest = right;
        }
        
        if (largest != pos) {
            double tmp = x[pos-1];
            x[pos-1] = x[largest-1];
            x[largest-1] = tmp;
            pos = largest;
        } else {
            break;
        }
    }
}

// R interface functions for heap operations
SEXP C_minheap(SEXP x) {
    R_xlen_t n = XLENGTH(x);
    if (n == 0) return x;
    
    SEXP result;
    PROTECT(result = duplicate(x));
    double *heap = REAL(result);
    
    // Floyd's algorithm
    for (R_xlen_t i = n/2; i >= 1; i--) {
        dnheap_min(heap, i, n);
    }
    
    setAttrib(result, R_ClassSymbol, mkString("minheap"));
    UNPROTECT(1);
    return result;
}

SEXP C_maxheap(SEXP x) {
    R_xlen_t n = XLENGTH(x);
    if (n == 0) return x;
    
    SEXP result;
    PROTECT(result = duplicate(x));
    double *heap = REAL(result);
    
    // Floyd's algorithm
    for (R_xlen_t i = n/2; i >= 1; i--) {
        dnheap_max(heap, i, n);
    }
    
    setAttrib(result, R_ClassSymbol, mkString("maxheap"));
    UNPROTECT(1);
    return result;
}

SEXP C_push_minheap(SEXP heap, SEXP values) {
    R_xlen_t n = XLENGTH(heap);
    R_xlen_t m = XLENGTH(values);
    
    SEXP result;
    PROTECT(result = allocVector(REALSXP, n + m));
    double *x = REAL(result);
    
    // Copy existing heap
    memcpy(x, REAL(heap), n * sizeof(double));
    
    // Add new values one by one
    double *vals = REAL(values);
    for (R_xlen_t i = 0; i < m; i++) {
        x[n + i] = vals[i];
        upheap_min(x, n + i + 1);
    }
    
    setAttrib(result, R_ClassSymbol, mkString("minheap"));
    UNPROTECT(1);
    return result;
}

SEXP C_push_maxheap(SEXP heap, SEXP values) {
    R_xlen_t n = XLENGTH(heap);
    R_xlen_t m = XLENGTH(values);
    
    SEXP result;
    PROTECT(result = allocVector(REALSXP, n + m));
    double *x = REAL(result);
    
    // Copy existing heap
    memcpy(x, REAL(heap), n * sizeof(double));
    
    // Add new values one by one
    double *vals = REAL(values);
    for (R_xlen_t i = 0; i < m; i++) {
        x[n + i] = vals[i];
        upheap_max(x, n + i + 1);
    }
    
    setAttrib(result, R_ClassSymbol, mkString("maxheap"));
    UNPROTECT(1);
    return result;
}

SEXP C_pop_minheap(SEXP heap, SEXP count) {
    R_xlen_t n = XLENGTH(heap);
    R_xlen_t k = asInteger(count);
    if (n == 0 || k <= 0) return R_NilValue;
    if (k > n) k = n;
    
    SEXP result, values, updated_heap, names;
    PROTECT(result = allocVector(VECSXP, 2));
    PROTECT(values = allocVector(REALSXP, k));
    PROTECT(updated_heap = allocVector(REALSXP, n - k));
    PROTECT(names = allocVector(STRSXP, 2));
    
    // Set names for the list elements
    SET_STRING_ELT(names, 0, mkChar("values"));
    SET_STRING_ELT(names, 1, mkChar("heap"));
    setAttrib(result, R_NamesSymbol, names);
    
    double *heap_data = REAL(heap);
    double *val_data = REAL(values);
    double *new_heap = REAL(updated_heap);
    
    // Create working copy of heap
    double *work_heap = (double *)R_alloc(n, sizeof(double));
    memcpy(work_heap, heap_data, n * sizeof(double));
    
    // Extract k minimum values
    for (R_xlen_t i = 0; i < k; i++) {
        val_data[i] = work_heap[0];  // Store minimum
        
        // Replace root with last element and heapify
        work_heap[0] = work_heap[n - i - 1];
        dnheap_min(work_heap, 1, n - i - 1);
    }
    
    // Copy remaining elements to new heap
    memcpy(new_heap, work_heap, (n - k) * sizeof(double));
    
    setAttrib(updated_heap, R_ClassSymbol, mkString("minheap"));
    SET_VECTOR_ELT(result, 0, values);
    SET_VECTOR_ELT(result, 1, updated_heap);
    
    UNPROTECT(4);
    return result;
}

SEXP C_pop_maxheap(SEXP heap, SEXP count) {
    R_xlen_t n = XLENGTH(heap);
    R_xlen_t k = asInteger(count);
    if (n == 0 || k <= 0) return R_NilValue;
    if (k > n) k = n;
    
    SEXP result, values, updated_heap, names;
    PROTECT(result = allocVector(VECSXP, 2));
    PROTECT(values = allocVector(REALSXP, k));
    PROTECT(updated_heap = allocVector(REALSXP, n - k));
    PROTECT(names = allocVector(STRSXP, 2));
    
    // Set names for the list elements
    SET_STRING_ELT(names, 0, mkChar("values"));
    SET_STRING_ELT(names, 1, mkChar("heap"));
    setAttrib(result, R_NamesSymbol, names);
    
    double *heap_data = REAL(heap);
    double *val_data = REAL(values);
    double *new_heap = REAL(updated_heap);
    
    // Create working copy of heap
    double *work_heap = (double *)R_alloc(n, sizeof(double));
    memcpy(work_heap, heap_data, n * sizeof(double));
    
    // Extract k maximum values
    for (R_xlen_t i = 0; i < k; i++) {
        val_data[i] = work_heap[0];  // Store maximum
        
        // Replace root with last element and heapify
        work_heap[0] = work_heap[n - i - 1];
        dnheap_max(work_heap, 1, n - i - 1);
    }
    
    // Copy remaining elements to new heap
    memcpy(new_heap, work_heap, (n - k) * sizeof(double));
    
    setAttrib(updated_heap, R_ClassSymbol, mkString("maxheap"));
    SET_VECTOR_ELT(result, 0, values);
    SET_VECTOR_ELT(result, 1, updated_heap);
    
    UNPROTECT(4);
    return result;
}

SEXP C_insert_minheap(SEXP heap, SEXP value) {
    double val = asReal(value);
    R_xlen_t n = XLENGTH(heap);
    if (n == 0) return heap;
    
    SEXP result;
    PROTECT(result = duplicate(heap));
    double *x = REAL(result);
    
    // Find first leaf
    R_xlen_t first_leaf = (n / 2) + 1;
    
    // Find maximum leaf
    double max_leaf = x[first_leaf-1];
    R_xlen_t max_pos = first_leaf;
    for (R_xlen_t i = first_leaf + 1; i <= n; i++) {
        if (x[i-1] > max_leaf) {
            max_leaf = x[i-1];
            max_pos = i;
        }
    }
    
    // Replace if new value is smaller
    if (val < max_leaf) {
        x[max_pos-1] = val;
        upheap_min(x, max_pos);
    }
    
    UNPROTECT(1);
    return result;
}

SEXP C_insert_maxheap(SEXP heap, SEXP value) {
    double val = asReal(value);
    R_xlen_t n = XLENGTH(heap);
    if (n == 0) return heap;
    
    SEXP result;
    PROTECT(result = duplicate(heap));
    double *x = REAL(result);
    
    // Find first leaf
    R_xlen_t first_leaf = (n / 2) + 1;
    
    // Find minimum leaf
    double min_leaf = x[first_leaf-1];
    R_xlen_t min_pos = first_leaf;
    for (R_xlen_t i = first_leaf + 1; i <= n; i++) {
        if (x[i-1] < min_leaf) {
            min_leaf = x[i-1];
            min_pos = i;
        }
    }
    
    // Replace if new value is larger
    if (val > min_leaf) {
        x[min_pos-1] = val;
        upheap_max(x, min_pos);
    }
    
    UNPROTECT(1);
    return result;
}

SEXP C_pushpop_minheap(SEXP heap, SEXP value) {
    double val = asReal(value);
    R_xlen_t n = XLENGTH(heap);
    if (n == 0) {
        SEXP result = allocVector(VECSXP, 2);
        PROTECT(result);
        SET_VECTOR_ELT(result, 0, ScalarReal(val));
        SET_VECTOR_ELT(result, 1, heap);
        UNPROTECT(1);
        return result;
    }
    
    // If new value would be the new minimum, just return it
    if (val <= REAL(heap)[0]) {
        SEXP result = allocVector(VECSXP, 2);
        PROTECT(result);
        SET_VECTOR_ELT(result, 0, ScalarReal(val));
        SET_VECTOR_ELT(result, 1, heap);
        UNPROTECT(1);
        return result;
    }
    
    // Otherwise replace root with new value and heapify down
    SEXP result = allocVector(VECSXP, 2);
    PROTECT(result);
    SET_VECTOR_ELT(result, 0, ScalarReal(REAL(heap)[0]));
    
    SEXP new_heap = duplicate(heap);
    PROTECT(new_heap);
    double *x = REAL(new_heap);
    x[0] = val;
    dnheap_min(x, 1, n);
    
    setAttrib(new_heap, R_ClassSymbol, mkString("minheap"));
    SET_VECTOR_ELT(result, 1, new_heap);
    
    UNPROTECT(2);
    return result;
}

SEXP C_pushpop_maxheap(SEXP heap, SEXP value) {
    double val = asReal(value);
    R_xlen_t n = XLENGTH(heap);
    if (n == 0) {
        SEXP result = allocVector(VECSXP, 2);
        PROTECT(result);
        SET_VECTOR_ELT(result, 0, ScalarReal(val));
        SET_VECTOR_ELT(result, 1, heap);
        UNPROTECT(1);
        return result;
    }
    
    // If new value would be the new maximum, just return it
    if (val >= REAL(heap)[0]) {
        SEXP result = allocVector(VECSXP, 2);
        PROTECT(result);
        SET_VECTOR_ELT(result, 0, ScalarReal(val));
        SET_VECTOR_ELT(result, 1, heap);
        UNPROTECT(1);
        return result;
    }
    
    // Otherwise replace root with new value and heapify down
    SEXP result = allocVector(VECSXP, 2);
    PROTECT(result);
    SET_VECTOR_ELT(result, 0, ScalarReal(REAL(heap)[0]));
    
    SEXP new_heap = duplicate(heap);
    PROTECT(new_heap);
    double *x = REAL(new_heap);
    x[0] = val;
    dnheap_max(x, 1, n);
    
    setAttrib(new_heap, R_ClassSymbol, mkString("maxheap"));
    SET_VECTOR_ELT(result, 1, new_heap);
    
    UNPROTECT(2);
    return result;
}

SEXP C_poppush_minheap(SEXP heap, SEXP value) {
    double val = asReal(value);
    R_xlen_t n = XLENGTH(heap);
    if (n == 0) {
        SEXP result = allocVector(VECSXP, 2);
        PROTECT(result);
        SET_VECTOR_ELT(result, 0, ScalarReal(val));
        SET_VECTOR_ELT(result, 1, heap);
        UNPROTECT(1);
        return result;
    }
    
    SEXP result = allocVector(VECSXP, 2);
    PROTECT(result);
    SET_VECTOR_ELT(result, 0, ScalarReal(REAL(heap)[0]));
    
    SEXP new_heap = duplicate(heap);
    PROTECT(new_heap);
    double *x = REAL(new_heap);
    x[0] = val;
    dnheap_min(x, 1, n);
    
    setAttrib(new_heap, R_ClassSymbol, mkString("minheap"));
    SET_VECTOR_ELT(result, 1, new_heap);
    
    UNPROTECT(2);
    return result;
}

SEXP C_poppush_maxheap(SEXP heap, SEXP value) {
    double val = asReal(value);
    R_xlen_t n = XLENGTH(heap);
    if (n == 0) {
        SEXP result = allocVector(VECSXP, 2);
        PROTECT(result);
        SET_VECTOR_ELT(result, 0, ScalarReal(val));
        SET_VECTOR_ELT(result, 1, heap);
        UNPROTECT(1);
        return result;
    }
    
    SEXP result = allocVector(VECSXP, 2);
    PROTECT(result);
    SET_VECTOR_ELT(result, 0, ScalarReal(REAL(heap)[0]));
    
    SEXP new_heap = duplicate(heap);
    PROTECT(new_heap);
    double *x = REAL(new_heap);
    x[0] = val;
    dnheap_max(x, 1, n);
    
    setAttrib(new_heap, R_ClassSymbol, mkString("maxheap"));
    SET_VECTOR_ELT(result, 1, new_heap);
    
    UNPROTECT(2);
    return result;
}

SEXP C_npushpop_minheap(SEXP heap, SEXP values) {
    R_xlen_t n = XLENGTH(heap);
    if (n == 0) return heap;
    
    SEXP result = duplicate(heap);
    PROTECT(result);
    double *x = REAL(result);
    double *vals = REAL(values);
    R_xlen_t nvals = XLENGTH(values);
    
    for (R_xlen_t i = 0; i < nvals; i++) {
        if (vals[i] > x[0]) {
            x[0] = vals[i];
            dnheap_min(x, 1, n);
        }
    }
    
    setAttrib(result, R_ClassSymbol, mkString("minheap"));
    UNPROTECT(1);
    return result;
}

SEXP C_npushpop_maxheap(SEXP heap, SEXP values) {
    R_xlen_t n = XLENGTH(heap);
    if (n == 0) return heap;
    
    SEXP result = duplicate(heap);
    PROTECT(result);
    double *x = REAL(result);
    double *vals = REAL(values);
    R_xlen_t nvals = XLENGTH(values);
    
    for (R_xlen_t i = 0; i < nvals; i++) {
        if (vals[i] < x[0]) {
            x[0] = vals[i];
            dnheap_max(x, 1, n);
        }
    }
    
    setAttrib(result, R_ClassSymbol, mkString("maxheap"));
    UNPROTECT(1);
    return result;
}

SEXP C_npoppush_minheap(SEXP heap, SEXP values) {
    R_xlen_t n = XLENGTH(heap);
    if (n == 0) return heap;
    
    SEXP result = duplicate(heap);
    PROTECT(result);
    double *x = REAL(result);
    double *vals = REAL(values);
    R_xlen_t nvals = XLENGTH(values);
    
    for (R_xlen_t i = 0; i < nvals; i++) {
        x[0] = vals[i];
        dnheap_min(x, 1, n);
    }
    
    setAttrib(result, R_ClassSymbol, mkString("minheap"));
    UNPROTECT(1);
    return result;
}

SEXP C_npoppush_maxheap(SEXP heap, SEXP values) {
    R_xlen_t n = XLENGTH(heap);
    if (n == 0) return heap;
    
    SEXP result = duplicate(heap);
    PROTECT(result);
    double *x = REAL(result);
    double *vals = REAL(values);
    R_xlen_t nvals = XLENGTH(values);
    
    for (R_xlen_t i = 0; i < nvals; i++) {
        x[0] = vals[i];
        dnheap_max(x, 1, n);
    }
    
    setAttrib(result, R_ClassSymbol, mkString("maxheap"));
    UNPROTECT(1);
    return result;
}

SEXP C_upheap_minheap(SEXP heap, SEXP pos) {
    R_xlen_t position = asInteger(pos);
    
    SEXP result;
    PROTECT(result = duplicate(heap));
    double *x = REAL(result);
    
    upheap_min(x, position);
    
    setAttrib(result, R_ClassSymbol, mkString("minheap"));
    UNPROTECT(1);
    return result;
}

SEXP C_upheap_maxheap(SEXP heap, SEXP pos) {
    R_xlen_t position = asInteger(pos);
    
    SEXP result;
    PROTECT(result = duplicate(heap));
    double *x = REAL(result);
    
    upheap_max(x, position);
    
    setAttrib(result, R_ClassSymbol, mkString("maxheap"));
    UNPROTECT(1);
    return result;
}

SEXP C_dnheap_minheap(SEXP heap, SEXP pos, SEXP size) {
    R_xlen_t position = asInteger(pos);
    R_xlen_t heap_size = asInteger(size);
    
    SEXP result;
    PROTECT(result = duplicate(heap));
    double *x = REAL(result);
    
    dnheap_min(x, position, heap_size);
    
    setAttrib(result, R_ClassSymbol, mkString("minheap"));
    UNPROTECT(1);
    return result;
}

SEXP C_dnheap_maxheap(SEXP heap, SEXP pos, SEXP size) {
    R_xlen_t position = asInteger(pos);
    R_xlen_t heap_size = asInteger(size);
    
    SEXP result;
    PROTECT(result = duplicate(heap));
    double *x = REAL(result);
    
    dnheap_max(x, position, heap_size);
    
    setAttrib(result, R_ClassSymbol, mkString("maxheap"));
    UNPROTECT(1);
    return result;
}

static const R_CallMethodDef CallEntries[] = {
    {"C_minheap", (DL_FUNC) &C_minheap, 1},
    {"C_maxheap", (DL_FUNC) &C_maxheap, 1},
    {"C_push_minheap", (DL_FUNC) &C_push_minheap, 2},
    {"C_push_maxheap", (DL_FUNC) &C_push_maxheap, 2},
    {"C_pop_minheap", (DL_FUNC) &C_pop_minheap, 2},
    {"C_pop_maxheap", (DL_FUNC) &C_pop_maxheap, 2},
    {"C_insert_minheap", (DL_FUNC) &C_insert_minheap, 2},
    {"C_insert_maxheap", (DL_FUNC) &C_insert_maxheap, 2},
    {"C_pushpop_minheap", (DL_FUNC) &C_pushpop_minheap, 2},
    {"C_pushpop_maxheap", (DL_FUNC) &C_pushpop_maxheap, 2},
    {"C_poppush_minheap", (DL_FUNC) &C_poppush_minheap, 2},
    {"C_poppush_maxheap", (DL_FUNC) &C_poppush_maxheap, 2},
    {"C_npushpop_minheap", (DL_FUNC) &C_npushpop_minheap, 2},
    {"C_npushpop_maxheap", (DL_FUNC) &C_npushpop_maxheap, 2},
    {"C_npoppush_minheap", (DL_FUNC) &C_npoppush_minheap, 2},
    {"C_npoppush_maxheap", (DL_FUNC) &C_npoppush_maxheap, 2},
    {"C_upheap_minheap", (DL_FUNC) &C_upheap_minheap, 2},
    {"C_upheap_maxheap", (DL_FUNC) &C_upheap_maxheap, 2},
    {"C_dnheap_minheap", (DL_FUNC) &C_dnheap_minheap, 3},
    {"C_dnheap_maxheap", (DL_FUNC) &C_dnheap_maxheap, 3},
    {NULL, NULL, 0}
};

void R_init_Rheap(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
} 