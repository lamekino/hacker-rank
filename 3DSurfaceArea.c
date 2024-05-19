#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static int
read_dimensions(FILE *stream, int *rows, int *cols) {
    size_t sep;
    char readbuf[512] = {0};
    char *split = &readbuf[0];

    if (!fgets(readbuf, sizeof(readbuf), stream)) {
        return -1;
    }

    sep = strcspn(readbuf, " ");
    if (readbuf[sep] == '\0') {
        return -1;
    }

    readbuf[sep] = '\0';
    *rows = atoi(split);

    split += sep + 1;
    *cols = atoi(split);

    return 0;
}

typedef int matrix_elem_t;
typedef matrix_elem_t** Matrix;

static Matrix
matrix_create(size_t n, size_t m) {
    size_t i;

    matrix_elem_t **ptrs = malloc(sizeof(*ptrs) * n);
    matrix_elem_t *arena = calloc(sizeof(*arena), n * m);

    if (!ptrs || !arena) {
        free(ptrs);
        free(arena);
        return NULL;
    }

    for (i = 0; i < n; i++) {
        ptrs[i] = &arena[i * m];
    }

    return ptrs;
}

static void
matrix_free(const Matrix matrix) {
    /* the first row of the matrix points to the entire arena */
    free(matrix[0]);
    free(matrix);
}

static Matrix
matrix_transpose(const Matrix src, size_t n, size_t m) {
    int i, j;

    Matrix trans = matrix_create(m, n);
    if (!trans) {
        return NULL;
    }

    for (i = 0; i < n; i++) {
        for (j = 0; j < m; j++) {
            trans[j][i] = src[i][j];
        }
    }

    return trans;
}

static void
matrix_print(const Matrix matrix, size_t n, size_t m) {
    int i, j;

    for (i = 0; i < n; i++) {
        for (j = 0; j < m; j++) {
            if (j > 0) {
                printf(" ");
            }
            printf("%d", matrix[i][j]);
        }
        puts("");
    }
}

static int
read_heights(FILE *stream, size_t rows, size_t cols, Matrix *dest) {
    size_t r, c;

    Matrix matrix = matrix_create(rows, cols);
    if (!matrix) {
        return -1;
    }

    for (r = 0; r < rows; r++) {
        char readbuf[512] = {0};
        char *split = &readbuf[0];

        if (!fgets(readbuf, sizeof(readbuf), stream)) {
            matrix_free(matrix);
            return -1;
        }

        for (c = 0; c < cols; c++) {
            const int has_next = cols - c > 1;
            size_t sep = strcspn(split, " ");

            if (has_next && split[sep] == '\0') {
                matrix_free(matrix);
                return -1;
            }

            split[sep] = '\0';
            matrix[r][c] = atoi(split);

            split += sep + 1;
        }
    }

    *dest = matrix;
    return 0;
}

static int
solve(const Matrix heights, int rows, int cols) {
    int r, c;
    int surface_area = 2 * rows * cols;

    /* start:     finish:
     *      %-----------%
     *      %-----------%
     *      %-----------% */
    for (r = 0; r < rows; r++) {
        surface_area += heights[r][0] + heights[r][cols - 1];

        for (c = 0; c < cols - 1; c++) {
            int cur = heights[r][c];
            int next = heights[r][c + 1];

            surface_area += abs(cur - next);
        }
    }

    /* start:  %%%%%
     *         | | |
     *         | | |
     *         | | |
     * finish: %%%%% */
    for (c = 0; c < cols; c++) {
        surface_area += heights[0][c] + heights[rows - 1][c];

        for (r = 0; r < rows - 1; r++) {
            int cur = heights[r][c];
            int next = heights[r + 1][c];

            surface_area += abs(cur - next);
        }
    }

    return surface_area;
}

int
main() {
    int rows, cols;
    Matrix height_matrix = NULL;

    if (read_dimensions(stdin, &rows, &cols) < 0) {
        return -1;
    }

    if (read_heights(stdin, rows, cols, &height_matrix) < 0) {
        return -1;
    }

    printf("%d\n", solve(height_matrix, rows, cols));

    matrix_free(height_matrix);
    return 0;
}
