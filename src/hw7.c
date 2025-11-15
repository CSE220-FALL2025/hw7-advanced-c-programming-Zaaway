#include "hw7.h"

bst_sf* insert_bst_sf(matrix_sf *mat, bst_sf *root) {
    if (root == NULL) {
        bst_sf *new_node = malloc(sizeof(bst_sf));
        if (!new_node) return NULL;
        new_node->mat = mat;
        new_node->left_child = new_node->right_child = NULL;
        return new_node;
    }
    
    if (mat->name < root->mat->name) {
        root->left_child = insert_bst_sf(mat, root->left_child);
    } else {
        root->right_child = insert_bst_sf(mat, root->right_child);
    }
    
    return root;
}

matrix_sf* find_bst_sf(char name, bst_sf *root) {
    if (root == NULL) {
        return NULL;
    }
    
    if (name == root->mat->name) {
        return root->mat;
    }
    
    if (name < root->mat->name) {
        return find_bst_sf(name, root->left_child);
    } else {
        return find_bst_sf(name, root->right_child);
    }
}

void free_bst_sf(bst_sf *root) {
    if (root == NULL) {
        return;
    }
    
    free_bst_sf(root->left_child);
    free_bst_sf(root->right_child);
    free(root->mat);
    free(root);
}

matrix_sf* add_mats_sf(const matrix_sf *mat1, const matrix_sf *mat2) {
    if (mat1->num_rows != mat2->num_rows || mat1->num_cols != mat2->num_cols) {
        return NULL;
    }
    
    matrix_sf *result = malloc(sizeof(matrix_sf) + mat1->num_rows * mat1->num_cols * sizeof(int));
    if (result == NULL) {
        return NULL;
    }
    
    result->name = '?'; 
    result->num_rows = mat1->num_rows;
    result->num_cols = mat1->num_cols;
    
    int total_elements = mat1->num_rows * mat1->num_cols;
    for (int i = 0; i < total_elements; i++) {
        result->values[i] = mat1->values[i] + mat2->values[i];
    }
    
    return result;
}

matrix_sf* mult_mats_sf(const matrix_sf *mat1, const matrix_sf *mat2) {
   if (mat1 == NULL || mat2 == NULL) {
        return NULL;
    }
    
    if (mat1->num_cols != mat2->num_rows) {
        return NULL;
    }
    
    int result_rows = mat1->num_rows;
    int result_cols = mat2->num_cols;
    matrix_sf *m = malloc(sizeof(matrix_sf) + result_rows * result_cols * sizeof(int));
    if (m == NULL) {
        return NULL;
    }

    m->name = '?';
    m->num_rows = result_rows;
    m->num_cols = result_cols;
    
    int mat1_cols = mat1->num_cols;
    int mat2_cols = mat2->num_cols;
    
    for (int i = 0; i < result_rows; i++) {
        int result_row_index = i * result_cols;
        int mat1_row_index = i * mat1_cols;
        
        for (int j = 0; j < result_cols; j++) {
            int sum = 0;
            
            for (int k = 0; k < mat1_cols; k++) {
                int index1 = mat1_row_index + k;
                int index2 = k * mat2_cols + j;
                sum += mat1->values[index1] * mat2->values[index2];
            }
            
            m->values[result_row_index + j] = sum;
        }
    }
        
    return m;
}

matrix_sf* transpose_mat_sf(const matrix_sf *mat) {
    if (mat == NULL) return NULL;
    
    matrix_sf *m = malloc(sizeof(matrix_sf) + mat->num_cols * mat->num_rows * sizeof(int));
    if (m == NULL) return NULL;
    
    m->name = '?';
    m->num_cols = mat->num_rows;
    m->num_rows = mat->num_cols;
    
    for (int i = 0; i < mat->num_rows; i++) {
        for (int j = 0; j < mat->num_cols; j++) {
            int og = i * mat->num_cols + j;
            int tp = j * m->num_cols + i;
            m->values[tp] = mat->values[og];
        }
    }
    
    return m;
}

matrix_sf* create_matrix_sf(char name, const char *expr) {
    if (expr == NULL) {
        return NULL;
    }
    
    char *ptr = (char*)expr;
    int row = (int)strtol(ptr, &ptr, 10);
    int col = (int)strtol(ptr, &ptr, 10);
    
    if (row <= 0 || col <= 0) {
        return NULL;
    }
    
    matrix_sf *m = malloc(sizeof(matrix_sf) + row * col * sizeof(int));
    if (m == NULL) {
        return NULL;
    }
    
    m->name = name;
    m->num_rows = row;
    m->num_cols = col;
    
    while (*ptr != '[' && *ptr != '\0') {
        ptr++;
    }
    
    if (*ptr != '[') {
        free(m);
        return NULL;
    }
    ptr++;
    
    int index = 0;
    for (int i = 0; i < row; i++) {
        for (int j = 0; j < col; j++) {
            while (*ptr == ' ' || *ptr == ';' || *ptr == '\t' || *ptr == '\n') {
                ptr++;
            }
            
            if (*ptr == '\0' || *ptr == ']') {
                free(m);
                return NULL;
            }
            
            char *endptr;
            int value = (int)strtol(ptr, &endptr, 10);
            
            if (endptr == ptr) {
                free(m);
                return NULL;
            }
            
            m->values[index++] = value;
            ptr = endptr;
        }
        
        while (*ptr == ' ' || *ptr == ';' || *ptr == '\t') {
            ptr++;
        }
    }
    
    return m;
}

int prec(char op) {
    if (op == '\'') return 3;
    if (op == '*') return 2;
    if (op == '+') return 1;
    return 0;
}

char* infix2postfix_sf(char *infix) {
    if (!infix) return NULL;
    
    char stack[1000], *output = malloc(1000);
    int top = -1, idx = 0;
    
    for (int i = 0; infix[i]; i++) {
        char c = infix[i];
        if (c == ' ') continue;
        
        if (isalpha(c)) output[idx++] = c;
        else if (c == '(') stack[++top] = c;
        else if (c == ')') {
            while (top >= 0 && stack[top] != '(') 
                output[idx++] = stack[top--];
            top--;
        }
        else {
            while (top >= 0 && stack[top] != '(' && prec(stack[top]) >= prec(c)) 
                output[idx++] = stack[top--];
            stack[++top] = c;
        }
    }
    
    while (top >= 0) output[idx++] = stack[top--];
    output[idx] = '\0';
    return output;
}

matrix_sf* evaluate_expr_sf(char name, char *expr, bst_sf *root) {
    if (root == NULL || expr == NULL) return NULL;

    char *postfix = infix2postfix_sf(expr);
    if (!postfix) return NULL;

    matrix_sf *stack[100];
    int top = -1;
    int i = 0;
    
    while (postfix[i] != '\0') {
        char c = postfix[i];
        if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')) {
            matrix_sf *found = find_bst_sf(c, root);
            if (!found) {
                free(postfix);
                return NULL;
            }
            stack[++top] = found;
        }
        else if (c == '\'') {
            if (top < 0) {
                free(postfix);
                return NULL;
            }
            matrix_sf *t = stack[top--];
            matrix_sf *tTrans = transpose_mat_sf(t);
            if (!tTrans) {
                free(postfix);
                return NULL;
            }
            if (!isalpha(t->name)) free(t);
            stack[++top] = tTrans;
        }
        else if (c == '*') {
            if (top < 1) {
                free(postfix);
                return NULL;
            }
            matrix_sf *t1 = stack[top--];
            matrix_sf *t2 = stack[top--];
            matrix_sf *t3 = mult_mats_sf(t2, t1);
            if (!t3) {
                free(postfix);
                return NULL;
            }
            if (!isalpha(t1->name)) free(t1);
            if (!isalpha(t2->name)) free(t2);
            stack[++top] = t3;
        }
        else if (c == '+') {
            if (top < 1) {
                free(postfix);
                return NULL;
            }
            matrix_sf *t1 = stack[top--];
            matrix_sf *t2 = stack[top--];
            matrix_sf *t3 = add_mats_sf(t2, t1);
            if (!t3) {
                free(postfix);
                return NULL;
            }
            if (!isalpha(t1->name)) free(t1);
            if (!isalpha(t2->name)) free(t2);
            stack[++top] = t3;
        }
        i++;
    }

    if (top != 0) {
        free(postfix);
        return NULL;
    }

    matrix_sf *r = stack[top];
    r->name = name;
    free(postfix);
    return r;
}

matrix_sf *execute_script_sf(char *filename) {
   FILE *file = fopen(filename, "r");
    if (file == NULL) return NULL;

    char *line = NULL;
    size_t len = 0;
    bst_sf *root = NULL;
    matrix_sf *result = NULL;

    while (getline(&line, &len, file) != -1) {   
        char *temp = line;
        while (*temp == ' ') temp++;
        
        char name = *temp;
        temp++;
        while (*temp == ' ' || *temp == '=') temp++;
        
        char *expr = temp;
        
        int has_brackets = 0;
        for (int i = 0; expr[i] != '\0'; i++) {
            if (expr[i] == '[' || expr[i] == ']') {
                has_brackets = 1;
                break;
            }
        }

        if (has_brackets) {
            result = create_matrix_sf(name, expr);
        } else {
            result = evaluate_expr_sf(name, expr, root);
        }
        
        if (result) {
            root = insert_bst_sf(result, root);
        }
    }
    
    fclose(file);
    free(line);
    
    matrix_sf *final_result = result; 
    free_bst_sf(root); 
    
    return final_result;
}

// This is a utility function used during testing. Feel free to adapt the code to implement some of
// the assignment. Feel equally free to ignore it.
matrix_sf *copy_matrix(unsigned int num_rows, unsigned int num_cols, int values[]) {
    matrix_sf *m = malloc(sizeof(matrix_sf)+num_rows*num_cols*sizeof(int));
    m->name = '?';
    m->num_rows = num_rows;
    m->num_cols = num_cols;
    memcpy(m->values, values, num_rows*num_cols*sizeof(int));
    return m;
}

// Don't touch this function. It's used by the testing framework.
// It's been left here in case it helps you debug and test your code.
void print_matrix_sf(matrix_sf *mat) {
    assert(mat != NULL);
    assert(mat->num_rows <= 1000);
    assert(mat->num_cols <= 1000);
    printf("%d %d ", mat->num_rows, mat->num_cols);
    for (unsigned int i = 0; i < mat->num_rows*mat->num_cols; i++) {
        printf("%d", mat->values[i]);
        if (i < mat->num_rows*mat->num_cols-1)
            printf(" ");
    }
    printf("\n");
}
