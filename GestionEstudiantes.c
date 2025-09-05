#include <stdio.h>
#include <stdlib.h>
#include <string.h>


typedef struct {
    char *nombre;
    char *apellido;
    int edad;
    unsigned int id;
    int num_materias;
    float *calificaciones; 
} Estudiante;


Estudiante* crearEstudiante(const char *nombre, const char *apellido, int edad, unsigned int id, int num_materias) {
    Estudiante *e = (Estudiante*) malloc(sizeof(Estudiante));
    if (!e) {
        printf("Error: no se pudo asignar memoria\n");
        exit(1);
    }


    e->nombre = (char*) malloc(strlen(nombre) + 1);
    e->apellido = (char*) malloc(strlen(apellido) + 1);
    strcpy(e->nombre, nombre);
    strcpy(e->apellido, apellido);

    e->edad = edad;
    e->id = id;
    e->num_materias = num_materias;


    e->calificaciones = (float*) malloc(num_materias * sizeof(float));
    for (int i = 0; i < num_materias; i++) {
        e->calificaciones[i] = 0.0;
    }

    return e;
}


void liberarEstudiante(Estudiante *e) {
    if (e) {
        free(e->nombre);
        free(e->apellido);
        free(e->calificaciones);
        free(e);
    }
}


void compactar(Estudiante **lista, int *num_estudiantes) {
    int nuevo_tam = 0;
    for (int i = 0; i < *num_estudiantes; i++) {
        if (lista[i] != NULL) {
            lista[nuevo_tam++] = lista[i];
        }
    }
    *num_estudiantes = nuevo_tam;
    lista = (Estudiante**) realloc(lista, nuevo_tam * sizeof(Estudiante*));
}

int main() {
    int num_estudiantes = 0;
    Estudiante **lista = NULL;


    lista = (Estudiante**) realloc(lista, (num_estudiantes + 1) * sizeof(Estudiante*));
    lista[num_estudiantes++] = crearEstudiante("Ana", "Lopez", 20, 1001, 3);

    lista = (Estudiante**) realloc(lista, (num_estudiantes + 1) * sizeof(Estudiante*));
    lista[num_estudiantes++] = crearEstudiante("Luis", "Martinez", 22, 1002, 4);

    liberarEstudiante(lista[1]);
    lista[1] = NULL;

    compactar(lista, &num_estudiantes);


    for (int i = 0; i < num_estudiantes; i++) {
        printf("ID: %d | Nombre: %s %s | Edad: %d | Materias: %d\n",
               lista[i]->id, lista[i]->nombre, lista[i]->apellido, lista[i]->edad, lista[i]->num_materias);
    }


    for (int i = 0; i < num_estudiantes; i++) {
        liberarEstudiante(lista[i]);
    }
    free(lista);

    return 0;
}
