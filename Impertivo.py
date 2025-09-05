
estudiantes = [("Ana", 85), ("Luis", 85), ("Carlos", 85), ("Marta", 95)]


for i in range(len(estudiantes)):
    for j in range(i + 1, len(estudiantes)):

        if (estudiantes[j][1] > estudiantes[i][1]) or \
           (estudiantes[j][1] == estudiantes[i][1] and estudiantes[j][0] < estudiantes[i][0]):

            estudiantes[i], estudiantes[j] = estudiantes[j], estudiantes[i]

print("Ordenamiento (imperativo):")
for nombre, nota in estudiantes:
    print(nombre, nota)
