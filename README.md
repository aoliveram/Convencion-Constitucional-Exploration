## README

<p xmlns:cc="http://creativecommons.org/ns#" xmlns:dct="http://purl.org/dc/terms/"><a property="dct:title" rel="cc:attributionURL" href="https://github.com/aoliveram/Convencion-Constitucional-Exploration">Convencion Constitucional Exploration</a> by <a rel="cc:attributionURL dct:creator" property="cc:attributionName" href="http://aoliveram.com">Aníbal Olivera</a> is licensed under <a href="https://creativecommons.org/licenses/by/4.0/?ref=chooser-v1" target="_blank" rel="license noopener noreferrer" style="display:inline-block;">CC BY 4.0<img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/cc.svg?ref=chooser-v1" alt=""><img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/by.svg?ref=chooser-v1" alt=""></a></p>

Repositorio para la exploración de datos de la Convención Constitucional. Los datos están disponibles como datos abiertos en https://www.cconstituyente.cl/datosabiertos/

La estimación del ordenamiento ideológico proviene de https://github.com/jfabregalacoa/rcp_convencion 

### Dinámica ordenamiento de convencionales

Para comprobar que existe dinámica en el ordenamiento político 1D dentro de la Convención Constitucional, se juntaron los datos de todas las sesiones en ventanas de ~40 días, sin considerar aquellas que no tenían votos, o cuyos votos tenían una índole distinta de la puramente política (ver https://github.com/jfabregalacoa/rcp_convencion ). 
Así, existen conjuntos de datos con las sesiones 01-15 (es decir, de la 1° a la 15° sesión del pleno), 16-21, 22-37, 38-46, 47-55, 56-75, 76-99, y 100-106, 107-109.

Como el criterio fue temporal más que de número de votos, existen conjuntos de datos con más votos que otros, que no representa un problema metodológico excepto cuando la cantidad total de votos es menor a ~90. En tal caso el bootstrapping no es exitoso. 

Para la estimación de ordenamiento se utilizó W-Nominate, dejando como ancla al convencional n°87, y realizando bootstrapping para hacer un contraste de hipótesis individual para la posición de cada convencional. De este modo, si el p-value de un convencional es menor a 0.05, entonces se comprueba que aquel convencional se ha desplazado de su posición original en comparación con la sesión actual. 

La siguiente imagen muestra los p-value para cada convencional según el conjunto de sesiones de referencia y de comparación. Por ejemplo, en 01-15 vs 22-37, se están comparando las votaciones dentro de las primeras 15 sesiones del pleno con las votaciones de las sesiones 22-37 del pleno. Se añaden los nombres de los convencionales que No han cambiado su posición significativamente. De esta forma, podemos ver que entre las primeras sesiones (01-15) y las tardías (76-99), todos los convencionales -salvo uno- se han desplazado de su valor numérico de posición original.

![image alt](https://github.com/aoliveram/Convencion-Constitucional-Exploration/blob/4cab3fa64cab3783bb797d3846b731dc062841a6/scripts%20-%20plots/matriz_comparaciones_sesiones_4.jpeg)
