# []()

# TP Final (consignas y datos)
- [Consignas](/tp_final/consignas.pdf)
- [Dataset](/tp_final/data/deptos_2014.csv)
- [Documento dataset](/tp_final/data/descripcion_data.pdf)

# Presentación
El objetivo de este curso es brindar una primera aproximación a algunos conceptos fundamentales de Machine Learning: hiperparámetros, sesgo-varianza, flujo de trabajo, error de generalización, serán algunos de los conceptos a trabajar.


# Contenidos y materiales

- __Unidad 1.__ Modelos de aprendizaje automático: fundamentos conceptuales, diferencias con el enfoque estadístico tradicional.

- __Unidad 2.__ Problemas de clasificación y regresión.
Aprendizaje supervisado y no supervisado. Modelos simples y ensambles de modelos. CART, Bagging, Random Forest y Boosting.

- __Unidad 3.__ Interpretable Machine Learning. Haciendo interpretables modelos de "caja negra".


# Clase 1. 
- [Explicación y práctica - Notebook](/clase_1/notebook/intro_caret_notebook.nb.html)
- [Explicación y práctica - RCode](/clase_1/scripts/intro_caret_script.R)
- [Slides Intro ML - pdf](/clase_1/slides/Clase1a.pdf)
- [Slides Over-Under fitting - pdf](/clase_1/slides/Clase1b.pdf)

Pueden descargarse la totalidad de los materiales del repositorio para trabajar en un único archivo .zip

- [![](img/Download.png)](clase_1.zip)


# Clase 2. 
- [Repaso - cross validation y train test desde cero - Notebook](/clase_1/notebook/intro_train_test_cv.nb.html)
- [Explicación y práctica guiada - Notebook](/clase_2/notebook/cart_notebook.nb.html)
- [Explicación y práctica guiada - RCode](/clase_2/scripts/cart_script.R)
- [Slides CART - pdf](/clase_2/slides/cart.pdf)

Pueden descargarse la totalidad de los materiales del repositorio para trabajar en un único archivo .zip

- [![](img/Download.png)](clase_2.zip)


# Clase 3. 
- [Slides Bagging/RF - pdf](/clase_3/slides/bagging.pdf)
- [Repaso flujo de trabajo](/clase_3/slides/workflow.pdf)
- [Explicación y práctica guiada - Clasificación - Notebook](/clase_3/notebook/rf_boosting_notebook.nb.html)
- [Explicación y práctica guiada - Clasificación - RCode](/clase_3/scripts/rf_boosting_script.R)


# Clase 4. 
- [Slides Boosting - pdf](/clase_3/slides/boosting.pdf)
- [Intuición Gradient Boosting Machine - Notebook](/clase_3/notebook/boosting_intuicion_notebook.nb.html)
- [Intuición Gradient Boosting Machine - RCode](/clase_3/scripts/boosting_intuicion_script.R)
- [Consignas práctica independiente - Notebook](/clase_3/notebook/practica.nb.html)
- [Soluciones práctica independiente - Notebook](/clase_3/notebook/solution_practica.nb.html)


# Clase 5. Machine Learning Interpretable, repaso y cierre

- [Interpretable ML - Notebook](/clase_4/notebook/interpretable_ml_notebook.nb.html)
- [Interpretable ML - RCode](/clase_4/scripts/interpretable_ml_script.R)
- [Consignas práctica independiente - Notebook](/clase_4/notebook/practica_independiente.nb.html)
- [Soluciones práctica independiente - Notebook](/clase_4/notebook/practica_independiente_solution.nb.html)



# Librerías a utilizar
El taller se desarrollará en R y se hará un uso extensivo de las siguientes librerías:

- `tidyverse`
- `caret`

Pueden instalarse utilizando las instrucciones:

```{r}
install.packages('tidyverse')  
install.packages('caret') 
```


# Bibliografía y sitios de consulta

- [James, G., Witten, D., Hastie, T. y Tibshirani, R. (2015), _Introduction to Statistical Learning_, Berlin: Springer.](http://faculty.marshall.usc.edu/gareth-james/ISL/)

- [Kuhn, M. (s/f), _The `caret` package](http://topepo.github.io/caret/index.html)

- [Molnar, C. (2020), _Interpretable Machine Learning. A Guide for Making Black Box Models Explainable.](https://christophm.github.io/interpretable-ml-book/)
