# Nuage des points


## c-gravité-intuitive
```yaml 
id: c-gravité-intuitive
deck: analyse_des_données
tags: [chap1, nuage_des_points, moyenne empirique]
note_type: basic
```
Rappelez la formule intuitive du centre de gravité d'un nuage de points. On rappelle les notations de i-ème point et j-ème variable:
\[ x_i = (x_i^1, \ldots, x_i^p)' \in \mathbb{R}^p \quad x^j = \begin{pmatrix} x_1^j \\ \vdots \\ x_n^j \end{pmatrix} \in \mathbb{R}^n \]

Le centre de gravité d'un nuage de points \( X = (x_i^j), \: j \in \llbracket 1, p \rrbracket \: i \in \llbracket 1, n \rrbracket \) est donné par:
\[ G = \sum_{i=1}^n p_i \cdot x_i \in \mathbb{R}^p \]
où \( \sum_{i=1}^n p_i = 1 \) et \( p_i \geq 0 \).
\[ g = \begin{pmatrix} g_1 \\ \vdots \\ g_p \end{pmatrix} \quad g_i = \sum_{i=1}^n p_i \cdot x_i^j \]


## c-gravité-géométrique
```yaml 
id: c-gravité-formule
deck: analyse_des_données
tags: [chap1, nuage_des_points, moyenne empirique]
note_type: basic
```
On rappelle la formule du centre de gravité d'un nuage de points.
\[ G = \sum_{i=1}^n p_i \cdot x_i \in \mathbb{R}^p \]
où \( \sum_{i=1}^n p_i = 1 \) et \( p_i \geq 0 \).
\[ g = \begin{pmatrix} g_1 \\ \vdots \\ g_p \end{pmatrix} \quad g_i = \sum_{i=1}^n p_i \cdot x_i^j \]
Donne une formule matricielle de \( g \) et une interprétation géométrique de j-ème composante \( g_j \) du centre de gravité.

\[ g = X' D_p \mathbf{1}_n \in \mathbb{R}^p, \quad D_p = \text{diag}(p_1, \ldots, p_n) \]
Remarquons que \( g_j = \langle x^j, \mathbf{1}_n \rangle_{D_p}\) est l'abscisse de la projection orthogonale pour la metrique \( D_p \) de \( x^j \) sur \( \text{Vect}(\mathbf{1}_n) \).


## tableau-centre
```yaml
id: tableau-centre
deck: analyse_des_données
tags: [chap1, nuage_des_points, moyenne empirique, tableau centré]
note_type: basic
```
Comment centre-t-on un tableau de données \( X \) par rapport au centre de gravité, donne une formule intuitive. 

Le tableau centré \( Y \) est donné par:
\[ y_i^j = x_i^j - g_j \quad \text{ou} \quad y_i = x_i - g\]


## tableau-centre-matriciel
```yaml
id: tableau-centre-matriciel
deck: analyse_des_données
tags: [chap1, nuage_des_points, moyenne empirique, matrice]
note_type: basic
```
Donne une formule matricielle du tableau centré \( Y \) par rapport au tableau de données \( X \) et au centre de gravité \( g \). On rappelle que le tableau centré \( Y \) est donné par:
\( y_i^j = x_i^j - g_j, \quad y_i = x_i - g \)

\[ Y = X - \begin{pmatrix} g' \\ \vdots \\ g' \end{pmatrix} \]
\[ Y = X - \mathbf{1}_n g' \]
Par ailleurs, on a aussi une nouvelle interprétation pour \( y^j \):
\[ y^j = x^j - g_j \mathbf{1}_n = x^j - \langle x^j, \mathbf{1}_n \rangle_{D_p} \mathbf{1}_n \]
C'est la projection orthogonale de \( x^j \) sur \( \text{Vect}(\mathbf{1}_n)^{\perp} \) pour la métrique \( D_p \).


## support-de-nuage
```yaml
id: support-de-nuage
deck: analyse_des_données
tags: [chap1, nuage_des_points, espace_affine]
note_type: basic
```
Donne la définition du support d'un nuage de points \( X = (x_i^j), \text{ et } Y = X - \mathbf{1}_n g' \).

Le support \( \text{Supp}(X) \) est le plus petit sous-espace affine de \( \mathbb{R}^p \) contenant tous les points \( x_i \)
\[ \text{Supp}(X) = g + \text{Vect}(y_1, \ldots, y_n) = g + \text{Vect}(Y) \]


