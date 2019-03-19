import Data.List
listaDeListas=[["Young","Young","Young","Young","Young","Middle","Middle","Middle","Middle","Middle","Old","Old","Old","Old","Old"],["False","False","True","True","False","False","False","True","False","False","False","False","True","True","False"],["False","False","False","True","False","False","False","True","True","True","True","True","False","False","False"],["Fair","Good","Good","Fair","Fair","Fair","Good","Good","Excellent","Excellent","Excellent","Good","Good","Excellent","Fair"],["No","No","Yes","Yes","No","No","No","Yes","Yes","Yes","Yes","Yes","Yes","Yes","No"]]
encabezados=["Age","HasJob","Own_House","Credit_Rating","Class"]
--listaDeListas=[["Sunny","Sunny","Windy","Rainy","Rainy","Rainy","Windy","Windy","Windy","Sunny"],["Yes","No","Yes","Yes","No","Yes","No","No","Yes","No"],["Rich","Rich","Rich","Poor","Rich","Poor","Poor","Rich","Rich","Rich"],["Cinema","Tennis","Cinema","Cinema","Stay in","Cinema","Cinema","Shopping","Cinema","Tennis"]]
--encabezados=["Weather","Parents","Money"] --,"Decision (Category)"]
--encabezados=["Outlook","Temp","Humidity", "Windy","Play Golf"]
--listaDeListas=[["rainy","rainy","overcast","sunny","sunny","sunny","overcast","rainy","rainy","sunny","rainy","overcast","overcast","sunny"],["Hot","Hot","Hot","Mild","Cool","Cool","Cool","Mild","Cool","Mild","Mild","Mild","Hot","Mild"],["High","High","High","High","Normal","Normal","Normal","High","Normal","Normal","Normal","High","Normal","High"],["false","true","false","false","false","true","true","false","false","false","true","true","false","true"],["No","No","Yes","Yes","Yes","No","Yes","No","Yes","Yes","Yes","Yes","Yes","No"]]
--encabezados=["Outlook","Temperature","Humidity","Windy","Play(Clasificacion)"]
--listaDeListas=[["sunny","sunny","overcast","rainy","rainy","rainy","overcast","sunny","sunny","rainy","sunny","overcast","overcast","rainy"],["hot","hot","hot","mild","cool","cool","cool","mild","cool","mild","mild","mild","hot","mild"],["high","high","high","high","normal","normal","normal","high","normal","normal","normal","high","normal","high"],["false","true","false","false","false","true","true","false","false","false","true","true","false","true"],["no","no","yes","yes","yes","no","yes","no","yes","yes","yes","yes","yes","no"]]
---------------------------------------------------------------------------------------------------
entropiaGlobal a b = (b/a) * logEnBase2  (b/a)
---------------------------------------------------------------------------------------------------
logEnBase :: Float -> Float -> Float
logEnBase b x = (log x) / (log b)
---------------------------------------------------------------------------------------------------
logEnBase2 :: Float -> Float
logEnBase2 = logEnBase 2
---------------------------------------------------------------------------------------------------
--entropiaPorCasoGlobal vecesaparece total = 0
entropiaPorCasoGlobal vecesaparece total =
  - entropiaGlobal total vecesaparece
---------------------------------------------------------------------------------------------------
entropiaPorCaso vecesaparece total 0 = 0
entropiaPorCaso vecesaparece total veces =
  - (vecesaparece/total) * entropiaGlobal vecesaparece veces
---------------------------------------------------------------------------------------------------
borrarElemento :: Eq a => a -> [a] -> [a]
borrarElemento _ [] = []
borrarElemento e (x:xs) = if (e == x) then xs else x : (borrarElemento e xs)
---------------------------------------------------------------------------------------------------
rep [] e = 0
rep (x:xs) e
   |x==e = 1 + rep xs e
   |otherwise = rep xs e
---------------------------------------------------------------------------------------------------
obtenerValoresPorPosicion [] _ = []
obtenerValoresPorPosicion (x:xs) listaclase =
     campo : obtenerValoresPorPosicion xs listaclase
 where
   campo = listaclase !!x
---------------------------------------------------------------------------------------------------
esCero elem
  |elem<=1 = 0
  |otherwise = 1
---------------------------------------------------------------------------------------------------
entroGlobal [] clasefinal total= []
entroGlobal (x:xs) clasefinal total=
  entro : entroGlobal xs clasefinal total
  where
    vecesaparecevalorClase = fromIntegral (rep clasefinal x)
    entro = entropiaPorCasoGlobal vecesaparecevalorClase total
---------------------------------------------------------------------------------------------------
recurEntropia [] totalElementos valoresClase vecesAparece = [0]
recurEntropia (x:xs) totalElementos valoresClase vecesAparece =
  entropia_aux : recurEntropia xs totalElementos valoresClase vecesAparece
  where
    vecesaparecevalorClase = fromIntegral (rep valoresClase x) -- cuento las veces que aparece ese valor
    entropia_aux = entropiaPorCaso vecesAparece totalElementos vecesaparecevalorClase
---------------------------------------------------------------------------------------------------
entropias :: Eq a => [a] -> [a] -> [a] -> [Float]
entropias [] lista listaclase = []
entropias (diferentesValores:xs) lista listaclase =
  entro : entropias xs lista listaclase
  where
    vecesAparece = fromIntegral (rep lista diferentesValores)  --obtengo cuantas veces aparece el elemento en la clase
    totalElementos = fromIntegral (length lista)   --obtengo el total de elementos
    indices = elemIndices diferentesValores lista -- indices del valor en la clase a calcular
    valoresClase = obtenerValoresPorPosicion indices listaclase -- obtengo las posiciones de la clase principal
    diferentesValoresClase = nub valoresClase -- obtengo cuantos valores diferentes hay en la clase principal en el rango de posiciones

    totalDiferentes = length diferentesValoresClase -- calculo el total de diferentes
    diferenciador = fromIntegral (esCero totalDiferentes) -- si hay un solo diferentes es una hoja por lo tanto es cero la respuesta

    entropia_aux = recurEntropia diferentesValoresClase totalElementos valoresClase vecesAparece
    entro = sum entropia_aux
---------------------------------------------------------------------------------------------------
entropiaPorClase [] listaClase = []
entropiaPorClase (x:xs) listaClase =
  suma : entropiaPorClase xs listaClase
  where
    listaValores = nub x
    entro = entropias listaValores x listaClase
    suma = sum entro
---------------------------------------------------------------------------------------------------
indicesDeCamino [] lista = []
indicesDeCamino (x:xs) lista =
  indices : indicesDeCamino xs lista
  where
    indices = elemIndices x lista
---------------------------------------------------------------------------------------------------
listasPorIndice indices [] = []
listasPorIndice indices (x:xs) =
  lista : listasPorIndice indices xs
  where
    lista = obtenerValoresPorPosicion indices x
---------------------------------------------------------------------------------------------------
nuevasListas lista [] = []
nuevasListas lista (x:xs) =
  listas : nuevasListas lista xs
  where
    listas = listasPorIndice x lista
---------------------------------------------------------------------------------------------------
esHoja e nodo h
 |e==1 = ("Es hoja " ++ h)
 |otherwise = ("La clase siguiente es nodo: " ++ nodo )
---------------------------------------------------------------------------------------------------
concatenarListas l1 l2 = l1 ++ l2
---------------------------------------------------------------------------------------------------
validarListas total l
 |total>1 = l
 |otherwise = []
---------------------------------------------------------------------------------------------------
validarEncabezados total enca_aux enca
  |total==1 = enca
  |otherwise = enca_aux
---------------------------------------------------------------------------------------------------
obtenerIndices [] _ = []
obtenerIndices (x:xs) lista =
  indice ++ obtenerIndices xs lista
  where
    indice = elemIndices x lista
---------------------------------------------------------------------------------------------------
general [] _ = []
general (x:xs) enca =
  eshoja : general recur encabezadosValidados
  where
    ultimo = length x
    claseLista = last x
    hoja = nub claseLista
    totalHoja = length hoja
    clases = take (ultimo-1) x
    totalClase = fromIntegral (length claseLista)
    nombreHoja = hoja !!0

    posiciones = obtenerIndices encabezados encabezados --sacar posiciones de listas que estoy trabajando
    valoresEncabezados = obtenerValoresPorPosicion posiciones encabezados

    entropiasporclase = entropiaPorClase clases claseLista
    entropiamenor = minimum entropiasporclase

    indiceEliminarEnLista = elemIndices entropiamenor entropiasporclase --se obtiene el indice de la entro menor
    indiceValor = indiceEliminarEnLista !!0 -- se obtiene el valor del indice
    nodoCabeza = valoresEncabezados !!indiceValor --se busca la cabeza
    caminosDiferentes = x !!indiceValor

    caminos = nub caminosDiferentes --se generan los caminos resultantes del nodo
    indicesNuevos = indicesDeCamino caminos caminosDiferentes

    listaNueva = borrarElemento caminosDiferentes x -- se elimina la clase ganadora
    nuevaslistas = nuevasListas listaNueva indicesNuevos -- se obtienen las nuevas listas de los nuevos caminos

    nodosImprimir = enca !!indiceValor -- se obtiene el valor del nodo a imprimir
    encabezados_aux = dropWhile (==nodoCabeza) enca -- elimino los encabezados ya usados
    encabezadosValidados = validarEncabezados totalHoja encabezados_aux enca -- se valida que no se eliminen encabezados cuando sea hoja

    listasinhoja = validarListas totalHoja nuevaslistas -- se validan que no se creen listas en hoja
    listasAMandar = reverse listasinhoja
    eshoja = esHoja totalHoja nodosImprimir nombreHoja -- string que determina si es hoja o nodo
    recur = concatenarListas listasAMandar xs --se concatenan los nodos resultantes y la cola faltante
---------------------------------------------------------------------------------------------------
main = do
  let clasefinal = last listaDeListas
  let diferentes = nub clasefinal
  let total = fromIntegral (length clasefinal)
  print diferentes
  let entropiasSeparadas = entroGlobal diferentes clasefinal total
  let entropiaglobal = sum entropiasSeparadas
  print "La entropia global calculada es: "
  print entropiaglobal

  let listafinal= general [listaDeListas] encabezados
  print listafinal
