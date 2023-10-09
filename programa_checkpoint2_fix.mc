:STATEMENTS
numero1:INT
numero2:INT
numero3:INT
aux:INT
_TESTE2:FLOAT

:ALGORITHM
# Coloca 3 números em ordem crescente
INPUT numero1
INPUT numero2
INPUT numero3
IF numero1 > numero2 THEN
      aux = 2+3-4+5-6*5-1
      numero2 = numero1
      numero1 = aux 
IF numero1 > numero3 AND numero2 <= numero4 AND numero1 > 3 OR numero2 != numero4 THEN
      aux = (numero3)
      numero3 = numero1
      numero1 = aux
IF numero2 > numero3 THEN
      aux = numero3
      numero3 = numero2
      numero2 = aux
PRINT(numero1)
PRINT(numero2)
PRINT(numero3)
PRINT("TESTE")