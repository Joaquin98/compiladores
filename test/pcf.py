# Solo sirve para list.pcf1 (base 256)

base = 256

# Toma una lista de python y retorna el string que representa la lista en pcf1
def to_pcf_list (nums):
    pcfList = "0"
    for n in nums[::-1]:
        pcfList = "(add " + str(n) + " " + pcfList + ")"
    return pcfList

# Toma el natural y retorna la lista que representa.
def nat_to_list(n):
    ans = []
    while n != 0:
        ans.append(n%base)
        n //= base 
    return ans

# Toma una lista y la convierte en el número que la representa.
def list_to_nat(nums):
    ans = sum([n * (base**i) for (i,n) in enumerate(nums)])
    return ans


def main():


    resNum = int(input("Ingrese el numero resultante: "))
    print("La lista resultante es :", nat_to_list(resNum))

"""
# Usar este fragmento para insertar una lista y que devuelva
# el codigo de pcf1 correspondiente y luego chequear que de
# lo mismo que aca.

    list256 = [23,12,4,2]
    print("Lista original: ", list256)
    print("En PCF1: ", to_pcf_list(list256))
    print("Numero que lo representa: ", list_to_nat(list256))
    list256Sorted = sorted(list256)
    print("Lista al ordenar: ", list256Sorted)
    print("Numero que lo representa: ", list_to_nat(list256Sorted))
    print("En PCF1: ", to_pcf_list(list256Sorted))
"""


if __name__ == "__main__":
    main()