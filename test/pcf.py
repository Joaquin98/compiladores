
def to_pcf_list (nums):
    pcfList = "0"
    for n in nums[::-1]:
        pcfList = "(add " + str(n) + " " + pcfList + ")"
    print(pcfList)
    return pcfList

def nat_to_list(n):
    ans = []
    while n != 0:
        ans.append(n%base)
        n //= base 
    print(ans)
    return ans

def list_to_nat(nums):
    ans = sum([n * (base**i) for (i,n) in enumerate(nums)])
    print(ans)
    return ans



base = 256
numbers = [23,12,4,2]
n2 = 2017551

to_pcf_list(numbers)
n = list_to_nat(numbers)
list_to_nat(numbers)
nat_to_list(n)
nat_to_list(386663426)
nat_to_list(n2)