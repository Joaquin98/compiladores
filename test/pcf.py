numbers = [4,3,2,1]

base = 255

def to_pcf_list (nums):
    pcfList = "0"
    for n in nums[::-1]:
        pcfList = "(add " + str(n) + " " + pcfList + ")"
    return pcfList

def nat_to_list(n):
    ans = []
    while n != 0:
        ans.append(n%base)
        n //= base 
    return ans

def list_to_nat(nums):
    return sum([n * (base**i) for (i,n) in enumerate(nums)])
n2 = 2017551


print(to_pcf_list(numbers))
n = list_to_nat(numbers)
print(list_to_nat(numbers))
print(nat_to_list(n))
print(nat_to_list(n2))