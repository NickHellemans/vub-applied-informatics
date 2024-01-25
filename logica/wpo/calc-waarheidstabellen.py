from itertools import product

def print_all_permutations():
    values = [0, 1]
    all_permutations = list(product(values, repeat=4))

    counter = 1
    for perm in all_permutations:
        print(f"tabel {counter}")
        print("p | q | r")
        print("----------")
        print(f"0 | 0 | {perm[0]}")
        print(f"0 | 1 | {perm[1]}")
        print(f"1 | 0 | {perm[2]}")
        print(f"1 | 1 | {perm[3]}")
        print("----------")
        counter += 1
# Call the function to print all permutations
print_all_permutations()