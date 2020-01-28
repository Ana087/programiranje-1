# 3.

# a)

# n = sirina balkona
# m = stevilo korit
# l = sirina korita

# test = stevilo_postavitev(9, 3, 2)

from functools import lru_cache


def stevilo_postavitev(n, m, l):

    @lru_cache(maxsize=None)

    def postavitve(mesto, korita):
        moznosti = 0
        if mesto > n:
            return 0
        else:
            while korita > 0:
                mesto += (l + 1)
                korita -= 1
            moznosti += 1

        return moznosti

    return postavitve(0, 0)
        



# b)

def razlicne_sirine(n, sez):
    return None