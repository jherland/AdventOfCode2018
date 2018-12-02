def first_dup(values):
    seen = set()
    for v in values:
        if v in seen:
            return v
        seen.add(v)
