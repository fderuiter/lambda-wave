def to_camel_case(snake_str):
    components = snake_str.split('_')
    if not components:
        return ""
    return components[0] + "".join(x.capitalize() for x in components[1:])

def capitalize_first(s):
    if not s:
        return ""
    return s[0].upper() + s[1:]

def to_snake_case(s):
    import re
    s = re.sub('(.)([A-Z][a-z]+)', r'\1_\2', s)
    return re.sub('([a-z0-9])([A-Z])', r'\1_\2', s).lower()
