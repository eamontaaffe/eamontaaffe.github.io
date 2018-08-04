[@bs.val] [@bs.scope "JSON"] external prettify:
  ('a, [@bs.as {json|null|json}] _, [@bs.as 2] _) => string = "stringify";
