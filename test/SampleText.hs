{-# LANGUAGE QuasiQuotes #-}

import Text.RawString.QQ

paragraph :: String
paragraph =
  [r|
A paragraph is simply one or more consecutive lines of text, separated
by one or more blank lines. (A blank line is any line that looks like a
blank line -- a line containing nothing but spaces or tabs is considered
blank.) Normal paragraphs should not be indented with spaces or tabs.
|]

tableSimple :: String
tableSimple =
  [r|
| Syntax | Description |
| ----------- | ----------- |
| Header | Title |
| Paragraph | Text |
|]



-- headers and paragraphs only
blockTextSimple :: String
blockTextSimple =
  [r|
# Heading 1 Example

## Heading 2 Example

### Heading 3 Example

A paragraph is simply one or more consecutive lines of text, separated
by one or more blank lines. (A blank line is any line that looks like a
blank line -- a line containing nothing but spaces or tabs is considered
blank.) Normal paragraphs should not be indented with spaces or tabs.
|]

-- all elements
blockTextComplete :: String
blockTextComplete =
  [r|
# Heading 1 Example
---
```
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _f base [] = base
foldr f base (x : xs) = x `f` foldr f base xs
```

We should also support **bold text** and *italic text*. Also with links like [this](http://example.com/).

List

- list item 1
- list item 2

Ordered List

1. list item 1
1. list item 2
1. list item 3

> blockquotes would look like this.

|]