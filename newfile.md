# Heading 1

This is `inline code`. 

##### A smaller heading

**bold**, *italic*, ~~struckthrough~~

```
fold :: (a -> b -> b) -> b -> [a] -> b
fold f z [] = z
fold f z (x:xs) = f x (fold f z xs)
```

3. This is a numbered list.
4. This is the second item.
5. This is the third item.

- This is an unordered list.
- ...

> Quotable quotes
> "Quote me!"

| Syntax | Description |
| ------ | ----------- |
| Header | Title |
| Paragraph | Text |
