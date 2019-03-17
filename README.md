# Definite clause grammar parser

A parser for a defnite clause grammar of English-Light written in Prolog.English Light is a fragment of English (i.e., English-Light does not include any ungrammatical English sentences) which accounts to structures as those appearing in the following examples:

1. The young boy who worked for the old man pushed and stored a big box in the large
empty room after school.
2. The old woman and the old man gave the poor young man a white envelope in the shed
behind the building.
3. Every boy quickly climbed some big tree while every girl secretly watched some boy.
4. Some brilliant students and many professors watched and admired talented lecturers
and appreciated bright scientists and researchers.

## Sample runs

```prolog
s(T,[the,young,boy,who,worked,for,the,old,man,pushed,and,stored,a,big,box,in,the,large,empty,room,after,school],[]).
s(T,[the,old,woman,and,the,old,man,gave,the,poor,young,man,a,white,envelope,in,the,shed,behind,the,building],[]).
s(T,[every,boy,quickly,climbed,some,big,tree,while,every,girl,secretly,watched,some,boy],[]).
s(T,[some,brilliant,students,and,many,professors,watched,and,admired,talented,lecturers,and,appreciated,bright,scientists,and,researchers],[]).
```

