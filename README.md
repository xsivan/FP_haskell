# What it is?
Group assignment from the subject of functional programming

## Team
- Bc. Lukáš Löbl
- Bc. Sebastián Ivan
- Bc. Pavol Švidraň
- ~~Bc. Juraj Rak~~

## Links:
- [Transparency in Keyword Faceted Search](https://datasetsearch.research.google.com/search?query=html%20pages&docid=L2cvMTFwendteW13cA%3D%3D)
- [The Anatomy of a Large-Scale Hypertextual Web Search Engine](http://papers.cumincad.org/data/works/att/2873.content.pdf)
- [Real World Haskell](http://book.realworldhaskell.org/read/)
- [What I wish I knew when learning haskell](http://dev.stephendiehl.com/hask/)

## How to run haskell inside docker (Win)?
- Install docker Desktop
- In docker-server folder is compose.yml file, run it via interface if your IDE supports it
  - Else start command like `docker-compose.exe -f <path> up -d haskell` (probably absolute path for compose is required)
- Open terminal to newly created container via IDE if supports it
  - Else type command like `docker exec -it docker_apache_php_1 bash`
- Inside container type command `cabal v2-repl`, which setup GHCi to project module
- Now just call module function like :main to exe main function etc.