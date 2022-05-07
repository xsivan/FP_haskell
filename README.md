# What it is?
*___Group assignment from the subject of functional programming___*

# Project setup for Visual Studio Code
 - Haskell environment setup was fully automatized into docker via VS code extension named Remote Containers. Gguide how use this automatization is [here](./.devcontainer/README.md)

# Cabal cheatsheet
- Load project modules `cabal v2-repl <module, can be empty if only one is defined>`
  - After code changes you need to reload module with `:r`
- Dependencies install `cabal build --only-dependencies -j4` 
- Apply changes (flags) to modules `cabal install --overwrite-policy=always` e.q. after dependencies change

# Links:
- [Transparency in Keyword Faceted Search](https://datasetsearch.research.google.com/search?query=html%20pages&docid=L2cvMTFwendteW13cA%3D%3D)
- [The Anatomy of a Large-Scale Hypertextual Web Search Engine](http://papers.cumincad.org/data/works/att/2873.content.pdf)
- [Real World Haskell](http://book.realworldhaskell.org/read/)
- [What I wish I knew when learning haskell](http://dev.stephendiehl.com/hask/)

# Team members
- Bc. Lukáš Löbl
- Bc. Sebastián Ivan
- Bc. Pavol Švidraň
- ~~Bc. Juraj Rak~~