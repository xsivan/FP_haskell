# Changelog
All notable changes to this project will be documented in this file.

<<<<<<< HEAD
=======
## [0.4.1] - 2022-05-24
### Changed
 - Fixed changelog typos 

## [0.4.0] - 2022-05-24
### Added
- Input collection of first 100 lines of Brasilian pages
- New Utils module with function shared across other modules. This modules includes the following functions:
  - decodeFileName
  - encodeFileName
  - indexOf
  - indexOfReverse
  - recreateDir
  - removeSubString
  - subString
  - validateFile
- JSON parsing functionality to read content of JL file
### Changed
- Parser now loops pages from JL file instead of html files inside specific folder
- Significantly optimized time complexity of removePairTag function
- Parser now names output files as base64 encode of cleaned URL. Cleaned URL is content url without redundant parts like: 
  - scheme
  - protocol
  - fragment
  - query params

## [0.3.0] - 2022-05-21
### Added
- Basic version of words parsing from html content was created, for now these words are not cleaned, it will be added in the future version
- Simple documentation of Parser functions
### Changed
- Replaced function with not optimal complexity for better ones
### Removed
- Data pages, input data will be added in the near future in differend format 

>>>>>>> b0c92ab4de870c683fb7faf439860652d34378ab
## [0.2.1] - 2022-05-08
### Changed
 - Versionning 

## [0.2.0] - 2022-05-07
### Added
- Parser executable with not fully implemented functionality to parse words from web files in directory. For now are implemented following parts:
  - Automatic creation of destination directory
  - Main loop via html files of source directory
  - Creation of destination file with some dummy content
  - Progress logging
### Changed
- Parse example

## [0.1.0] - 2022-05-06
### Added
<<<<<<< HEAD
- Setup of VS Code remote containers. It includes setup of haskell environment & VS code extensions to correctly work with haskell
=======
- Setup of VS Code remote containers. It includes setup of haskell environment and VS code extensions to correctly work with haskell
>>>>>>> b0c92ab4de870c683fb7faf439860652d34378ab
- Real input data
### Changed
- Readme file

## [0.0.2] - 2022-05-04
### Added
- Cabal config
- Changelog
- Docker setup via docker compose
- Gitignore
### Changed
- Renamed main code folder structure from app to src
- Updated content of Readme file, include instructions for docker setup

## [0.0.1] - 2022-04-02
### Added
- Base src structure with main
- Licence file
- List of stop words
- Readme file

<<<<<<< HEAD
## [0.0.3] - 2022-05-17
### update cabal
- add new dependencies

## [1.0.0] - 2022-05-17
### Add new file hs
- Create Index.hs
- Create inverted_index.txt
- Add functionalities to Index.hs

[1.0.0]: https://github.com/xsivan/FP_haskell/compare/0.0.0...1.0.0
[0.2.1]: https://github.com/xsivan/FP_haskell/compare/0.2.0...0.2.1
[0.2.0]: https://github.com/xsivan/FP_haskell/compare/0.1.0...0.2.0
[0.1.0]: https://github.com/xsivan/FP_haskell/compare/0.0.2...0.1.0
[0.0.3]: https://github.com/xsivan/FP_haskell/compare/main...0.0.3
=======
[0.4.1]: https://github.com/xsivan/FP_haskell/compare/0.4.0...0.4.1
[0.4.0]: https://github.com/xsivan/FP_haskell/compare/0.3.0...0.4.0
[0.3.0]: https://github.com/xsivan/FP_haskell/compare/0.2.1...0.3.0
[0.2.1]: https://github.com/xsivan/FP_haskell/compare/0.2.0...0.2.1
[0.2.0]: https://github.com/xsivan/FP_haskell/compare/0.1.0...0.2.0
[0.1.0]: https://github.com/xsivan/FP_haskell/compare/0.0.2...0.1.0
>>>>>>> b0c92ab4de870c683fb7faf439860652d34378ab
[0.0.2]: https://github.com/xsivan/FP_haskell/compare/0.0.1...0.0.2
[0.0.1]: https://github.com/xsivan/FP_haskell/compare/0.0.0...0.0.1