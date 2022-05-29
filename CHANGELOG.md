# Changelog
All notable changes to this project will be documented in this file.

## [1.0.1] - 2022-05-29
### Changed
 - Fixed issue with parser exception where file names was too long, solved with compression.

## [1.0.0] - 2022-05-28
### Added
- Add functionality to get Pagerank
- Mergfing of core functionality into App
- Utils functions :
  - getParsePath
  - getParseLinksPath
  - getParseWordsPath
  - writeToFileUTF8
### Changed
- Move Index.hs functionality to App.hs
- Update Index.hs
- Merged with pagerank.hs
- Update indexing docId
- Fixing minor warnings
- Fixed issue with fileWrite of special characters on windows
- Fixed unclosed file handles
- Fixing one error in index.hs

## [0.6.0] - 2022-05-27
### Added
 - Base concept of initApp and searchText functions into App module
 - Implemented parsing of links into Parser module
 - Added Utils functions:
   - toLowerStringArr 
   - toLowerString 
   - uniqArr
### Changed
- Parsed words are now uniq and lowercased
### Removed 
 - Data misc

## [0.5.4] - 2022-05-27
### Changed
- Fixing some warnings
- Remove unused libraries 

## [0.5.3] - 2022-05-27
### Changed
- Significantly optimized time complexity of removePairTags function.
- Project now default enables -Wall ghc option
- Project now default enables O2 ghc optimization

## [0.5.2] - 2022-05-26
### Added
- Utils funsions:
   - putTimeDiffFormatted 
   - lPadNumber 
### Changed
- Formatted text of parser. Log now includes duration of parse.

## [0.5.1] - 2022-05-26
### Changed
 - Update Index.hs
 - Update Utils.hs
 - Prepare funcionality for Pagerank
 - Separated Index.hs into own module

## [0.5.0] - 2022-05-26
### Changed
 - Add new dependecies to cabal file
 - Add new extension to devcontainer.json
 - Update Index.hs:
    - update funcionality for searching
    - decode hashed pages to normal text
    - Add new functionalities for reverse index
 - Testing funcionality -> inverted_index.txt
 - Add parse-words to gitignore

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
- Setup of VS Code remote containers. It includes setup of haskell environment and VS code extensions to correctly work with haskell
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


[1.0.1]: https://github.com/xsivan/FP_haskell/compare/1.0.0...1.0.1
[1.0.0]: https://github.com/xsivan/FP_haskell/compare/0.6.0...1.0.0
[0.6.0]: https://github.com/xsivan/FP_haskell/compare/0.5.4...0.6.0
[0.5.4]: https://github.com/xsivan/FP_haskell/compare/0.5.3...0.5.4
[0.5.3]: https://github.com/xsivan/FP_haskell/compare/0.5.2...0.5.3
[0.5.2]: https://github.com/xsivan/FP_haskell/compare/0.5.1...0.5.2
[0.5.1]: https://github.com/xsivan/FP_haskell/compare/0.5.0...0.5.1
[0.5.0]: https://github.com/xsivan/FP_haskell/compare/0.4.1...0.5.0
[0.4.1]: https://github.com/xsivan/FP_haskell/compare/0.4.0...0.4.1
[0.4.0]: https://github.com/xsivan/FP_haskell/compare/0.3.0...0.4.0
[0.3.0]: https://github.com/xsivan/FP_haskell/compare/0.2.1...0.3.0
[0.2.1]: https://github.com/xsivan/FP_haskell/compare/0.2.0...0.2.1
[0.2.0]: https://github.com/xsivan/FP_haskell/compare/0.1.0...0.2.0
[0.1.0]: https://github.com/xsivan/FP_haskell/compare/0.0.2...0.1.0
[0.0.2]: https://github.com/xsivan/FP_haskell/compare/0.0.1...0.0.2
[0.0.1]: https://github.com/xsivan/FP_haskell/compare/0.0.0...0.0.1