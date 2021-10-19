# Revision history for ValveValueKeyvalue

## 1.1.0.0 -- yyyy-mm-dd

* Changed class to use Either
* Updated documentation
* Changed export order so haddock makes better documentation
* Added tested-with in cabal file
* Changed list instance of ValveVKV to always return Right. In case no items are found, return Right []. To get a list that is certain to not be empty, use NonEmpty from the Data.List.NonEmpty module in base
* Fixed example in readme

## 1.0.1.0 -- 2021-09-28

* Added parseToVKV function and added some documentation

## 1.0.0.0 -- 2021-09-27

* First version. Released on an unsuspecting world.
