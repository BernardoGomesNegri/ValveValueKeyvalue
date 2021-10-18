# This only works if you have 7-Zip installed, on your path and are running on Windows
$version = "1.1.0.0"
cabal sdist -o ".\out"
cabal haddock --builddir=".\out" --haddock-for-hackage --enable-doc
Remove-Item ".\out\build" -Recurse
Remove-Item ".\out\cache" -Recurse
Remove-Item ".\out\packagedb" -Recurse
Remove-Item ".\out\tmp" -Recurse
$docs = ".\out\ValveValueKeyvalue-" + $version + "-docs.tar.gz"
$command = "7z.exe e " + $docs + " -y -oout -tgzip"
Invoke-Expression $command
Remove-Item $docs