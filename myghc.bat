ghc -outputdir ./out -threaded -prof -Wall -fwarn-tabs -fwarn-incomplete-uni-patterns -fwarn-incomplete-record-updates --make -O2 -ddump-prep -ddump-to-file -main-is %1.main%1 -o ./bin/%1.exe ./src/%1.hs
echo myghc ran
