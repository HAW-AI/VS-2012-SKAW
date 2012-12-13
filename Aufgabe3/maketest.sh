mkdir ./test
mkdir ./test/bank
mkdir ./test/filiale
mkdir ./test/geldautomat

echo copying branch_access...
cp -r ./build/classes/branch_access ./test/bank

cp -r ./build/classes/branch_access ./test/filiale

echo copying cach_access...
cp -r ./build/classes/cash_access ./test/bank

cp -r ./build/classes/cash_access ./test/geldautomat

echo copying nameservice...
cp -r ./build/classes/ns ./test/nameservice

echo copying mware_lib...
cp -r ./build/classes/mware_lib ./test/bank
cp -r ./build/classes/mware_lib ./test/filiale
cp -r ./build/classes/mware_lib ./test/geldautomat


