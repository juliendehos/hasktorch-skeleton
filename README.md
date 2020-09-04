
```
./setup-data.sh
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/run/opengl-driver/lib
export DEVICE="cuda:0"
cabal run mnist-cnn                                   
```

