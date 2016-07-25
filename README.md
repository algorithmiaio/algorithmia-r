# algorithmia-r
R client library for accessing the Algorithmia API

# Calling algorithms
First make a client object:

```
client <- getAlgorithmiaClient("ALGORITHMIA_API_KEY")
```

Then get the algorithm object:

```
algorithm <- client$algo("algo://AUTHOR/ALGORITHM_NAME/VERSION")
```

Call pipe passing in the input to the algorithm:

```
response <- algorithm$pipe(input)
```

There are two fields in the result. The meta data (which has timing data) and the result.

```
result <- response$result
```
