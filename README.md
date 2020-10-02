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

* * * * *

## Deployment

### New feature release without breaking API changes

* Verify that the `Version` field in the [DESCRIPTION](DESCRIPTION) file has had its minor release version (y in x.y.z incremented from the previously released version, and that the patch version (z in x.y.z) is set to `0`.
* Ensure that CI build passes successfully
* Run `R CMD build .`
* Verify that the submission follows [all CRAN policies](https://cran.r-project.org/web/packages/policies.html).
* Submit the new package https://xmpalantir.wu.ac.at/cransubmit/
  * Name: Robert Fulton
  * Email: rfulton@algorithmia.com
  * Package: <the `tar.gz` file that was created from the build>
  * Optional comment: <leave blank>
