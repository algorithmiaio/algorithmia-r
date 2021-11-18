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

* Verify that the `Version` field in the [DESCRIPTION](DESCRIPTION) file has had its version updated per proper [semantic versioning](https://semver.org/) from [the previous version in CRAN](https://cran.r-project.org/web/packages/algorithmia/index.html)
* Ensure that CI build passes successfully
* Inside a Docker container with the image for that stage with the local directory bind-mounted (ex: `docker run -it --rm -v `pwd`:/algorithmia-r <docker image from .gitlab-ci.yml>`)
  * `cd /algorithmia-r`
  * Run all commands `test:check_as_cran` CI stage defined in `.gitlab-ci.yml`
* Verify that the submission follows [all CRAN policies](https://cran.r-project.org/web/packages/policies.html).
* Submit the new package https://xmpalantir.wu.ac.at/cransubmit/
  * Name: Robert Fulton
  * Email: rfulton@datarobot.com
  * Package: <the `tar.gz` file that was created from the build>
  * Optional comment: <leave blank>
* Monitor email to rfulton@datarobot.com for any CRAN emails
  * At the very least, an email should come through asking to confirm the submission. This must be done before the submission will appear in CRAN.
* Wait for the version of [the library in CRAN](https://cran.r-project.org/web/packages/algorithmia/index.html) is updated with the changes submitted
* Create a new Git tag on the commit submitted to CRAN. This should include the major, minor, and patch versions (ex: `0.3.0`).
* Push any Git tags / release branches up to Github.
