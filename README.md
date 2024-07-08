# `policyoptimizer`
County-Level Policy Optimizer for Cost-Effective Reduction of Transportation Emissions

- Team Lead: Tim Fraser, PhD
- Principal Investigator: Oliver Gao, PhD
- Description: repository for `policyoptimizer` R package(s) and validation analyses.

## What is `policyoptimizer`?

`policyoptimizer` is an `R` package for emissions policy optimization based on Cornell's server of previous MOVES analyses and the federal CMAQ database of past transportation emissions policies' cost effectiveness stats.

## How do I use `policyoptimizer`?

### Using `policyoptimizer` as a member of the public

Our team at Gao Labs is currently developing a REST API that anyone can use to connect to Cornell's CATSERVER, including `policyoptimizer` users. We expect this online interface to be available by the end of 2024.

### Using `policyoptimizer` in your research

To use MOVESLite as an external user, you will need `CATSERVER` credentials to access the Cornell CATSERVER that MOVESLite queries. Since these credentials are sensitive, CATSERVER access is currently limited to research and development personnel. If you would like to be one of our user testing partners for `policyoptimizer`, reach out to Dr. Tim Fraser at Gao Labs @ Cornell at <tmf77@cornell.edu>.


## Setup for `policyoptimizer`

To use `policyoptimizer`, you will need an active `RStudio` coding environment.  You can install the most recent version of the package from github using our `workflow.R` script, which contains helpful examples of how to use it. (Or, you can build the package from source using our `dev.R` script in this repository.)

### Configuring `CATSERVER` credentials

These credentials are environmental variables, which should be saved in a `.Renviron` file in your main directory. 

Your `.Renviron` file might look like this:

```
CATSERVER_USERNAME="someusernamehere"
CATSERVER_PASSWORD="somepasswordhere"
CATSERVER_HOST="someurl.cornell.edu"
CATSERVER_PORT=anumberhere
```



### Links

- For more tools by Gao Labs @ Cornell, see our Github Organization Page here at https://github.com/Gao-Labs
- For more information about Gao Labs, see our website at https://gao.cee.cornell.edu/
