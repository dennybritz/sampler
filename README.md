Gibbs sampler for factor graphs, including weight learning.

Usage:

```
Usage: sampler [options]

  -w <weightsFile> | --weights <weightsFile>
        weights file in TSV format (required)
  -v <variablesFile> | --variables <variablesFile>
        variables file in TSV format (required)
  -f <factorsFile> | --factors <factorsFile>
        factors File in TSV format (required)
  -o <outputFile> | --outputFile <outputFile>
        output file path (required)
  -i <numSamplesInference> | --numSamplesInference <numSamplesInference>
        number of samples during inference (default: 100)
  -l <learningNumIterations> | --learningNumIterations <learningNumIterations>
        number of iterations during weight learning (default: 100)
  -s <learningNumSamples> | --learningNumSamples <learningNumSamples>
        number of samples per iteration during weight learning (default: 1)
  --alpha <learningRate>
        the learning rate for gradient descent (default: 0.1)
  --diminish <diminishRate>
        the diminish rate for learning (default: 0.95)
  -t <numThreads> | --threads <numThreads>
        This setting is no longer supported and will be ignored. The number of threads is automatically decided by the JVM.
```

Input format:

See [Deepdive developer guide](http://deepdive.stanford.edu/doc/developer.html)

Output format:

Two tab-separated files:

[outputFile].txt

```
[variable_id] [last_sample] [expectation]
```

[outputFile].txt.weights

```
[weight_id] [value]
```

