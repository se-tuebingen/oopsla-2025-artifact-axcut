### axcut
| command                                |   mean | stddev | median | ... |
| -------------------------------------- | ------ | ------ | ------ | --- |
| ./axcut/factorial_accumulator 10000000 | 0.126… | 0.000… | 0.126… | ... |
| ./axcut/fibonacci_recursive 40         | 0.999… | 0.006… | 0.997… | ... |
| ./axcut/sum_range 10000000             | 0.460… | 0.003… | 0.461… | ... |
| ./axcut/iterate_increment 100000000    | 0.420… | 0.002… | 0.420… | ... |
| ./axcut/match_options 10000000         | 0.317… | 0.001… | 0.317… | ... |
| ./axcut/lookup_tree 10000000           | 0.384… | 0.002… | 0.385… | ... |
| ./axcut/erase_unused 10000             | 0.211… | 0.003… | 0.211… | ... |

### mlton
| command                                |   mean | stddev | median | ... |
| -------------------------------------- | ------ | ------ | ------ | --- |
| ./mlton/factorial_accumulator 10000000 | 0.115… | 0.000… | 0.115… | ... |
| ./mlton/fibonacci_recursive 40         | 0.821… | 0.007… | 0.820… | ... |
| ./mlton/sum_range 10000000             | 0.570… | 0.003… | 0.570… | ... |
| ./mlton/iterate_increment 100000000    | 0.106… | 0.000… | 0.106… | ... |
| ./mlton/match_options 10000000         | 0.137… | 0.001… | 0.138… | ... |
| ./mlton/lookup_tree 10000000           | 0.378… | 0.002… | 0.378… | ... |
| ./mlton/erase_unused 10000             | 0.027… | 0.000… | 0.027… | ... |

### ocaml
| command                                |   mean | stddev | median | ... |
| -------------------------------------- | ------ | ------ | ------ | --- |
| ./ocaml/factorial_accumulator 10000000 | 0.054… | 0.000… | 0.054… | ... |
| ./ocaml/fibonacci_recursive 40         | 0.535… | 0.005… | 0.534… | ... |
| ./ocaml/sum_range 10000000             | 5.244… | 0.038… | 5.236… | ... |
| ./ocaml/iterate_increment 100000000    | 0.159… | 0.001… | 0.158… | ... |
| ./ocaml/match_options 10000000         | 2.332… | 0.004… | 2.332… | ... |
| ./ocaml/lookup_tree 10000000           | 4.329… | 0.008… | 4.327… | ... |
| ./ocaml/erase_unused 10000             | 0.130… | 0.011… | 0.128… | ... |

### koka
| command                               |    mean | stddev |  median | ... |
| ------------------------------------- | ------- | ------ | ------- | --- |
| ./koka/factorial_accumulator 10000000 |  0.115… | 0.000… |  0.115… | ... |
| ./koka/fibonacci_recursive 40         |  0.517… | 0.005… |  0.516… | ... |
| ./koka/sum_range 10000000             |  2.835… | 0.026… |  2.839… | ... |
| ./koka/iterate_increment 100000000    |  0.737… | 0.006… |  0.738… | ... |
| ./koka/match_options 10000000         |  0.368… | 0.003… |  0.366… | ... |
| ./koka/lookup_tree 10000000           |  2.721… | 0.032… |  2.710… | ... |
| ./koka/erase_unused 10000             | 12.509… | 0.575… | 12.573… | ... |

### koka_opt
| command                                   |   mean | stddev | median | ... |
| ----------------------------------------- | ------ | ------ | ------ | --- |
| ./koka_opt/factorial_accumulator 10000000 | 0.117… | 0.004… | 0.115… | ... |
| ./koka_opt/fibonacci_recursive 40         | 0.187… | 0.015… | 0.184… | ... |
| ./koka_opt/sum_range 10000000             | 0.528… | 0.075… | 0.504… | ... |
| ./koka_opt/iterate_increment 100000000    | 0.001… | 0.000… | 0.001… | ... |
| ./koka_opt/match_options 10000000         | 0.320… | 0.040… | 0.295… | ... |
| ./koka_opt/lookup_tree 10000000           | 0.466… | 0.056… | 0.436… | ... |
| ./koka_opt/erase_unused 10000             | 0.466… | 0.028… | 0.453… | ... |

### rust
| command                               |   mean | stddev | median | ... |
| ------------------------------------- | ------ | ------ | ------ | --- |
| ./rust/factorial_accumulator 10000000 | 0.133… | 0.001… | 0.133… | ... |
| ./rust/fibonacci_recursive 40         | 0.780… | 0.047… | 0.765… | ... |
| ./rust/sum_range 10000000             | 1.779… | 0.203… | 1.774… | ... |
| ./rust/iterate_increment 100000000    | 0.605… | 0.003… | 0.605… | ... |
| ./rust/match_options 10000000         | 0.348… | 0.042… | 0.353… | ... |
| ./rust/lookup_tree 10000000           | 1.835… | 0.175… | 1.809… | ... |
| ./rust/erase_unused 10000             | 2.824… | 0.014… | 2.822… | ... |

### rust_opt
| command                                   |   mean | stddev | median | ... |
| ----------------------------------------- | ------ | ------ | ------ | --- |
| ./rust_opt/factorial_accumulator 10000000 | 0.038… | 0.000… | 0.038… | ... |
| ./rust_opt/fibonacci_recursive 40         | 0.310… | 0.001… | 0.310… | ... |
| ./rust_opt/sum_range 10000000             | 0.899… | 0.004… | 0.898… | ... |
| ./rust_opt/iterate_increment 100000000    | 0.002… | 0.001… | 0.003… | ... |
| ./rust_opt/match_options 10000000         | 0.002… | 0.001… | 0.002… | ... |
| ./rust_opt/lookup_tree 10000000           | 0.839… | 0.006… | 0.841… | ... |
| ./rust_opt/erase_unused 10000             | 1.175… | 0.008… | 1.175… | ... |

### mean
| benchmark             |  axcut |  mlton |  ocaml |    koka | koka_opt |   rust | rust_opt |
| --------------------- | ------ | ------ | ------ | ------- | -------- | ------ | -------- |
| factorial_accumulator | 0.126… | 0.115… | 0.054… |  0.115… |   0.117… | 0.133… |   0.038… |
| fibonacci_recursive   | 0.999… | 0.821… | 0.535… |  0.517… |   0.187… | 0.780… |   0.310… |
| sum_range             | 0.460… | 0.570… | 5.244… |  2.835… |   0.528… | 1.779… |   0.899… |
| iterate_increment     | 0.420… | 0.106… | 0.159… |  0.737… |   0.001… | 0.605… |   0.002… |
| match_options         | 0.317… | 0.137… | 2.332… |  0.368… |   0.320… | 0.348… |   0.002… |
| lookup_tree           | 0.384… | 0.378… | 4.329… |  2.721… |   0.466… | 1.835… |   0.839… |
| erase_unused          | 0.211… | 0.027… | 0.130… | 12.509… |   0.466… | 2.824… |   1.175… |
