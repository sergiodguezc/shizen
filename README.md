# Shizen

Shizen is a powerful bio-based algorithm library written in Haskell. It leverages the capabilities of the Accelerate Library to parallelize algorithms and generate efficient CUDA code or LLVM code. The name "Shizen" (自然) is derived from the Japanese word for "nature," reflecting the library's focus on nature-inspired optimization techniques.

**Please note that Shizen is currently under active development, and the API is subject to change.**

## Algorithms

Shizen provides a range of algorithms that can be used for optimization and problem-solving tasks. The following algorithms are currently implemented in the library:

1. Ant Colony Optimization (ACO) *(complete, testing)*: ACO is a metaheuristic algorithm inspired by the foraging behavior of ants. It utilizes a pheromone-based communication system to find optimal solutions to combinatorial optimization problems. While the ACO implementation in Shizen is complete, it currently has performance issues and may not provide satisfactory results. We recommend using other available algorithms until the performance of ACO is improved in future updates.

2. Particle Swarm Optimization (PSO) *(complete, tested)*: PSO is a population-based optimization algorithm inspired by the collective behavior of bird flocks or fish schools. It uses a swarm of particles to explore the search space and converge towards the optimal solution. The PSO implementation in Shizen is complete and has been thoroughly tested for accuracy and efficiency.

3. Differential Evolution (DE) *(complete, tested)*: DE is an evolutionary algorithm that operates on a population of candidate solutions. It creates new solutions by combining and mutating existing solutions, allowing for exploration and exploitation of the search space. The DE implementation in Shizen is complete and has been extensively tested to ensure its effectiveness and reliability.

## Getting Started

To use Shizen in your Haskell projects, you will need to follow these steps:

1. Install Haskell and the necessary dependencies, including the Accelerate Library.

2. Clone the Shizen repository from GitHub or add it as a dependency to your project using your preferred package manager.

3. Import the Shizen library in your Haskell code and start using the implemented algorithms for your optimization tasks.

**Note:** As the library is still in development, it is recommended to consult the documentation and examples provided in the repository to understand the current API and usage patterns.

## Contributing

Shizen welcomes contributions from the open-source community. If you would like to contribute to the library, please follow these steps:

1. Fork the Shizen repository on GitHub.

2. Create a new branch for your contribution and make the necessary changes.

3. Write tests to ensure the correctness of your implementation.

4. Submit a pull request, providing a clear description of the changes you have made and any relevant information.

The maintainers of Shizen will review your contribution and provide feedback or merge it into the main repository.

## License

Shizen is open-source software released under the [BSD 3 License](https://opensource.org/licenses/BSD-3-Clause). You are free to use, modify, and distribute the library in accordance with the terms of the license.

## Contact

If you have any questions, suggestions, or feedback regarding Shizen, you can reach out to the maintainers through the GitHub repository's issue tracker or by contacting them directly via email.

We appreciate your interest in Shizen and hope that it proves to be a valuable tool for your optimization needs. Happy optimizing with nature-inspired algorithms!
