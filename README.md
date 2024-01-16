# %make / %vapor

> **Disclaimer**: This project is no longer maintained. Feel free to fork and
improve it if you'd like - it is [vaporware](LICENSE) after all.
>
> Future development of this project will be continued by the primary author, [ryjm](https://github.com/ryjm). Special thanks to [rabsef-bicrym](https://github.com/rabsef-bicrym) for pioneering the initial product and [xiphiness](https://github.com/xiphiness) for important contributions.

%make is a suite of agents that use a shrubbery-like strategy to watch Ethereum addresses and potentially take action in Urbit as a result of transactions. An example use case (Vaporware's): When an NFT is minted, distribute an app to the new owner.

## nix

- clean-deploy: removes the pier desk entirely and resyncs. also commits and revives the project.
- deploy: cleans build dir, syncs deps, copies to ship. does not automatically commit.

## no nix

- run `make clean && make && make install`
