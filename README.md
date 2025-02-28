# MoC-Interfaces-SY-SDF

Interfaces for heterogeneous system modeling in ForSyDe using the SY, SDF and SADF MoCs

For an installation guide please check: [ForSyDe Setup Page](https://forsyde.github.io/forsyde-shallow/setup)

## Running the simulation

- Open a terminal in the repository folder and run the following command:
```sh
stack ghci --package forsyde-shallow Interfaces.hs
```

- In the ghc interpreter, check the input signal:
```sh
ghci> sTest
{1,2,_,10,3,_}
```

### Simulation of the case study from Fig. 7a

Run the system with the input signal
```sh
ghci> system1 sTest
{10,40,160,290}
```

### Simulation of the case study from Fig. 7b

Run the system with the input signal
```sh
ghci> system2 sTest
{10,40,_,160,290,_}
```

In the first example, the timing information is lost, and in the second exemple it is recovered.

### Simulation of the RISC-V processor

The RISC-V input signal is the sCtrl, it controls when it should switch from RV32I to RV32E. Run the following command to simulate the processor for 72 timestamps.
```sh
ghci> takeS 72 $ riscVsadf sCtrl
{_,0,1,_,1,_,2,_,_,_,_,3,_,_,_,_,5,_,_,_,_,8,_,_,_,_,
13,_,_,_,_,21,_,_,_,_,34,_,_,_,_,55,_,_,_,_,89,_,_,_,_,
144,_,_,_,_,233,_,_,_,_,377,_,_,_,_,610,_,_,_,_,987}
```

