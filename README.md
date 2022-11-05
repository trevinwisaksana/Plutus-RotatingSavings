# Plutus Rotating Savings
A proof-of-concept Plutus script that mimics the functionality of a rotating savings session. [Rotating savings](https://www.investopedia.com/terms/r/rotating-credit-and-savings-association.asp#:~:text=A%20rotating%20savings%20and%20credit%20association%20(ROSCA)%20is%20a%20group,the%20funds%20at%20each%20meeting) is a way to provide loans to people within a community without paying any interest. This practice has been documented to have occurred as early as 200 B.C.

[![Code in Cardano Workspace](https://demeter.run/code/badge.svg)](https://demeter.run/code?repository=https://github.com/trevinwisaksana/plutus-rotating-savings/tree/improvement/demeter-run)

## How does it work?
1. A user calls the `startSession` endpoint (up to 12 people can join per session), sets the join deadline and pays the amount he/she wants to be staked
2. Participants can join a session by calling `joinSession` and pays the amount of stake he/she wants to place
3. After the join deadline has passed, a member of the session can call the `raffle` endpoint to randomly determine the winner. The winner gets all the money staked in the session.
4. The session then continues and the winner cannot win again until everyone gets a chance to win
5. Each member can then call `makePayment` to continue adding their stake to session. The idea is each winner has to pay the money they won back over time.
6. A member can leave at anytime, but cannot reclaim the amount they staked

## Ways to improve
- The script should prevent someone from winning and then leaving immediately
- Script may be very heavy and unoptimized
- Members shouldn't be able to call raffle anytime after the deadline has passed but in time intervals instead

## How to run
1. Change directory to the project's root folder
2. Run `cabal repl`
3. Call the test functions in the file
- `startSessionTest`
- `joinSessionTest`
- `raffleTest`
- `raffleTwiceTest`
- `makePaymentTest`
- `leaveSessionTest`
- `everyoneWinsTest`
- `failRaffleWithoutJoiningTest`
