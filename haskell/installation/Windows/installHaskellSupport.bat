@echo off
runghc --help > nul 2> nul || ( echo "haskell could not be run here," && echo "which means that haskell is not yet installed or was not installed properly." && echo "If you have not tried to install haskell yet, please follow the instructions to install haskell." && echo "If you have already tried to install haskell, please contact a teaching assistant." )
runghc --help > nul 2> nul && START /B /WAIT runghc "..\..\..\init\init.hs"
echo[
echo[
echo[
echo[
echo "Press any key to exit"
echo[
echo[
echo[
echo[
pause > nul