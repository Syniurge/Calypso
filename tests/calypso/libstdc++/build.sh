#!/bin/bash

function maybeErrors() {
        if [ -f $target ]; then
          echo -e "\e[35mTHERE MAY HAVE BEEN SOME ERRORS THAT DIDN'T STOP COMPILATION.\e[0m"
        else
          echo -e "\e[35m\\nTHERE ARE ERRORS THAT STOPPED COMPILATION.\e[0m"
        fi

        # Errors during compilation! Deal with them and then return because there is no executable
        showErrorYN=""

        read -p $'\e[35mDo you want to view the errors?: [y/N] \e[0m' -n1 showErrorYN
        echo -e "\\n"

        if [ -z $showErrorYN ]; then
          showErrorYN="n"
        fi

        if [ $showErrorYN = "y" ]; then
          echo -e "$1\\n\\n\\n"
        fi
}

function builder() {

  # Chop off the '/' on the directory, if there is one
  target=${1%/}

  # If it's a directory then go ahead
  if [ -d "$1" ]; then

    # Should be a safe change of directory
    targetDir=$(cd -P -- "$(dirname -- "$0")/$1" && pwd -P)
    cd "$targetDir"

    YN=""
    echo -n -e "\e[32mCOMPILE $target.d: \e[0m\\n"
    echo -n -e "\e[33mDo you want to delete the old executable \"$target\" before compiling?: \e[0m"
    read -p $'\e[33m[Y/n] \e[0m' -n1 YN
    echo ""
    if [ -z $YN ]; then
      YN="y"
    fi

    if [ $YN = "n" ]; then
      echo -e "\e[1;33mNot removing executable \"$target\" and compiling.\e[0m"
    else
      echo -e "\e[1;33mRemoving \"$target\" and compiling.\e[0m"
      if [ -f $target ]; then
        /bin/rm $target
      fi
    fi

    # Compile here but save the output for later
    retVal=""
    errorVal=""
    retVal="$(ldc2 $targetDir/$target.d 2>&1 > /dev/null)"
    errorVal="$retVal"

    # Now check the output of the compile command and act on it
    # This error is specific to missing cpp arguments
    grepStrCPP="error: This file requires compiler"

    # Try to catch a few other errors
    grepStrPossibleERR="error: "
    grepStrPossibleUnhandled="Unhandled"
    grepStrPossibleErrCap="Error"

    case "$retVal" in
      *"$grepStrCPP"* )

        retValYN=""
        echo -e "\e[31mIt appears as though the file \"$target.d\" requires the -cpp-args -std=c++11 arguments. \e[0m"
        read -p $'\e[31mShould I add these command line arguments?: [Y/n] \e[0m' -n1 retValYN
        echo ""

        # Figure out whether we need to add the cpp arguments and recompile
        if [ -z $retValYN ]; then
          retValYN="y"
        fi

        if [ $retValYN = "n" ]; then
          cd ..
          return
        else
          # Save the errors here
          errorVal="$(ldc2 -cpp-args -std=c++11 -L-lstdc++ $targetDir/$target.d 2>&1 > /dev/null)"

          maybeErrors "$errorVal"
        fi
        ;;

      *"$grepStrPossibleERR"* )
        maybeErrors "$errorVal"
        ;;

      *"$grepStrPossibleUnhandled"* )
        maybeErrors "$errorVal"
        ;;

      *"$grepStrPossibleErrCap"* )
        maybeErrors "$errorVal"
        ;;

      * )
        echo "$retVal"
        ;;

      esac

    # Clean the directory to get rid of the Calypso generated files
    dummy="$(/bin/rm calyp* 2>&1 > /dev/null)"
    dummy="$(/bin/rm *.o 2>&1 > /dev/null)"
    cd ..
    return
  else
    return
  fi
}

function run() {
  target=${1%/}
  RUN=""

  if [ -f $targetDir/$target ]; then
    # If there were no errors just go on to possibly run the executable
    echo -e "\e[32mIt appears as though the executable \"$target\" was built successfully:\e[0m"
    read -p $'\e[32mDo you want to run the executable?: [Y/n] \e[0m' -n1 RUN
  else
    # Don't run executable
    echo -e "\e[31mIt appears as though the executable was NOT built successfully as it does not exist:\e[0m"
    echo -e "\e[31mNOT running any executable!\\n\\n\\n\e[0m"
    return
  fi

  if [ -z $RUN ]; then
    RUN="y"
  fi

  if [ $RUN = "n" ]; then
    echo -e "\\n\\n\\n"
    return
  else
    echo -e "\e[36mOUTPUT:\e[0m"
    echo -e "\e[34m***************************************************************************\e[0m"
    $targetDir/$target
    echo -e "\e[34m***************************************************************************\e[0m\\n\\n\\n"
  fi
}


target=${1%/}

# The --all option just lets us run through all the subdirectories to test each one in turn
allDirectories=()
if [ "$target" == "--all" ]; then
  for d in */; do
    if [ -d "$d" ]; then
      allDirectories=("${allDirectories[@]}" "$d")
    fi
  done
fi

# If one directory is named on the command line then process it or --all directories or usage message
if [ -d "$1" ]; then
  builder "$target"
  run "$target"
elif [ ! -z "$allDirectories" ]; then
  for d in "${allDirectories[@]}"; do
    target="$d"
    builder "$d"
    run "$d"
  done
else
  echo -e "\e[1;33mYou need to supply a directory name on the command line (ie. \"./BUILD bitset\"), or \"--all\" to test all subdirectories!\e[0m"
  exit 0
fi
