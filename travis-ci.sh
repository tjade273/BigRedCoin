echo -en "travis_fold:start:prepare.ci\r"

# If a fork of these scripts is specified, use that GitHub user instead
fork_user=${FORK_USER:-ocaml}

# If a branch of these scripts is specified, use that branch instead of 'master'
fork_branch=${FORK_BRANCH:-master}

export OPAMYES=1

opam init --comp=4.05.0
eval $(opam config env)

echo OCAML_VERSION=4.05 > .travis-ocaml.env
echo OPAM_SWITCH=5.05.0 >> .travis-ocaml.env

ocaml -version
opam --version
opam --git-version

opam depext conf-m4
opam pin add bigredcoin .

echo -en "travis_fold:end:prepare.ci\r"

