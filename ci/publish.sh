#!/bin/bash

usage() {
	cat <<EOF
$0 [OPTIONS] [-e user.email] [-u user.name] [-d GITDIR [-n]] PACKAGEFILE [PACKAGEFILES...]
  -v : Be more verbose
  -c : Continue even if a package update fails
  -e : Set git config option user.email
  -u : Set git config option user.name
  -d : Use GITDIR for the clone of olanilsson.bitbucket.org instead of a tempdir
  -n : Use an existing clone of olanilsson.bitbucket.org at GITDIR instead of cloning
  -h : Print this help message
EOF
	[ "$1" ] && exit $1
}

while getopts "u:e:d:nhvc" opt ; do
	case $opt in
		c) continue=1       ;;
		e) usermail=$OPTARG ;;
		d) clonedir=$OPTARG ;;
		n) noclone=1        ;;
		u) username=$OPTARG	;;
		v) verbose=1        ;;
		h) usage 0 ;;
		*) usage 1 ;;
	esac
done
shift $((OPTIND-1))

cleanup() {
	rm -f deployout commitmsg
	[ "$noclone" ] || rm -rf $clonedir
}
trap cleanup EXIT

set -e
[ ! "$verbose" ] || set -x

[ ! "$noclone" ] || [ "$clonedir" ] || {
	echo "-n requires -d"
	exit 1
}

: ${clonedir:=$(mktemp -d -p .)}
[ "$noclone" ]    || git clone git@bitbucket.org:olanilsson/olanilsson.bitbucket.org.git $clonedir
[ ! "$username" ] || git -C ${clonedir}/packages config user.name "$username"
[ ! "$usermail" ] || git -C ${clonedir}/packages config user.email "$usermail"

dest=${clonedir}/packages
git -C $dest config push.default simple
for pack in $@; do
	if ${EMACS:-emacs} -Q --batch --script $dest/deploy.el $dest $pack 2>deployout; then
		>&2 cat deployout
		ls $dest/${pack%.*}-*.${pack##*.} | sed 's!.*/\(.*\)-\(.*\)\.el!Update \1 to \2!' > commitmsg
		git -C $dest add --all
		cat $(pwd)/commitmsg | (cd $dest && git commit -F - )
	else
		errcode=$?
		>&2 cat deployout
		grep -q "New package has smaller version:" deployout && [ "$continue" ] || exit $errcode
	fi
done
git -C $dest push origin
