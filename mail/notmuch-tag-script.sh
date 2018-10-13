#!/bin/sh
PATH=$PATH:/run/current-system/sw/bin/:/etc/profiles/per-user/jassob/bin/:/home/jassob/.nix-profile/bin/

echo "Synchronizing inbox"
mbsync -Va

echo "Indexing new mail"
notmuch new

echo "Tagging messages from Medium Daily Digest as 'to-read' and archiving them"
notmuch tag +to-read -inbox -unread is:unread and from:"Medium Daily Digest"

echo "Tagging messages from Chalmers Sångkör as 'choir' and archiving them"
notmuch tag +choir -unread is:unread and to:"mk@choir.chs.chalmers.se"

echo "Tagging messages from Chalmersspexet Bob as 'spex'"
notmuch tag +spex is:unread and to:bob.chalmersspexet.se

echo "Tagging Utskick as important"
notmuch tag +flagged +spex is:unread and subject:"Utskick"

echo "Tagging receipts as 'receipts' and archiving them"
notmuch tag +receipt -inbox -unread is:unread from:"info@vasttrafik.se" and subject:"Betalningsbekräftelse"
notmuch tag +receipt -inbox -unread is:unread from:"service@paypal.se" and subject:"Kvitto för betalning"
