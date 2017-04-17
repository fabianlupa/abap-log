#!/bin/bash -e

git config --global user.name "Travis CI"
git config --global user.email "builds@travis-ci.com"
openssl aes-256-cbc -K $encrypted_56a5c505f75f_key -iv $encrypted_56a5c505f75f_iv \
        -in ./travis/ghpages.key.enc -out ghpages.key -d
chmod 600 ghpages.key
printf "%s\n" \
       "Host github.com" \
       "  IdentityFile ${TRAVIS_BUILD_DIR}/ghpages.key" \
       "  LogLevel ERROR" >> ~/.ssh/config

cd ..
git clone git@github.com:flaiker/abap-log gh-pages -b gh-pages --depth=1
cd ./gh-pages

rm -rf ./_abapdoc/*
cp -r $TRAVIS_BUILD_DIR/abapdoc/* ./_abapdoc/
find ./_abapdoc -type f ! -name '*.html' -delete

# Remove generation date footer so it does not show up as a diff
for file in $(find ./_abapdoc -name '*.html');
do
  sed -E 's/<div id="footer">Generated on [[:digit:]]{2}.[[:digit:]]{2}.[[:digit:]]{4}<\/div>//g' \
      $file > "${file}.tmp" && mv "${file}.tmp" $file
done


# Update only if there is a difference
if ! git diff-index --quiet HEAD ; then
  echo "Updating ABAP Doc on GitHub pages"

  git add -A
  git commit -m "Update ABAP Doc ($TRAVIS_BUILD_NUMBER)"
  git push -q git@github.com:flaiker/abap-log gh-pages
fi
