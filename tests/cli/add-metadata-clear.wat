;; RUN: metadata add % --name foo --language bar=1 --processed-by baz=1 --sdk my-sdk=2 --version 1.2.3 | metadata add --clear-version --clear-name --description sample-description | metadata show
(module)
