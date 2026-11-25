;; RUN[replace]: metadata add % --authors new-authors --description new-description --licenses Apache-2.0 \
;;                           --source https://example.com/new-source --homepage https://example.com/new-homepage \
;;                           --revision new-revision --version 2.0 | print
;; RUN[clear]: metadata add % --clear-authors --clear-description --clear-licenses --clear-source \
;;                         --clear-homepage --clear-revision --clear-version | print

(module
  (@custom "authors" "old-authors")
  (@custom "description" "old-description")
  (@custom "licenses" "MIT")
  (@custom "source" "https://example.com/old-source")
  (@custom "homepage" "https://example.com/old-homepage")
  (@custom "revision" "old-revision")
  (@custom "version" "1.0")
)
