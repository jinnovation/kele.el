(when (require 'undercover nil t)
  (with-no-warnings
    (undercover "*.el"
                (:report-file "./coverage/unit/lcov-buttercup.info")
                (:report-format 'lcov)
                (:send-report nil))))
