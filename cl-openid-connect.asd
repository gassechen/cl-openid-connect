(asdf:defsystem :cl-openid-connect
  :version "0.1"
  :author "gassechen"
  :license "MIT"
  :description "cl-openid-connect is a Common Lisp library tailored for web applications seeking integration with Azure Active Directory B2C (Azure B2C). This manual provides a detailed guide on how to use each function within the library to implement secure user authentication and authorization for web-based projects."
  :depends-on (:dexador :jose :hunchentoot :yason :ironclad :cl-base64 :str :jose/jwt)
  :components ((:file "cl-openid-connect")))
