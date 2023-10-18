(defparameter *csp-header*  "frame-ancestors 'self'; form-action 'self'")
;;;;;; Define OpenID Connect configuration ;;;;;;

(defvar *tenant-name* "TENANT-NAME")
(defvar *user-flow* "USER-FLOW")
(defvar *openid-config* nil)
(defvar *jwt* nil)
(defvar *client-id* "CLIENT-ID")
(defvar *client-secret* "CLIENT-SECRET")

;;;;;; Login page handler ;;;;;;

(hunchentoot:define-easy-handler (cl-openid-connect:login :uri "/login") ()
  (setf (hunchentoot:content-type*) "text/html")
  (setf (hunchentoot:header-out "Content-Security-Policy") *csp-header*)
  (setf (hunchentoot:header-out "X-Frame-Options") "DENY")
  (setf (hunchentoot:header-out "X-Content-Type-Options") "nosniff")
  (cl-openid-connect:set-openid-config)  
  
  (let ((auth-url (cl-openid-connect:construct-auth-endpoint-url
                   :endpoint (cl-openid-connect:authorization-endpoint *openid-config*)
                   :client-id *client-id*
                   :response-type "code+id_token"
                   :redirect-uri "http://localhost:8000/getAToken"
                   :response-mode "form_post"
                   :scope "https://{TENANT}.onmicrosoft.com/api/user_impersonation+openid+profile+offline_access")))
    (hunchentoot:redirect auth-url)))

;;;;;; Token retrieval page handler ;;;;;;

(hunchentoot:define-easy-handler (cl-openid-connect:callback :uri "/getAToken") ()
  (let* ((code (hunchentoot:parameter "code"))
         (id_token (hunchentoot:parameter "id_token"))
         (state (hunchentoot:parameter "state"))
         (token (hunchentoot:parameter "token"))
         (erro (hunchentoot:parameter "error"))
         (error_description (hunchentoot:parameter "error_description")))

    ;; Handle errors here, if any

    (setf (hunchentoot:session-value 'code) code)
    (setf (hunchentoot:session-value 'id_token) id_token)
    (setf (hunchentoot:session-value 'token) token)
    (setf (hunchentoot:session-value 'state) state)
    (setf (hunchentoot:session-value 'erro) erro)
    (setf (hunchentoot:session-value 'error_description) error_description)

    (if (eql (cl-openid-connect:auth-token (hunchentoot:session-value 'id_token) *openid-config*) 'id_token)
        (hunchentoot:redirect "/logout")
        (hunchentoot:redirect "/getApiToken"))))

;;;;;; API token retrieval handler ;;;;;;

(hunchentoot:define-easy-handler (cl-openid-connect:get-api-token :uri "/getApiToken") ()
  (let* ((api-url (cl-openid-connect:make-api-url "https://{TENANT}.onmicrosoft.com/webapivehiculos/user_impersonation+offline_access"))
         (response
           (handler-case (cl-openid-connect:get-api-token-access api-url (hunchentoot:session-value 'code) *client-secret*)
             (dex:http-request-bad-request ()
               ;; Handle 400 Bad Request here
               (hunchentoot:redirect "/error"))
             (dex:http-request-failed (e)
               ;; Handle other 4xx or 5xx errors here
               (hunchentoot:redirect "/error")
               (format *error-output* "The server returned ~D" (dex:response-status e))))))
    (setf (hunchentoot:session-value 'refresh_token)
          (gethash "refresh_token" response))
    (setf (hunchentoot:session-value 'access_token)
          (gethash "access_token" response))
    (hunchentoot:redirect "/callApi")))

;;;;;; API call handler ;;;;;;

(hunchentoot:define-easy-handler (cl-openid-connect:call-api :uri "/callApi") ()
  (cl-openid-connect:get-call-api-data
   "YOUR-API-CALL"
    (hunchentoot:session-value 'access_token)))

;;;;;; Logout handler ;;;;;;

(hunchentoot:define-easy-handler (cl-openid-connect:logout :uri "/logout") ()
  (hunchentoot:redirect "https://mtkb2c.b2clogin.com/mtkb2c.onmicrosoft.com/b2c_1_login/oauth2/v2.0/logout?post_logout_redirect_uri=http://localhost:8000/"))

;;;;;; Root handler ;;;;;;

(hunchentoot:define-easy-handler (cl-openid-connect:root :uri "/") ()
 (format nil "Hello"))

;;;;;; Error handler ;;;;;;

(hunchentoot:define-easy-handler (cl-openid-connect:error :uri "/error") ()
(format nil "ERROR"))

;; Initialize the Hunchentoot server
(defvar *acceptor*)
(defun start-server ()
  (setq *acceptor* (make-instance 'hunchentoot:easy-acceptor :port 8000))
  (hunchentoot:start *acceptor*))
;; Start the server
(start-server)

