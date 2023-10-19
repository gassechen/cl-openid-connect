(defpackage :cl-openid-connect
    (:export :select-random-item :random-string
           :set-openid-config :auth-token
           :OpenIDConfiguration :JWT
           :user-name :nonce
           :create-openid-configuration :get-openid-configuration :get-openid-key
           :get-api-token-access :get-call-api-data))


(in-package :cl-openid-connect)


(defvar *charset* "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")


(defun select-random-item (seq)
  (elt seq (random (length seq)
                   (make-random-state t))))

(defun random-string (&optional (len 16))
  (map 'string
       (lambda (x) (select-random-item *charset*))
       (make-string len)))





(defun set-openid-config () 
  (setf *openid-config*
	(create-openid-configuration
	 (get-openid-configuration *tenant-name* *user-flow*))))

(defun auth-token (id_token openid-config)
  (setf *jwt*
	(unpack-and-check-jwt id_token (get-openid-key openid-config))))


(defclass OpenIDConfiguration ()
  ((issuer :accessor issuer)
   (authorization_endpoint :accessor authorization_endpoint)
   (token_endpoint :accessor token_endpoint)
   (end_session_endpoint :accessor end_session_endpoint)
   (jwks_uri :accessor jwks_uri)
   (response_modes_supported :accessor response_modes_supported)
   (response_types_supported :accessor response_types_supported)
   (scopes_supported :accessor scopes_supported)
   (subject_types_supported :accessor subject_types_supported)
   (id_token_signing_alg_values_supported :accessor token_signing_alg_values_supported)
   (token_endpoint_auth_methods_supported :accessor token_endpoint_auth_methods_supported)
   (claims_supported :accessor claims_supported)))


(defclass JWT ()
  ((nbf :accessor jwt-nbf)
   (c_hash :accessor jwt-c-hash)
   (tfp :accessor jwt-tfp)
   (emails :accessor jwt-emails)
   (family_name :accessor jwt-family-name)
   (extension_MicrotrackUsuarioID :accessor jwt-extension-microtrack-usuario-id)
   (given_name :accessor jwt-given-name)
   (name :accessor jwt-name)
   (auth_time :accessor jwt-auth-time)
   (iat :accessor jwt-iat)
   (nonce :accessor jwt-nonce)
   (exp :accessor jwt-exp)
   (aud :accessor jwt-aud)
   (sub :accessor jwt-sub)
   (iss :accessor jwt-iss)
   (ver :accessor jwt-ver)))

(defmethod user-name ((jwt JWT))
  (car (slot-value jwt 'emails)))

(defmethod nonce ((jwt JWT))
   (slot-value jwt 'nonce))


(defun create-openid-configuration (response)
  (let ((config (make-instance 'OpenIDConfiguration)))
    (dolist (entry response)
      (destructuring-bind (key . value) entry
        (setf (slot-value config (intern (string-upcase key))) value)))
    config))

(defun get-openid-configuration (tenant-name user-flow)
  (let* ((url (format nil "https://~a.b2clogin.com/~a.onmicrosoft.com/~a/v2.0/.well-known/openid-configuration" tenant-name tenant-name user-flow))
       (respuesta
         (yason:parse
          (dex:get url
                   :keep-alive t
                   :use-connection-pool t 
                   :connect-timeout 60       
                   :want-stream t)
          :object-as :alist)))
    respuesta))

(defun get-openid-key (openid-config)
  (let* ((url (format nil (jwks_uri openid-config)))
       (respuesta
         (yason:parse
          (dex:get url
                   :keep-alive t
                   :use-connection-pool t 
                   :connect-timeout 60       
                   )
          :object-as :hash-table)))
    respuesta))


(defun unpack-and-check-jwt (id-token public-key)
  (handler-case
    (let* ((jwt-data
             (if (jose:verify :rs256 (get-jwk-key public-key) id-token)
                 (multiple-value-bind (tokinfo keyinfo _)
                     (jose:inspect-token id-token)
                   (declare (ignore keyinfo))
                   (declare (ignore _))
                   tokinfo)
                 (error "not valid key")))
	   (config (make-instance 'JWT)))
	   (dolist (entry jwt-data)
	     (destructuring-bind (key . value) entry
	       (setf (slot-value config (intern (string-upcase key))) value)))
      config)
    (error ()
      'error)))

 
(defun construct-auth-endpoint-url (&key endpoint client-id response-type redirect-uri response-mode scope)
  (let* ((state (random-string 16))
         (nonce (random-string 16)))
    (format nil
            "~A?client_id=~A&response_type=~A&redirect_uri=~A&response_mode=~A&scope=~A&state=~A&nonce=~A"
            endpoint client-id response-type redirect-uri response-mode scope state nonce)))


(defun construct-api-endpoint-url (&key endpoint grant_type client-id response-type redirect-uri response-mode scope prompt)
  (let* ((state (random-string 16))
         (nonce (random-string 16)))
    (format nil
            "~A?grant_type=~A&client_id=~A&response_type=~A&redirect_uri=~A&response_mode=~A&scope=~A&state=~A&nonce=~A&prompt=~A"
            endpoint grant_type client-id response-type redirect-uri response-mode scope state nonce prompt)))



(defun make-api-url(scope)
  (construct-api-endpoint-url
   :endpoint (token_endpoint *openid-config*)
   :grant_type "authorization_code"
   :client-id *client-id*
   :response-type "token"
   :redirect-uri "http://localhost:8000/getAToken"
   :response-mode "form_post"
   :scope scope
   :prompt "none"))
      


(defun get-api-token-access(api-url code client-secret)
 (let ((respuesta
         (yason:parse   
	  (dex:post api-url 
		    :headers '(("x-client-sku". "MSAL.Python")
			       ("Content-Type" . "application/x-www-form-urlencoded")
			       ("x-client-ver". "1.22.0")
			       ("x-client-os" . "linux")
			       ("x-client-cpu" . "x64")
			       ("x-ms-lib-capability" . "retry-after, h429"))
		    :content `(("client_info" . "1") 
			       ("client_secret" . ,client-secret)
			       ("code" . ,code)))
	  :object-as :hash-table)))
       respuesta))


(defun get-call-api-data (url-api access-token)
  (let* ((url url-api)  
         (headers `(("Authorization" . ,(format nil "Bearer ~A" access-token))))
	 (yason:*parse-json-arrays-as-vectors* t)
         (yason:*parse-json-booleans-as-symbols* t)
         (response
	   (handler-case (dex:get url :headers headers)
	     (dex:http-request-failed (e)
	       (format *error-output* "The server returned ~D" (dex:response-status e))
	       (get-call-api-data url-api (hunchentoot:session-value 'refresh_token))))
	   
	     ))
     response))

