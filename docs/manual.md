
# cl-openid-connect - User Manual

## Introduction

**cl-openid-connect** is a Common Lisp library tailored for web applications seeking integration with Azure Active Directory B2C (Azure B2C). This manual provides a detailed guide on how to use each function within the library to implement secure user authentication and authorization for web-based projects.

## Table of Contents

1. [Setting Up Azure B2C Configuration](#setting-up-azure-b2c-configuration)
2. [Authentication and Token Handling](#authentication-and-token-handling)
3. [Customizing Authentication Flow](#customizing-authentication-flow)
4. [Accessing User Data](#accessing-user-data)
5. [Making API Calls](#making-api-calls)

## 1. Setting Up Azure B2C Configuration

- **Function:** `set-openid-config`

  This function initializes the OpenID configuration for Azure B2C. It sets up the necessary endpoints and settings for authentication.

  ```common-lisp
  (set-openid-config)
  ```

## 2. Authentication and Token Handling

- **Function:** `auth-token`

  Use this function to handle user authentication and token validation. It unpacks and checks the JWT (JSON Web Token) provided by Azure B2C.

  ```common-lisp
  (auth-token id_token openid-config)
  ```

## 3. Customizing Authentication Flow

- **Function:** `construct-auth-endpoint-url`

  Customize the authentication flow by constructing the authentication endpoint URL. You can specify parameters such as the client ID, response type, and more.

  ```common-lisp
  (construct-auth-endpoint-url
    :endpoint "https://your-authorization-endpoint"
    :client-id "your-client-id"
    :response-type "code"
    :redirect-uri "your-redirect-uri"
    :response-mode "form_post"
    :scope "openid"
  )
  ```

## 4. Accessing User Data

- **Function:** `user-name`

  Retrieve the user's name from the JWT token obtained during authentication.

  ```common-lisp
  (user-name jwt)
  ```

- **Function:** `nonce`

  Get the nonce value from the JWT token for additional security.

  ```common-lisp
  (nonce jwt)
  ```

## 5. Making API Calls

- **Function:** `get-call-api-data`

  Use this function to make authenticated API calls to Azure B2C. It takes the API URL and the user's access token as parameters.

  ```common-lisp
  (get-call-api-data "https://your-api-endpoint" access-token)
  ```

---

This user manual provides an overview of the functions and their usage within the **cl-openid-connect** library. These functions enable web applications to implement secure authentication and authorization through Azure B2C effortlessly.

For further details and advanced use cases, refer to the official documentation and examples provided with the library.

---

Feel free to customize this manual to include specific examples, code snippets, and additional information based on your project's requirements.
