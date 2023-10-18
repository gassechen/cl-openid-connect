# CL-OpenID-Connect Example

This is an example of how to use the CL-OpenID-Connect library to integrate Azure B2C OpenID Connect authentication into a web application.

## Project Overview

The **CL-OpenID-Connect** project is designed to demonstrate how to use OpenID Connect for authentication with Azure B2C. This project is intended for web applications and provides handlers for login, token retrieval, API token retrieval, and logout.

## Installation and Dependencies

Before running the project, make sure to install the required dependencies using Quicklisp:

- **dexador**: HTTP client library.
- **jose**: JSON Object Signing and Encryption library.
- **hunchentoot**: Web server library.
- **yason**: JSON parser and serializer.
- **ironclad**: Cryptographic library.
- **cl-base64**: Base64 encoding and decoding library.
- **str**: String manipulation library.

## Usage

1. Start the server using `(start-server)`.

2. Access the login page at `/login`.

3. Authenticate using your Azure B2C account.

4. After successful login, you will be redirected to the API token retrieval page at `/getAToken`.

5. The application will retrieve an API token for subsequent API calls.

6. You can access the API by navigating to the `/callApi` page.

7. If you want to log out, visit `/logout`.

## Functions and Handlers

- `cl-openid-connect:login`: Handles the login page and initiates the authentication process with Azure B2C.

- `cl-openid-connect:callback`: Handles the callback after authentication, retrieves tokens, and handles errors if any.

- `cl-openid-connect:get-api-token`: Retrieves the API token using the authorization code.

- `cl-openid-connect:call-api`: Makes an authenticated API call.

- `cl-openid-connect:logout`: Logs out of the application.

- `cl-openid-connect:root`: A simple root handler for the home page.

- `cl-openid-connect:error`: Handles errors and displays an error message.

## Configuration

You should provide the following configuration parameters in the code:

- `*tenant-name*`: Your Azure B2C tenant name.

- `*user-flow*`: The user flow name.

- `*client-id*`: Your client ID.

- `*client-secret*`: Your client secret.

## Security Considerations

The example includes security features like Content Security Policy (CSP) and other headers to enhance the security of your web application. Make sure to adjust these headers according to your needs.

## Further Customization

You can further customize this example to meet the specific requirements of your web application. Ensure that you have configured your Azure B2C settings properly.

## License

This project is provided under the [MIT License](LICENSE.md).

---

Feel free to modify and extend this documentation as needed for your specific project. If you have any further questions or need additional information, please don't hesitate to ask.
