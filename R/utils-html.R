default_response_headers <- list(
  "Accept" = "application/json, text/xml, application/xml, */*",
  "Content-Type" = ""
)

#' Decode a Base64 encoded value to a string
#'
#' @description
#' Events coming via an API Gateway can have content with bodies encoded as
#' Base64. This is especially true for HTML API Gateways (as opposed to REST
#' API Gateways).
#'
#' This function propagates `NULL`s. That is, `from_base64(NULL)` returns
#' `NULL`.
#'
#' @param x a Base64 string
#'
#' @return character
#' @export
#'
#' @examples
#' from_base64("eyJudW1iZXIiOjd9")
from_base64 <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  rawToChar(jsonlite::base64_dec(x))
}

#' Decode the body of event content coming via an API Gateway
#'
#' @param body character body of an event received via an API Gateway
#'   invocation. Usually this isn't the entire content of the event, but the
#'   "body" component of it.
#' @inheritParams validate_lambda_config
#' @param base64_encoded logical that indicates if the body is encoded as Base64
#'
#' @return Either a list or, if the body is Base64 and the configuration demands
#'   that Base64 values are not decoded, a Base64 value as a character
#'
#' @keywords internal
decode_html_body <- function(body, config, base64_encoded = FALSE) {
  if (!base64_encoded) {
    parse_json_or_empty(body)
  } else if (config$decode_base64) {
    parse_json_or_empty(from_base64(body))
  } else {
    body
  }
}

#' Prepare a HTML response for a Lambda behind an API Gateway
#'
#' @description
#' Lambdas behind API Gateways need to send specially formatted responses that
#' look like this:
#'
#' ```json
#' {
#'   "statusCode": 200,
#'   "headers": {
#'     "Content-Type": "application/json"
#'   },
#'   "isBase64Encoded": false,
#'   "body": "{\"best_animal\": \"corgi\"}"
#' }
#' ```
#'
#' For basic applications where the handler function is returning a simple
#' result, `lambdr` will do its best to automatically return a result compatible
#' with API Gateways. It will do this whenever an event is detected as having
#' come via an API Gateway. For most purposes this is sufficient, and allows
#' users to focus on the handler function rather than the specifics of how
#' _AWS Lambda_ works.
#'
#' For more complicated applications, such as when the Lambda needs to return a
#' specific content type or specific headers, may require a bespoke response.
#' This function will take any R object and format it in style of the above
#' example, allowing for customisation.
#'
#' When the handler function returns a `html_response` the formatted result will
#' be returned to the API Gateway without further serialisation.
#'
#' @param body the actual result to be delivered. This is not serialised in any
#'   way, so if this is a list to be interpreted JSON it should be
#'   stringified, that is, it should be a string of a JSON. Consider using the
#'   \code{\link{as_stringified_json}} function.
#' @param is_base64 logical which indicates if `body` is Base64 encoded.
#'   Defaults to False.
#' @param status_code integer status code of the response. Defaults to `200L`
#'   (OK).
#' @param content_type MIME type for the content. This will be appended to the
#'   headers (as "Content-Type"), unless such a value is already provided to
#'   `headers`, in which case this argument is ignored. If not provided then no
#'   information on headers will be sent in the response, leaving the beahviour
#'   up to the defaults of the API Gateway.
#' @param headers additional headers, as a named list, to be included in the
#'   response. If this contains a "Content-Type" value then `content_type` is
#'   ignored.
#'
#' @return A stringified JSON response for an API Gateway, with the
#'   "already_serialised" attribute marked as `TRUE`. This will stop
#'   `serialise_result` from attempting to serialise the result again.
#'
#' @export
#'
#' @examples
#' html_response("abc")
#' html_response("YWJj", is_base64 = TRUE)
#' html_response("abc", headers = list(x = "a"))
#' html_response(
#'   "<html><body>Hello World!</body></html>",
#'   content_type = "text/html"
#' )
html_response <- function(
    body,
    is_base64 = FALSE,
    status_code = 200L,
    content_type = NULL,
    headers = list()
) {
  response_list <- list(
    body = body,
    isBase64Encoded = is_base64,
    statusCode = status_code
  )

  if (length(headers) != 0 || !is.null(content_type)) {
    if (!("Content-Type" %in% names(headers))) {
      headers[["Content-Type"]] = content_type
    }
    response_list[["headers"]] = headers
  }

  mark_as_already_serialised(as_stringified_json(response_list))
}
