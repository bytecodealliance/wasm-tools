use bindings::service::{Error, Request, Response, Service};
use std::str;

struct Component;

impl Service for Component {
    fn execute(req: Request) -> Result<Response, Error> {
        // The content should be plain text
        let content_type = req
            .headers
            .iter()
            .find(|(k, _)| k == b"content-type")
            .map(|(_, v)| v)
            .ok_or_else(|| Error::BadRequest)?;
        if content_type != b"text/plain" {
            return Err(Error::BadRequest);
        }

        // We assume the body is UTF-8 encoded
        let body = str::from_utf8(&req.body).map_err(|_| Error::BadRequest)?;

        // Echo the body back in the response
        Ok(Response {
            headers: vec![(b"content-type".to_vec(), b"text/plain".to_vec())],
            body: format!("The request body was: {body}").into_bytes(),
        })
    }
}

bindings::export!(Component);
