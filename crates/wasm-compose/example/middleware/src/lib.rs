use bindings::{
    backend,
    service::{Error, Request, Response, Service},
};
use flate2::{write::GzEncoder, Compression};
use std::io::Write;

struct Component;

impl Service for Component {
    fn execute(req: Request) -> Result<Response, Error> {
        let headers: Vec<_> = req
            .headers
            .iter()
            .map(|(k, v)| (k.as_slice(), v.as_slice()))
            .collect();

        // Send the request to the backend
        let mut response = backend::execute(backend::Request {
            headers: &headers,
            body: &req.body,
        })
        .map(|r| Response {
            headers: r.headers,
            body: r.body,
        })
        .map_err(|e| match e {
            backend::Error::BadRequest => Error::BadRequest,
        })?;

        // If the response is already encoded, leave it alone
        if response
            .headers
            .iter()
            .any(|(k, _)| k == b"content-encoding")
        {
            return Ok(response);
        }

        // Set the `content-encoding` header to `gzip`
        response
            .headers
            .push((b"content-encoding".to_vec(), b"gzip".to_vec()));

        // Compress the response body
        let mut encoder = GzEncoder::new(Vec::new(), Compression::default());
        encoder
            .write_all(&response.body)
            .map_err(|_| Error::BadRequest)?;
        response.body = encoder.finish().map_err(|_| Error::BadRequest)?;

        Ok(response)
    }
}

bindings::export!(Component);
